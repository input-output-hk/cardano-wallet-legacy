{- | A collection of plugins used by this edge node.
     A @Plugin@ is essentially a set of actions which will be run in
     a particular monad, at some point in time.
-}
{-# LANGUAGE TupleSections #-}

module Cardano.Wallet.Server.LegacyPlugins (
      Plugin
    , syncWalletWorker
    , acidCleanupWorker
    , conversation
    , legacyWalletBackend
    , walletDocumentation
    , resubmitterPlugin
    , notifierPlugin
    ) where

import           Universum

import           Data.Acid (AcidState)

import           Control.Exception (fromException)
import           Data.Aeson
import           Formatting (build, sformat, (%))
import           Network.HTTP.Types.Status (badRequest400)
import           Network.Wai (Application, Middleware, Response, responseLBS)
import           Network.Wai.Handler.Warp (defaultSettings,
                     setOnExceptionResponse)
import           Network.Wai.Middleware.Cors (cors, corsMethods,
                     corsRequestHeaders, simpleCorsResourcePolicy,
                     simpleMethods)
import           Ntp.Client (NtpStatus)
import           System.Wlog (logInfo, modifyLoggerName, usingLoggerName)

import qualified Network.HTTP.Types.Status as Http
import qualified Servant

import           Cardano.NodeIPC (startNodeJsIPC)
import           Cardano.Wallet.API as API
import           Cardano.Wallet.Server.CLI (NewWalletBackendParams (..),
                     RunMode, WalletBackendParams (..), isDebugMode,
                     walletAcidInterval, walletDbOptions)
import           Cardano.Wallet.WalletLayer (ActiveWalletLayer,
                     PassiveWalletLayer)
import           Cardano.Wallet.WalletLayer.Kernel (bracketActiveWallet)
import           Pos.Chain.Txp (TxpConfiguration)
import           Pos.Configuration (walletProductionApi,
                     walletTxCreationDisabled)
import           Pos.Context (HasNodeContext)
import           Pos.Crypto (ProtocolMagic)
import           Pos.Infra.Diffusion.Types (Diffusion (..))
import           Pos.Infra.Shutdown (HasShutdownContext (shutdownContext),
                     ShutdownContext)
import           Pos.Infra.Shutdown.Class (HasShutdownContext (shutdownContext))
import           Pos.Launcher.Configuration (HasConfigurations)
import           Pos.Util (lensOf)
import           Pos.Util.CompileInfo (HasCompileInfo)
import           Pos.Wallet.Web (cleanupAcidStatePeriodically)
import           Pos.Wallet.Web.Mode (WalletWebMode)
import           Pos.Wallet.Web.Pending.Worker (startPendingTxsResubmitter)
import           Pos.Wallet.Web.Server.Launcher (walletDocumentationImpl,
                     walletServeImpl)
import           Pos.Wallet.Web.Sockets (getWalletWebSockets,
                     upgradeApplicationWS)
import           Pos.Wallet.Web.State (askWalletDB)
import           Pos.Wallet.Web.Tracking.Sync (processSyncRequest)
import           Pos.Wallet.Web.Tracking.Types (SyncQueue)
import           Pos.Web (serveWeb)
import           Pos.WorkMode (WorkMode)

import qualified Cardano.Wallet.API.V1.Errors as V1
import qualified Cardano.Wallet.LegacyServer as LegacyServer
import qualified Cardano.Wallet.Server as Server
import qualified Pos.Wallet.Web.Server.Runner as V0


-- A @Plugin@ running in the monad @m@.
type Plugin m = [Diffusion m -> m ()]

-- | A @Plugin@ to periodically compact & snapshot the acid-state database.
acidCleanupWorker :: WalletBackendParams
                  -> Plugin WalletWebMode
acidCleanupWorker WalletBackendParams{..} = pure $ const $
    modifyLoggerName (const "acidcleanup") $
    askWalletDB >>= \db -> cleanupAcidStatePeriodically db (walletAcidInterval walletDbOptions)

-- | The @Plugin@ which defines part of the conversation protocol for this node.
conversation :: HasConfigurations => WalletBackendParams -> Plugin WalletWebMode
conversation wArgs = map const (pluginsMonitoringApi wArgs)
  where
    pluginsMonitoringApi :: (WorkMode ctx m , HasNodeContext ctx)
                         => WalletBackendParams
                         -> [m ()]
    pluginsMonitoringApi WalletBackendParams {..}
        | enableMonitoringApi = [serveWeb monitoringApiPort walletTLSParams]
        | otherwise = []

walletDocumentation
    :: (HasConfigurations, HasCompileInfo)
    => WalletBackendParams
    -> Plugin WalletWebMode
walletDocumentation WalletBackendParams {..} = pure $ \_ ->
    walletDocumentationImpl
        application
        walletDocAddress
        tls
        (Just defaultSettings)
        Nothing
  where
    application :: WalletWebMode Application
    application = do
        let app = Servant.serve API.walletDocAPI LegacyServer.walletDocServer
        withMiddleware walletRunMode app
    tls =
        if isDebugMode walletRunMode then Nothing else walletTLSParams

-- | A @Plugin@ to start the wallet backend API.
legacyWalletBackend :: (HasConfigurations, HasCompileInfo)
                    => Core.Config
                    -> TxpConfiguration
                    -> WalletBackendParams
                    -> TVar NtpStatus
                    -> Plugin WalletWebMode
legacyWalletBackend coreConfig txpConfig WalletBackendParams {..} ntpStatus = pure $ \diffusion -> do
    modifyLoggerName (const "legacyServantBackend") $ do
      logInfo $ sformat ("Production mode for API: "%build)
        walletProductionApi
      logInfo $ sformat ("Transaction submission disabled: "%build)
        walletTxCreationDisabled

      ctx <- view shutdownContext
      let
        portCallback :: Word16 -> IO ()
        portCallback port = usingLoggerName "NodeIPC" $ flip runReaderT ctx $ startNodeJsIPC port
      walletServeImpl
        (getApplication diffusion)
        walletAddress
        -- Disable TLS if in debug mode.
        (if isDebugMode walletRunMode then Nothing else walletTLSParams)
        (Just $ setOnExceptionResponse exceptionHandler defaultSettings)
        (Just portCallback)
  where
    -- Gets the Wai `Application` to run.
    getApplication :: Diffusion WalletWebMode -> WalletWebMode Application
    getApplication diffusion = do
        logInfo "Wallet Web API has STARTED!"
        wsConn <- getWalletWebSockets
        ctx <- V0.walletWebModeContext
        withMiddleware walletRunMode
            $ upgradeApplicationWS wsConn
            $ Servant.serve API.walletAPI
            $ LegacyServer.walletServer
                (V0.convertHandler ctx)
                coreConfig
                txpConfig
                diffusion
                ntpStatus
                walletRunMode

    exceptionHandler :: SomeException -> Response
    exceptionHandler se =
        case asum [handleV1Errors se, handleV0Errors se] of
             Nothing -> handleGenericError se
             Just r  -> r

    -- Handles domain-specific errors coming from the V1 API.
    handleV1Errors :: SomeException -> Maybe Response
    handleV1Errors se =
        let reify (we :: V1.WalletError) =
                responseLBS (V1.toHttpErrorStatus we) [applicationJson] .  encode $ we
        in fmap reify (fromException se)

    -- Handles domain-specific errors coming from the V0 API, but rewraps it
    -- into a jsend payload. It doesn't explicitly handle 'InternalError' or
    -- 'DecodeError', as they can come from any part of the stack or even
    -- rewrap some other exceptions (cfr 'rewrapToWalletError').
    -- Uses the 'Buildable' istance on 'WalletError' to exploit any
    -- available rendering and information-masking improvements.
    handleV0Errors :: SomeException -> Maybe Response
    handleV0Errors se =
        let maskSensitive err =
                case err of
                    V0.RequestError _         -> err
                    V0.InternalError _        -> V0.RequestError "InternalError"
                    V0.DecodeError _          -> V0.RequestError "DecodeError"
                    V0.DuplicateWalletError _ -> V0.RequestError "DuplicateWalletError"
                    V0.NoSuchWalletError _    -> V0.RequestError "NoSuchWalletError"
            reify :: V0.WalletError -> V1.WalletError
            reify = V1.UnknownError . sformat build . maskSensitive
        in fmap (responseLBS badRequest400 [applicationJson] .  encode . reify) (fromException se)

    -- Handles any generic error, trying to prevent internal exceptions from leak outside.
    handleGenericError :: SomeException -> Response
    handleGenericError _ =
        let
            unknownV1Error = V1.UnknownError "Something went wrong."
        in
            responseLBS badRequest400 [applicationJson] $ encode unknownV1Error


-- | A @Plugin@ to resubmit pending transactions.
resubmitterPlugin :: HasConfigurations
                  => Core.Config
                  -> TxpConfiguration
                  -> Plugin WalletWebMode
resubmitterPlugin coreConfig txpConfig = [\diffusion -> askWalletDB >>= \db ->
                        startPendingTxsResubmitter coreConfig txpConfig db (sendTx diffusion)]

-- | A @Plugin@ to notify frontend via websockets.
notifierPlugin :: HasConfigurations => Plugin WalletWebMode
notifierPlugin = [const V0.notifierPlugin]

-- | The @Plugin@ responsible for the restoration & syncing of a wallet.
syncWalletWorker :: HasConfigurations => Core.Config -> Plugin WalletWebMode
syncWalletWorker coreConfig = pure $ const $
    modifyLoggerName (const "syncWalletWorker") $
    (view (lensOf @SyncQueue) >>= processSyncRequest coreConfig)

corsMiddleware :: Middleware
corsMiddleware = cors (const $ Just policy)
    where
      policy = simpleCorsResourcePolicy
        { corsRequestHeaders = ["Content-Type"]
        , corsMethods = "PUT" : simpleMethods
        }


-- | "Attaches" the middleware to this 'Application', if any.
-- When running in debug mode, chances are we want to at least allow CORS to test the API
-- with a Swagger editor, locally.
withMiddleware
    :: MonadIO m
    => RunMode
    -> Application
    -> m Application
withMiddleware wrm app = do
    st <- liftIO $ Throttle.initThrottler
    pure
        . (if isDebugMode wrm then corsMiddleware else identity)
        . Throttle.throttle throttleSettings st
        $ app
  where
    throttleSettings = Throttle.defaultThrottleSettings
        { Throttle.onThrottled = \microsTilRetry ->
            responseLBS
                (V1.toHttpErrorStatus we)
                [applicationJson]
                (encode (V1.RequestThrottled microsTilRetry))
        , Throttle.throttleRate = 90
        }
