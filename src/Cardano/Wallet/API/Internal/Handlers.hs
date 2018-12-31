module Cardano.Wallet.API.Internal.Handlers (handlers) where

import           Universum

import           Network.HTTP.Types (Status (..))
import           Servant ((:<|>) (..), Handler, NoContent (..), ServerT, err404,
                     err500, throwError)
import           Servant.Client (GenResponse (..), ServantError (..))

import           Cardano.Node.Client (ClientError (..), NodeHttpClient)
import qualified Cardano.Node.Client as NodeClient
import qualified Pos.Node.API as NodeClient


import qualified Cardano.Wallet.API.Internal as Internal
import           Cardano.Wallet.API.Response (APIResponse, single)
import           Cardano.Wallet.API.V1.Types (Wallet, WalletImport,
                     WalletSoftwareVersion (..))
import           Cardano.Wallet.WalletLayer (PassiveWalletLayer)
import qualified Cardano.Wallet.WalletLayer as WalletLayer

handlers
    :: NodeHttpClient
    -> PassiveWalletLayer IO
    -> ServerT Internal.API Handler
handlers nc w =
    nextUpdate nc
    :<|> applyUpdate nc
    :<|> postponeUpdate
    :<|> resetWalletState w
    :<|> importWallet w

nextUpdate :: NodeHttpClient -> Handler (APIResponse WalletSoftwareVersion)
nextUpdate nc = do
    emUpd <- liftIO . runExceptT $ NodeClient.getNextUpdate nc
    case emUpd of
        Left err  ->
            handleNodeError err
        Right (NodeClient.V1 upd) ->
            single <$> pure (WalletSoftwareVersion upd)

applyUpdate :: NodeHttpClient -> Handler NoContent
applyUpdate nc = do
    enc <- liftIO . runExceptT $ NodeClient.restartNode nc
    case enc of
        Left err ->
            handleNodeError err
        Right () ->
            pure NoContent

handleNodeError :: ClientError () -> Handler a
handleNodeError err =
    case err of
        KnownError _ ->
            throwError err500
        ErrFromServant servantError ->
            case servantError of
                FailureResponse (Response (Status statusCode _) _ _ _)
                    | statusCode == 404 ->
                        throwError err404
                    | otherwise ->
                        throwM servantError
                _ ->
                    throwM servantError

-- | This endpoint has been made into a no-op.
postponeUpdate :: Handler NoContent
postponeUpdate = pure NoContent

resetWalletState :: PassiveWalletLayer IO -> Handler NoContent
resetWalletState w = liftIO (WalletLayer.resetWalletState w) >> return NoContent

-- | Imports a 'Wallet' from a backup.
importWallet :: PassiveWalletLayer IO -> WalletImport -> Handler (APIResponse Wallet)
importWallet w walletImport = do
    res <- liftIO $ WalletLayer.importWallet w walletImport
    case res of
         Left e               -> throwM e
         Right importedWallet -> pure $ single importedWallet
