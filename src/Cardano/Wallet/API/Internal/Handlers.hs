module Cardano.Wallet.API.Internal.Handlers (handlers) where

import           Universum

import           Servant ((:<|>) (..), Handler, NoContent (..), ServerT)

import           Cardano.Node.Client (NodeHttpClient)
import qualified Cardano.Node.Client as NodeClient
import qualified Pos.Node.API as NodeClient

import qualified Cardano.Wallet.API.Internal as Internal
import           Cardano.Wallet.API.Response (APIResponse, single)
import           Cardano.Wallet.API.V1.Types (Wallet, WalletImport,
                     WalletSoftwareVersion (..))
import           Cardano.Wallet.NodeProxy (handleNodeError)
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

-- | This endpoint has been made into a no-op.
postponeUpdate :: Handler NoContent
postponeUpdate = pure NoContent

resetWalletState :: PassiveWalletLayer IO -> Handler NoContent
resetWalletState w =
    liftIO (WalletLayer.resetWalletState w) >> return NoContent

-- | Imports a 'Wallet' from a backup.
importWallet :: PassiveWalletLayer IO -> WalletImport -> Handler (APIResponse Wallet)
importWallet w walletImport = do
    res <- liftIO $ WalletLayer.importWallet w walletImport
    case res of
         Left e               -> throwM e
         Right importedWallet -> pure $ single importedWallet
