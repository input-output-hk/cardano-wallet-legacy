module Cardano.Wallet.API.V1.Handlers.Settings (handlers) where

import           Universum

import           Servant

import qualified Cardano.Node.Client as NodeClient
import           Cardano.Wallet.API.Response (APIResponse, single)
import           Cardano.Wallet.API.V1.Types (NodeSettings)
import           Cardano.Wallet.NodeProxy (NodeHttpClient, handleNodeError)
import qualified Pos.Node.API as Node

handlers
    :: NodeHttpClient
    -> ServerT Node.SettingsAPI Handler
handlers = getNodeSettings

-- | Retrieve the static settings for this node
getNodeSettings
    :: NodeHttpClient
    -> Handler (APIResponse NodeSettings)
getNodeSettings nc = do
    emUpd <- liftIO . runExceptT $ NodeClient.getNodeSettings nc
    case emUpd of
        Left err  ->
            handleNodeError err
        Right settings ->
            single <$> pure settings
