module Cardano.Wallet.NodeProxy
    ( NodeHttpClient
    , ClientError(..)
    , handleNodeError
    ) where

import           Universum

import           Control.Monad.Except
import           Network.HTTP.Types (Status (..))
import           Servant
import           Servant.Client (GenResponse (..), ServantError (..))

import           Cardano.Node.Client (ClientError (..), NodeHttpClient)

handleNodeError
    :: (MonadThrow m, MonadError ServantErr m)
    => ClientError () -> m a
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
