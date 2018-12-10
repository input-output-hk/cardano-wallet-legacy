module Test.Integration.Framework.Request
    ( HasHttpClient
    , request
    , request_
    , successfulRequest
    , ($-)
    ) where

import           Universum

import           Data.Generics.Product.Typed (HasType, typed)

import           Cardano.Wallet.Client.Http (ClientError (..), WalletClient)
import           Pos.Util.Servant (APIResponse (..))

class (HasType (WalletClient IO) ctx) => HasHttpClient ctx where
    httpClient :: Lens' ctx (WalletClient IO)
    httpClient = typed @(WalletClient IO)

instance (HasType (WalletClient IO) ctx) => HasHttpClient ctx

-- | Removes some boilerplates getting the content of a 'Resp' directly.
class Request originalResponse where
    type Response originalResponse :: *

    -- | Run a given request and transform the response
    request
        :: forall m ctx. (MonadIO m, MonadReader ctx m, HasHttpClient ctx)
        => (WalletClient IO -> IO (Either ClientError originalResponse))
        -> m (Either ClientError (Response originalResponse))

    -- | Run a given request and discard the response
    request_
        :: forall m ctx. (MonadIO m, MonadReader ctx m, HasHttpClient ctx)
        => (WalletClient IO -> IO (Either ClientError originalResponse))
        -> m ()
    request_ =
        void . request

    -- | Run a given request as above, but throws if it fails
    successfulRequest
        :: forall m ctx. (MonadIO m, MonadFail m, MonadReader ctx m, HasHttpClient ctx)
        => (WalletClient IO -> IO (Either ClientError originalResponse))
        -> m (Response originalResponse)
    successfulRequest =
        request >=> \case
            Left e  ->
                fail . ("expected a successful response but got an error: " <>) . show $ e
            Right a ->
                return a

instance Request (APIResponse a) where
    type Response (APIResponse a) = a
    request action =
        view httpClient >>= liftIO . fmap (fmap wrData) . action

instance Request () where
    type Response () = ()
    request action =
        view httpClient >>= liftIO . action

-- | Provide "next" arguments to a function, leaving the first one untouched.
--
-- e.g.
--    myFunction  :: Ctx -> Int -> String -> Result
--    myFunction' :: Ctx -> Result
--    myFunction' = myFunction $- 14 $- "patate"
infixl 1 $-
($-) :: (a -> b -> c) -> b -> a -> c
($-) = flip
