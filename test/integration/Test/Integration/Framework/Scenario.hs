module Test.Integration.Framework.Scenario
    ( Scenario
    , runWithContext
    ) where

import           Universum

import           Test.Hspec.Core.Spec (ActionWith, Example (..), Item (..),
                     Result (..), ResultStatus (..), Spec, SpecM, mapSpecItem)


-- | A Wrapper around 'StateT' around which we define a few instances. The most
-- interesting one is 'Example'
newtype Scenario s m a = Scenario (StateT s m a)
    deriving newtype
        ( Functor
        , Applicative
        , Monad
        , MonadThrow
        , MonadCatch
        , MonadFail
        , MonadIO
        , MonadMask
        , MonadState s
        )

-- | We emulate the 'MonadReader' using the 'MonadState' instance, this way, we
-- can lower down our constraints for methods that actually just read the state.
instance (Monad m, MonadState s (Scenario s m)) => MonadReader s (Scenario s m) where
    ask = get
    local f m = do
        s <- get
        put (f s) *> m <* put s

-- | This gives us the ability to define our spec as `Scenario` instead of just
-- plain `IO`. This way, each scenario runs within a context and has access to
-- a wallet client and a dedicated faucet wallet.
instance Example (Scenario s IO ()) where
    type Arg (Scenario s IO ()) = MVar s
    evaluateExample (Scenario io) _ action _ =
        action runAndPersist >> return (Result "" Success)
      where
        runAndPersist :: MVar s -> IO ()
        runAndPersist mvar = do
            s <- takeMVar mvar
            (_, s') <- runStateT io s
            putMVar mvar s'

-- | Execute all scenarios in sequence providing the given context 'ctx'
runWithContext :: forall ctx. ctx -> SpecM ctx () -> Spec
runWithContext ctx = mapSpecItem mapActionWith mapItem
  where
    mapActionWith :: ActionWith ctx -> ActionWith ()
    mapActionWith fn = \() -> fn ctx

    mapItem :: Item ctx -> Item ()
    mapItem item = item
        { itemExample =
            \p action -> (itemExample item) p (action . mapActionWith)
        }
