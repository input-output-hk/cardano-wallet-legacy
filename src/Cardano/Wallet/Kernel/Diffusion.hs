{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
module Cardano.Wallet.Kernel.Diffusion (
    WalletDiffusion(..)
  , fromDiffusion
  , tickDiffusionLayer
  ) where

import           Universum

import           Control.Concurrent (threadDelay)
import qualified Data.Map.Strict as Map
import           Formatting (sformat, (%))
import qualified Formatting as F

import           Pos.Chain.Block (Block, BlockHeader, HeaderHash, prevBlockL)
import           Pos.Chain.Txp (TxAux, Utxo)
import           Pos.Core ()
import           Pos.Core.Chrono (OldestFirst (..))
import           Pos.DB.Block (MonadBlockVerify)
import           Pos.Infra.Communication.Types.Protocol (NodeId)
import           Pos.Infra.Diffusion.Subscription.Status (SubscriptionStatus,
                     ssMap)
import           Pos.Infra.Diffusion.Types
import           Pos.Util.Wlog (Severity (..))


-- | Wallet diffusion layer
--
-- Limited diffusion that the active wallet needs
--
-- This has two design objectives:
--
-- * We should be able to easily instantiate this from the unit tests
--   (with some kind of mock diffusion layer)
-- * We should be able to translate @Diffusion m@ into @WalletDiffusion@ for
--   any @m@ that we can lower to @IO@ (to isolate us from the specific monad
--   stacks that are used in the full node).
--
-- Note that the latter requirement implies avoiding functionality from the full
-- diffusion layer with negative occurrences of the monad parameter.
data WalletDiffusion = WalletDiffusion {
      -- | Submit a transaction to the network
      walletSendTx                :: TxAux -> IO Bool

      -- | Get subscription status (needed for the node settings endpoint)
    , walletGetSubscriptionStatus :: IO (Map NodeId SubscriptionStatus)

      -- | Request tip-of-chain from the network
    , walletRequestTip            :: IO (Map NodeId (IO BlockHeader))

      -- | Get blocks from checkpoint up to the header (not-included) from the node
    , walletGetBlocks
        :: NodeId
        -> HeaderHash
        -> [HeaderHash]
        -> IO (OldestFirst [] Block)

    }

-- | Extract necessary functionality from the full diffusion layer
fromDiffusion :: forall m ctx .
                 (MonadBlockVerify ctx m)
              => (forall a. m a -> IO a)
              -> Diffusion m
              -> WalletDiffusion
fromDiffusion nat d = WalletDiffusion {
      walletSendTx                = nat . sendTx d
    , walletGetSubscriptionStatus = readTVarIO $ ssMap (subscriptionStates d)
    , walletRequestTip            = do
           fromDiff <- nat $ requestTip d
           pure $ Map.map nat fromDiff
    , walletGetBlocks             = \nodeId from toNotIncluding -> do
            case toNotIncluding of
                [lastConsumedHeader] ->
                    nat $ getPrevBlock nodeId from (Just lastConsumedHeader) []
                _ ->
                    nat $ getPrevBlock nodeId from Nothing []
    }
    where
        getPrevBlock
            :: NodeId
            -> HeaderHash
            -> Maybe HeaderHash
            -> [Block]
            -> m (OldestFirst [] Block)
        getPrevBlock nodeId from toMaybe blocksInThisBatch = do
            blocks <- getOldestFirst <$> getBlocks d nodeId from [from]
            case blocks of
                [block@(Right _)] -> do
                    -- here we are dealing with MainBlock
                    let prevBlockHeader = block ^. prevBlockL
                    case toMaybe of
                        Just upToHeader ->
                            if (prevBlockHeader /= upToHeader) then
                                -- still need to download backwards
                                getPrevBlock nodeId prevBlockHeader toMaybe (block : blocksInThisBatch)
                            else
                                -- we just downloaded the last missing block
                                pure $ OldestFirst $ block : blocksInThisBatch
                        Nothing -> do
                            -- just one block downloaded
                            pure $ OldestFirst (block : blocksInThisBatch)
                _ -> do
                    pure $ OldestFirst blocksInThisBatch


tickDiffusionLayer
    :: forall m. (MonadCatch m, MonadIO m)
    => (Severity -> Text -> m ())
       -- ^ A logging function
    -> ( (Severity -> Text -> m ()) -> (([HeaderHash],[HeaderHash]),Utxo) -> m (([HeaderHash],[HeaderHash]), Utxo) )
       -- ^ A function to call at each 'tick' of the worker.
       -- This callback will be responsible for doing any pre
       -- and post processing of the state.
    -> (([HeaderHash],[HeaderHash]),Utxo)
    -> m ()
tickDiffusionLayer logFunction tick initialState =
    go initialState
    `catch`
    (\(e :: SomeException) ->
            let msg = "Terminating tickDiffusionLayer due to " % F.shown
            in logFunction Error (sformat msg e)
    )
    where
      go :: (([HeaderHash],[HeaderHash]), Utxo) -> m ()
      go previousState  = do
          logFunction Debug "ticking the slot in the diffusion layer..."
          currentState <- tick logFunction previousState
          liftIO $ threadDelay tickDiffusionRate
          go currentState

tickDiffusionRate :: Int
tickDiffusionRate = 1000000
