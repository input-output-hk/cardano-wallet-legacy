{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}

-- | Deal with pending transactions
module Cardano.Wallet.Kernel.Pending (
    newPending
  , newForeign
  , cancelPending
  , NewPendingError
  , PartialTxMeta
  ) where

import           Universum hiding (State)

import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map

import           Control.Concurrent.MVar (modifyMVar_)

import           Data.Acid.Advanced (update')

import           Pos.Chain.Txp (Tx (..), TxAux (..), TxOut (..))
import           Pos.Core (Address, Coin (..))
import           Pos.Crypto (EncryptedSecretKey)

import           Cardano.Wallet.Kernel.AddressPool (AddressPool)
import           Cardano.Wallet.Kernel.DB.AcidState (DB, CancelPending (..),
                     NewForeign (..), NewForeignError (..), NewPending (..),
                     NewPendingError (..))
import           Cardano.Wallet.Kernel.DB.HdRootId (HdRootId)
import           Cardano.Wallet.Kernel.DB.HdWallet
import           Cardano.Wallet.Kernel.DB.InDb
import qualified Cardano.Wallet.Kernel.DB.Spec.Pending as Pending
import           Cardano.Wallet.Kernel.DB.TxMeta (TxMeta, putTxMeta)
import           Cardano.Wallet.Kernel.Internal
import           Cardano.Wallet.Kernel.Read (getHdRndWallets, getHdSeqWallets, getWalletSnapshot)
import           Cardano.Wallet.Kernel.Submission (Cancelled, addPending)
import           Cardano.Wallet.Kernel.Util.Core

{-------------------------------------------------------------------------------
  Submit pending transactions
-------------------------------------------------------------------------------}

-- | When we create a new Transaction, we don`t yet know which outputs belong to us
-- (it may not be just the change addresses change we create, but also addresses the user specifies).
-- This check happenes in @newTx@. Until then we move around this partial TxMetadata.
-- @Bool@ indicates if all outputs are ours and @Coin@ the sum of the coin of our outputs.
type PartialTxMeta = Bool -> Coin -> TxMeta

-- | Submit a new pending transaction
--
-- If the pending transaction is successfully added to the wallet state, the
-- submission layer is notified accordingly.
--
-- NOTE: we select "our" output addresses from the transaction and pass it along to the data layer
newPending :: ActiveWallet
           -> HdAccountId
           -> TxAux
           -> PartialTxMeta
           -> IO (Either NewPendingError TxMeta)
newPending w accountId tx partialMeta = do
    newTx w accountId tx partialMeta $ \ourAddrs ->
        update' ((walletPassive w) ^. wallets) $ NewPending accountId (InDb tx) ourAddrs

-- | Submit new foreign transaction
--
-- A foreign transaction is a transaction that transfers funds from /another/
-- wallet to this one.
newForeign :: ActiveWallet
           -> HdAccountId
           -> TxAux
           -> TxMeta
           -> IO (Either NewForeignError ())
newForeign w accountId tx meta = do
    map void <$> newTx w accountId tx (\_ _ ->  meta) $ \ourAddrs ->
        update' ((walletPassive w) ^. wallets) $ NewForeign accountId (InDb tx) ourAddrs

-- | Submit a new transaction
--
-- Will fail if the HdAccountId does not exist or if some inputs of the
-- new transaction are not available for spending.
--
-- If the transaction is successfully added to the wallet state, transaction metadata
-- is persisted and the submission layer is notified accordingly.
--
-- NOTE: we select "our" output addresses from the transaction and pass it along to the data layer
newTx :: forall e. ActiveWallet
      -> HdAccountId
      -> TxAux
      -> PartialTxMeta
      -> ([HdAddress] -> IO (Either e ())) -- ^ the update to run, takes ourAddrs as arg
      -> IO (Either e TxMeta)
newTx ActiveWallet{..} accountId tx partialMeta upd = do
    snapshot <- getWalletSnapshot walletPassive

    -- | NOTE: we recognise addresses in the transaction outputs that belong
    -- to _all_ wallets (not only for the account and wallet to which this
    -- transaction is being submitted)
    (hdRnds, hdSeqs) <- fullPref snapshot
    let allOurAddresses = fst <$> allOurs hdRnds <> allOurs hdSeqs

    -- run the update
    upd allOurAddresses >>= \case
        Left e   -> return (Left e)
        Right () -> do
            let ourOutputCoins
                    = snd <$> allOurs (thisPrefRnd hdRnds) <> allOurs (thisPrefSeq hdSeqs)
                -- | NOTE: above, we prefilter coins that belong to _all_ accounts
                -- which share the root of the given accountId.
                -- Since we don't know the type of this wallet, we prefilter
                -- it as either type (one of which will be trivial)
                gainedOutputCoins = sumCoinsUnsafe ourOutputCoins
                allOutsOurs = length ourOutputCoins == length txOut
                txMeta = partialMeta allOutsOurs gainedOutputCoins
            putTxMeta (walletPassive ^. walletMeta) txMeta
            submitTx
            return (Right txMeta)
    where
        (txOut :: [TxOut]) = NE.toList $ (_txOutputs . taTx $ tx)

        -- Prefiltering context for _all_ wallets
        fullPref
            :: DB
            -> IO ( Map HdRootId EncryptedSecretKey
                  , Map HdAccountId (AddressPool Address))
        fullPref db = do
            rnds <- getHdRndWallets db
                        (walletPassive ^. walletKeystore)
                        (walletPassive ^. walletProtocolMagic)
                        (walletPassive ^. walletLogMessage)
            seqs <- getHdSeqWallets db
            return (rnds, seqs)

        -- Provides the prefiltering context for all accounts in a wallet.
        --
        -- If the accountId is part of an HdRnd wallet, then this selects
        -- the HdRootId of that wallet.
        thisPrefRnd
            :: Map HdRootId EncryptedSecretKey
            -> Map HdRootId EncryptedSecretKey
        thisPrefRnd
            = Map.filterWithKey (\r _ -> accountId ^. hdAccountIdParent == r)

        -- Provides the prefiltering context for all accounts in a wallet.
        --
        -- If the accountId is part of an HdSeq wallet, then this selects
        -- all HdAccountId's that have the same root of the given accountId.
        thisPrefSeq
            :: Map HdAccountId (AddressPool Address)
            -> Map HdAccountId (AddressPool Address)
        thisPrefSeq
            = Map.filterWithKey (\a _ -> accountId ^. hdAccountIdParent == a ^. hdAccountIdParent)

        -- filter the Tx outputs for "ours" in the prefilter context _s_
        allOurs
            :: IsOurs s
            => s
            -> [(HdAddress, Coin)]
        allOurs (isOursSkip -> True) = []
        allOurs s = flip evalState s $ fmap catMaybes $ forM txOut $ \out -> do
            fmap (, txOutValue out) <$> state (isOurs $ txOutAddress out)

        submitTx :: IO ()
        submitTx = modifyMVar_ (walletPassive ^. walletSubmission) $
                    return . addPending accountId (Pending.singleton tx)

-- | Cancel a pending transaction
--
-- NOTE: This gets called in response to events /from/ the wallet submission
-- layer, so we shouldn't be notifying the submission in return here.
--
-- This removes the transaction from either pending or foreign.
cancelPending :: PassiveWallet -> Cancelled -> IO ()
cancelPending passiveWallet cancelled =
    update' (passiveWallet ^. wallets) $ CancelPending (fmap InDb cancelled)
