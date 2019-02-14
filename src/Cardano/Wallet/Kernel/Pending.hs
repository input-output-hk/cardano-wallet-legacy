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

import           Control.Concurrent.MVar (modifyMVar_)

import           Data.Acid.Advanced (update')

import           Pos.Chain.Txp (Tx (..), TxAux (..), TxOut (..))
import           Pos.Core (Coin (..))

import           Cardano.Wallet.Kernel.DB.AcidState (CancelPending (..), DB,
                     IsNewTxError (..), NewForeign (..), NewForeignError (..),
                     NewPending (..), NewPendingError (..))
import           Cardano.Wallet.Kernel.DB.HdWallet
import           Cardano.Wallet.Kernel.DB.InDb
import           Cardano.Wallet.Kernel.DB.Read (lookupHdAccountId)
import qualified Cardano.Wallet.Kernel.DB.Spec.Pending as Pending
import           Cardano.Wallet.Kernel.DB.TxMeta (TxMeta, putTxMeta)
import           Cardano.Wallet.Kernel.Internal
import           Cardano.Wallet.Kernel.Read (getEosPools, getFOWallets,
                     getWalletSnapshot, mkEosAddress)
import           Cardano.Wallet.Kernel.Submission (Cancelled, addPending)
import           Cardano.Wallet.Kernel.Util.Core
import           Cardano.Wallet.WalletLayer.Kernel.Conv (exceptT, withExceptT)

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
-- 1) Discover all our addresses (for all wallets) in the tx outputs
-- and persist them. The address discovery mechanism is dependent on
-- the type of wallet, EO or FO. We can learn the wallet type by
-- looking at the given Account.
--
-- 2) Create and persist transaction metadata, which depends on the transaction
-- output coins in this wallet (the root is determined by the given account).
--
-- 3) Finally, the transaction is submitted to the submission layer.
--
-- Will fail if the HdAccountId does not exist or if some inputs of the
-- new transaction are not available for spending.
newTx :: IsNewTxError e
      => ActiveWallet
      -> HdAccountId
      -> TxAux
      -> PartialTxMeta
      -> ([HdAddress] -> IO (Either e ())) -- ^ the update to run, takes ourAddrs as arg
      -> IO (Either e TxMeta)
newTx ActiveWallet{..} accountId tx partialMeta upd = runExceptT $ do
    db <- liftIO $ getWalletSnapshot walletPassive

    account <- withExceptT newTxUnknownAccountErr $ exceptT $
                lookupHdAccountId db accountId

    (allAddrs, coinsForRoot) <- ExceptT $ liftIO $
                                  prefilterOurs db (account ^. hdAccountBase)
    _ <- liftIO $ upd allAddrs

    let txMeta = mkTxMeta coinsForRoot
    liftIO $ putTxMeta (walletPassive ^. walletMeta) txMeta
    liftIO $ submitTx
    return txMeta
    where
        rootId = accountId ^. hdAccountIdParent

        (txOut :: [TxOut]) = NE.toList $ (_txOutputs . taTx $ tx)

        -- Prepare tx metadata using tx output coins in this wallet
        mkTxMeta :: [Coin] -> TxMeta
        mkTxMeta coins
            = let gainedOutputCoins = sumCoinsUnsafe coins
                  allOutsOurs = length coins == length txOut
              in partialMeta allOutsOurs gainedOutputCoins

        -- Submit transaction to submission layer
        submitTx :: IO ()
        submitTx = modifyMVar_ (walletPassive ^. walletSubmission) $
                    return . addPending accountId (Pending.singleton tx)

        -- Prefilter tx outputs for the given prefilter context `s`
        allOurs
            :: IsOurs s
            => s
            -> [(HdAddress, Coin)]
        allOurs = evalState $ fmap catMaybes $ forM txOut $ \out -> do
            fmap (, txOutValue out) <$> state (isOurs $ txOutAddress out)

        -- Filter the given addresses for this root and return the coin
        coinForThisRoot :: [(HdAddress, Coin)] -> [Coin]
        coinForThisRoot ours
            = map snd $
                flip filter ours $ \(a,_) ->
                    rootId == a ^. hdAddressId . hdAddressIdParent . hdAccountIdParent

        -- Based on the wallet type of the AccountBase, prepare the appropriate
        -- prefilter context to find all our output addresses in the tx.
        -- Return our addresses alongside the coin for _this_ wallet.
        prefilterOurs
            :: MonadIO m
            => DB
            -> HdAccountBase
            -> m (Either e ([HdAddress], [Coin]))
        prefilterOurs db HdAccountBaseFO{} = do
            foRoots <- liftIO $ getFOWallets walletPassive db
            let ours = allOurs foRoots
            return . Right $ (fst <$> ours, coinForThisRoot ours)
        prefilterOurs db HdAccountBaseEO{} = do
            eoAccounts <- liftIO $ getEosPools db (mkEosAddress $ walletPassive ^. walletProtocolMagic)
            let ours = allOurs eoAccounts
            return . Right $ (fst <$> ours, coinForThisRoot ours)

-- | Cancel a pending transaction
--
-- NOTE: This gets called in response to events /from/ the wallet submission
-- layer, so we shouldn't be notifying the submission in return here.
--
-- This removes the transaction from either pending or foreign.
cancelPending :: PassiveWallet -> Cancelled -> IO ()
cancelPending passiveWallet cancelled =
    update' (passiveWallet ^. wallets) $ CancelPending (fmap InDb cancelled)
