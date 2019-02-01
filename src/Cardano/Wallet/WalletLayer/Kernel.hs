{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module Cardano.Wallet.WalletLayer.Kernel
    ( bracketPassiveWallet
    , bracketActiveWallet
    ) where

import           Universum hiding (for_)

import           Control.Concurrent.Async (async, cancel)
import qualified Control.Concurrent.STM as STM
import           Control.Monad.IO.Unlift (MonadUnliftIO)
import           Data.Foldable (for_)
import qualified Data.List as List
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Strict.Maybe as StrictMaybe
import           Formatting ((%))
import qualified Formatting as F

import           Pos.Chain.Block (Blund, HeaderHash, blockHeader,
                     getBlockHeader, headerHash, mainBlockSlot,
                     mainBlockTxPayload, prevBlockL)
import           Pos.Chain.Genesis (Config (..))
import           Pos.Chain.Txp (Tx (..), TxIn (..), Utxo, txpTxs, utxoToLookup)
import           Pos.Core (getCurrentTimestamp)
import           Pos.Core.Chrono (OldestFirst (..))
import           Pos.Crypto (ProtocolMagic)
import           Pos.Infra.InjectFail (FInjects)
import           Pos.Util.Wlog (Severity (..))
import           UTxO.Context (CardanoContext (..), initCardanoContext)


import qualified Cardano.Wallet.Kernel as Kernel
import qualified Cardano.Wallet.Kernel.Actions as Actions
import qualified Cardano.Wallet.Kernel.BListener as Kernel
import           Cardano.Wallet.Kernel.DB.AcidState (dbHdWallets)
import           Cardano.Wallet.Kernel.DB.BlockContext (BlockContext (..))
import           Cardano.Wallet.Kernel.DB.HdWallet (hdAccountRestorationState,
                     hdRootId, hdWalletsRoots)
import           Cardano.Wallet.Kernel.DB.InDb
import qualified Cardano.Wallet.Kernel.DB.Read as Kernel
import           Cardano.Wallet.Kernel.DB.Resolved (ResolvedBlock (..),
                     ResolvedTx (..))
import qualified Cardano.Wallet.Kernel.DB.Util.IxSet as IxSet
import           Cardano.Wallet.Kernel.Diffusion (WalletDiffusion (..),
                     tickDiffusionLayer)
import           Cardano.Wallet.Kernel.Keystore (Keystore)
import           Cardano.Wallet.Kernel.NodeStateAdaptor
import qualified Cardano.Wallet.Kernel.Read as Kernel
import qualified Cardano.Wallet.Kernel.Restore as Kernel
import           Cardano.Wallet.Kernel.Util.Core (txOuts, utxoRemoveInputs,
                     utxoUnions)
import           Cardano.Wallet.WalletLayer (ActiveWalletLayer (..),
                     PassiveWalletLayer (..))
import qualified Cardano.Wallet.WalletLayer.Kernel.Accounts as Accounts
import qualified Cardano.Wallet.WalletLayer.Kernel.Active as Active
import qualified Cardano.Wallet.WalletLayer.Kernel.Addresses as Addresses
import qualified Cardano.Wallet.WalletLayer.Kernel.Internal as Internal
import qualified Cardano.Wallet.WalletLayer.Kernel.Transactions as Transactions
import qualified Cardano.Wallet.WalletLayer.Kernel.Wallets as Wallets


-- | Initialize the passive wallet.
-- The passive wallet cannot send new transactions.
bracketPassiveWallet
    :: forall m n a. (MonadIO n, MonadUnliftIO m, MonadMask m)
    => ProtocolMagic
    -> Kernel.DatabaseMode
    -> Bool
    -> (Severity -> Text -> IO ())
    -> Keystore
    -> NodeStateAdaptor IO
    -> FInjects IO
    -> (PassiveWalletLayer n -> Kernel.PassiveWallet -> m a) -> m a
bracketPassiveWallet pm mode usePullMechanism logFunction keystore node fInjects f = do
    Kernel.bracketPassiveWallet pm mode logFunction keystore node fInjects $ \w -> do

      -- For each wallet in a restoration state, re-start the background
      -- restoration tasks.
      liftIO $ do
          snapshot <- Kernel.getWalletSnapshot w
          let wallets = snapshot ^. dbHdWallets . hdWalletsRoots
          for_ wallets $ \root -> do
              let accts      = Kernel.accountsByRootId snapshot (root ^. hdRootId)
                  restoring  = IxSet.findWithEvidence hdAccountRestorationState accts

              whenJust restoring $ \(src, tgt) -> do
                  (w ^. Kernel.walletLogMessage) Warning $
                      F.sformat ("bracketPassiveWallet: continuing restoration of " %
                       F.build %
                       " from checkpoint " % F.build %
                       " with target "     % F.build)
                       (root ^. hdRootId) (maybe "(genesis)" pretty src) (pretty tgt)
                  Kernel.continueRestoration w root src tgt

      -- Start the wallet worker
      let wai = Actions.WalletActionInterp
                { Actions.applyBlocks =
                  if usePullMechanism then
                      \_ -> return ()
                  else
                      \blunds -> do
                          ls <- mapM (Wallets.blundToResolvedBlock node)
                              (toList (getOldestFirst blunds))
                          let mp = catMaybes ls
                          mapM_ (Kernel.applyBlock w) mp

                 , Actions.switchToFork = \_ (OldestFirst blunds) -> do
                     -- Get the hash of the last main block before this fork.
                     let almostOldest = fst (NE.head blunds)
                     gh     <- configGenesisHash <$> getCoreConfig node
                     oldest <- withNodeState node $ \_lock ->
                                 mostRecentMainBlock gh
                                   (almostOldest ^. blockHeader . prevBlockL)

                     bs <- catMaybes <$> mapM (Wallets.blundToResolvedBlock node)
                                             (NE.toList blunds)

                     Kernel.switchToFork w (headerHash <$> oldest) bs

                 , Actions.emit = logFunction Debug
                 }
      Actions.withWalletWorker wai $ \invoke -> do
         f (passiveWalletLayer w invoke) w

  where
    passiveWalletLayer :: Kernel.PassiveWallet
                       -> (Actions.WalletAction Blund -> STM ())
                       -> PassiveWalletLayer n
    passiveWalletLayer w invoke = PassiveWalletLayer
        { -- Operations that modify the wallet
          createWallet         = Wallets.createWallet         w
        , createEosWallet      = Wallets.createEosWallet      w
        , updateWallet         = Wallets.updateWallet         w
        , updateWalletPassword = Wallets.updateWalletPassword w
        , deleteWallet         = Wallets.deleteWallet         w
        , deleteEosWallet      = Wallets.deleteEosWallet      w
        , createAccount        = Accounts.createAccount       w
        , updateAccount        = Accounts.updateAccount       w
        , deleteAccount        = Accounts.deleteAccount       w
        , createAddress        = Addresses.createAddress      w
        , importAddresses      = Addresses.importAddresses    w
        , resetWalletState     = Internal.resetWalletState    w
        , importWallet         = Internal.importWallet        w
        , applyBlocks          = invokeIO . Actions.ApplyBlocks
        , rollbackBlocks       = invokeIO . Actions.RollbackBlocks . length

          -- Read-only operations
        , getWallets           =                   join (ro $ Wallets.getWallets w)
        , getWallet            = \wId           -> join (ro $ Wallets.getWallet w wId)
        , getUtxos             = \wId           -> ro $ Wallets.getWalletUtxos wId
        , getAccounts          = \wId           -> ro $ Accounts.getAccounts         wId
        , getAccount           = \wId acc       -> ro $ Accounts.getAccount          wId acc
        , getAccountBalance    = \wId acc       -> ro $ Accounts.getAccountBalance   wId acc
        , getAccountAddresses  = \wId acc rp fo -> ro $ Accounts.getAccountAddresses wId acc rp fo
        , getAddresses         = \rp            -> ro $ Addresses.getAddresses rp
        , validateAddress      = \txt           -> ro $ Addresses.validateAddress txt
        , getTransactions      = Transactions.getTransactions w
        , getTxFromMeta        = Transactions.toTransaction w
        }
      where
        -- Read-only operations
        ro :: (Kernel.DB -> x) -> n x
        ro g = g <$> liftIO (Kernel.getWalletSnapshot w)

        invokeIO :: forall m'. MonadIO m' => Actions.WalletAction Blund -> m' ()
        invokeIO = liftIO . STM.atomically . invoke

-- | Initialize the active wallet.
-- The active wallet is allowed to send transactions, as it has the full
-- 'WalletDiffusion' layer in scope.
bracketActiveWallet
    :: forall m n a. (MonadIO m, MonadMask m, MonadIO n)
    => PassiveWalletLayer n
    -> Kernel.PassiveWallet
    -> WalletDiffusion
    -> Bool
    -> (ActiveWalletLayer n -> Kernel.ActiveWallet -> m a) -> m a
bracketActiveWallet walletPassiveLayer passiveWallet walletDiffusion walletApplyBlockPullMechanism runActiveLayer =
    Kernel.bracketActiveWallet passiveWallet walletDiffusion $ \w ->

        if walletApplyBlockPullMechanism then do

            liftIO $ logging Debug "pull mechanism (diffusion layer based) for block syncing is active"

            genesisConfig <- liftIO $ getCoreConfig $ passiveWallet ^. Kernel.walletNode
            let initialUtxo = ccUtxo $ initCardanoContext genesisConfig
            let headersConsumed = passiveWallet ^. Kernel.walletProcessedBlocks

            applyingBlockTicker <- liftIO $ async $
                tickDiffusionLayer
                logging
                tickDiffusionFunction
                (([],headersConsumed), initialUtxo)

            bracket
                (return (activeWalletLayer w))
                (\_ -> liftIO $ do
                    logging Error "stopping the wallet applying block layer..."
                    cancel applyingBlockTicker
                )
                (flip runActiveLayer w)
         else do

            liftIO $ logging Debug "push mechanism (readerT based) for block syncing is active"

            bracket
                (return (activeWalletLayer w))
                (\_ -> return ())
                (flip runActiveLayer w)
  where
    activeWalletLayer :: Kernel.ActiveWallet -> ActiveWalletLayer n
    activeWalletLayer w = ActiveWalletLayer {
          walletPassiveLayer = walletPassiveLayer
        , pay                = Active.pay              w
        , estimateFees       = Active.estimateFees     w
        , createUnsignedTx   = Active.createUnsignedTx w
        , submitSignedTx     = Active.submitSignedTx   w
        , redeemAda          = Active.redeemAda        w
        }

    logging :: Severity -> Text -> IO ()
    logging = passiveWallet ^. Kernel.walletLogMessage

    tickDiffusionFunction
        :: (Severity -> Text -> IO ())
        -> (([HeaderHash], [HeaderHash]), Utxo)
        -> IO (([HeaderHash], [HeaderHash]), Utxo)
    tickDiffusionFunction log currentState@((inProgressHeaders, consumedHeaders),utxo) = do

        nodeBlockHeaderMap <- walletRequestTip walletDiffusion

        case (List.take 1 $ Map.toList nodeBlockHeaderMap, inProgressHeaders) of

            ([(nodeId, _)], [h1, h2]) -> do
                --downloading blocks
                blocksDowloaded <- getOldestFirst <$> walletGetBlocks walletDiffusion nodeId h2 (take 1 consumedHeaders)

                let accomodatedHeaders =  map (headerHash . getBlockHeader) $ blocksDowloaded

                let prevHeaders = case consumedHeaders of
                                      [] -> (take 1 accomodatedHeaders) ++ ((reverse . drop 1 . reverse) accomodatedHeaders)
                                      cs -> (take 1 cs) ++ ((reverse . drop 1 . reverse) accomodatedHeaders)

                let slotIdsM =  map createSlotIdM blocksDowloaded

                let (txPayload, updatedUtxo) = foldl processBlockTx ([],utxo) blocksDowloaded

                now <- getCurrentTimestamp

                let context = List.zipWith3 createBlockContext (catMaybes slotIdsM) accomodatedHeaders prevHeaders

                let resolvedTxs = map (payloadToResolved now) $ catMaybes txPayload

                let resolvedBlocks = List.zipWith (createResolvedBlock now) resolvedTxs context

                mapM_ (Kernel.applyBlock passiveWallet) resolvedBlocks

                pure $ (([h1], (reverse accomodatedHeaders) ++ consumedHeaders), updatedUtxo)

            ([(_, headerIO)],_) -> do
                --collecting headers
                headerHashE <- try ( headerHash <$> headerIO ) :: IO (Either SomeException HeaderHash)
                case headerHashE of
                    Right header -> do
                        pure $ ((List.nub $ header : inProgressHeaders, consumedHeaders), utxo)
                    Left exep -> do
                        log Error (show exep)
                        pure $ currentState

            _ -> do
                -- no communication
                pure $ currentState
            where
                updateUtxo payload theUtxo =
                    let utxoAfterAddition = utxoUnions $ theUtxo : (map (snd . snd) $ catMaybes payload)
                        utxoSetToDel = Set.fromList $ concatMap fst (catMaybes payload)
                    in  utxoRemoveInputs utxoAfterAddition utxoSetToDel

                createBlockContext slotId header1 header2 =
                    if (header1 == header2) then
                        BlockContext (InDb $ slotId) (InDb $ header1) (StrictMaybe.Nothing)
                    else
                        BlockContext (InDb $ slotId) (InDb $ header1) (StrictMaybe.Just (InDb $ header2))

                createResolvedTx txIn payload time =
                    let resolvedTxIn = fst payload
                        resolvedTxOut = (fst . snd) payload
                        theUtxo = (snd . snd) payload
                    in ResolvedTx (InDb $ NE.fromList $ zip resolvedTxIn resolvedTxOut) (InDb theUtxo) (InDb $ (txIn,time))

                createResolvedBlock time resTx bCtx  =
                    case resTx of
                        Just resolvedTx ->
                            let (_, timestamp) = _fromDb $ _rtxMeta resolvedTx
                            in ResolvedBlock [resolvedTx] bCtx timestamp
                        Nothing  ->
                            ResolvedBlock [] bCtx time

                createSlotIdM = (\case
                                        Right block -> Just $ block ^. mainBlockSlot
                                        Left _  -> Nothing
                                )

                processBlockTx (payloads,currentUtxo) theBlock =
                    case theBlock of
                        Right block ->
                            let transactionsInBlock = block ^. mainBlockTxPayload . txpTxs
                                outputs = utxoUnions $ map txOuts transactionsInBlock
                                inputs = map _txInputs transactionsInBlock
                                inputsToRemove = concatMap toList inputs
                                findUtxo = utxoToLookup currentUtxo
                                inputsE = concatMap ((mapMaybe findUtxo) . toList) inputs
                                payloadToAdd = [Just (inputsToRemove, (inputsE, outputs))]
                                updatedUtxo = updateUtxo payloadToAdd currentUtxo
                            in (Just (inputsToRemove, (inputsE, outputs)) : payloads,updatedUtxo)
                        Left _  -> (Nothing : payloads,currentUtxo)

                payloadToResolved time = (\payload ->
                                              case List.take 1 $ Map.keys ((snd . snd) payload) of
                                                  [(TxInUtxo justOne _)]  -> Just $ createResolvedTx justOne payload time
                                                  _ -> Nothing
                                         )
