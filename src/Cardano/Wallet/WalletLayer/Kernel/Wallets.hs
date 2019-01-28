{-# LANGUAGE LambdaCase #-}
module Cardano.Wallet.WalletLayer.Kernel.Wallets (
      createWallet
    , createEosWallet
    , updateWallet
    , updateWalletPassword
    , deleteWallet
    , deleteEosWallet
    , getWallet
    , getEosWallet
    , getWallets
    , getWalletUtxos
    , blundToResolvedBlock
    ) where

import           Universum

import           Control.Monad.Except (throwError)
import           Data.Coerce (coerce)
import           Data.Default (def)

import           Pos.Chain.Txp (Utxo)
import           Pos.Core (mkCoin)
import           Pos.Core.NetworkMagic (NetworkMagic, makeNetworkMagic)
import           Pos.Crypto.Signing

import qualified Cardano.Mnemonic as Mnemonic
import qualified Cardano.Wallet.API.V1.Types as V1
import           Cardano.Wallet.Kernel.Addresses (newHdAddress)
import           Cardano.Wallet.Kernel.DB.AcidState (dbHdWallets)
import qualified Cardano.Wallet.Kernel.DB.HdRootId as HD
import qualified Cardano.Wallet.Kernel.DB.HdWallet as HD
import           Cardano.Wallet.Kernel.DB.InDb (fromDb)
import qualified Cardano.Wallet.Kernel.DB.TxMeta.Types as Kernel
import           Cardano.Wallet.Kernel.DB.Util.IxSet (IxSet)
import qualified Cardano.Wallet.Kernel.DB.Util.IxSet as IxSet
import           Cardano.Wallet.Kernel.Internal (walletKeystore, walletMeta,
                     walletProtocolMagic, _wriProgress)
import qualified Cardano.Wallet.Kernel.Internal as Kernel
import qualified Cardano.Wallet.Kernel.Keystore as Keystore
import qualified Cardano.Wallet.Kernel.Read as Kernel
import           Cardano.Wallet.Kernel.Restore (blundToResolvedBlock,
                     restoreWallet)
import qualified Cardano.Wallet.Kernel.Wallets as Kernel
import           Cardano.Wallet.WalletLayer (CreateWallet (..),
                     CreateWalletError (..), DeleteWalletError (..),
                     GetUtxosError (..), GetWalletError (..),
                     UpdateWalletError (..), UpdateWalletPasswordError (..))
import           Cardano.Wallet.WalletLayer.Kernel.Conv

createWallet :: MonadIO m
             => Kernel.PassiveWallet
             -> CreateWallet
             -> m (Either CreateWalletError V1.Wallet)
createWallet wallet newWalletRequest = liftIO $ do
    let nm = makeNetworkMagic $ wallet ^. walletProtocolMagic
    case newWalletRequest of
        CreateWallet newWallet@V1.NewWallet{..} ->
            case newwalOperation of
                V1.RestoreWallet -> restore nm newWallet
                V1.CreateWallet  -> create newWallet
        ImportWalletFromESK esk mbSpendingPassword ->
            restoreFromESK nm
                           esk
                           (spendingPassword mbSpendingPassword)
                           "Imported Wallet"
                           HD.AssuranceLevelNormal
  where
    create :: V1.NewWallet -> IO (Either CreateWalletError V1.Wallet)
    create newWallet@V1.NewWallet{..} = runExceptT $ do
      root <- withExceptT CreateWalletError $ ExceptT $
                Kernel.createHdWallet wallet
                                      (mnemonic newWallet)
                                      (spendingPassword newwalSpendingPassword)
                                      (fromAssuranceLevel newwalAssuranceLevel)
                                      (HD.WalletName newwalName)
      return (mkRoot newwalName newwalAssuranceLevel root)

    restore :: NetworkMagic
            -> V1.NewWallet
            -> IO (Either CreateWalletError V1.Wallet)
    restore nm newWallet@V1.NewWallet{..} = do
        let esk    = snd $ safeDeterministicKeyGen
                             (Mnemonic.mnemonicToSeed (mnemonic newWallet))
                             (spendingPassword newwalSpendingPassword)
        restoreFromESK nm
                       esk
                       (spendingPassword newwalSpendingPassword)
                       newwalName
                       (fromAssuranceLevel newwalAssuranceLevel)

    restoreFromESK :: NetworkMagic
                   -> EncryptedSecretKey
                   -> PassPhrase
                   -> Text
                   -> HD.AssuranceLevel
                   -> IO (Either CreateWalletError V1.Wallet)
    restoreFromESK nm esk pwd walletName hdAssuranceLevel = runExceptT $ do
        let rootId = HD.eskToHdRootId nm esk

        -- Insert the 'EncryptedSecretKey' into the 'Keystore'
        liftIO $ Keystore.insert rootId esk (wallet ^. walletKeystore)

        -- Synchronously restore the wallet balance, and begin to
        -- asynchronously reconstruct the wallet's history.
        let mbHdAddress = newHdAddress nm
                                       esk
                                       pwd
                                       (Kernel.defaultHdAccountId rootId)
                                       (Kernel.defaultHdAddressId rootId)
        case mbHdAddress of
            Nothing -> throwError (CreateWalletError Kernel.CreateWalletDefaultAddressDerivationFailed)
            Just hdAddress -> do
                (root, coins) <- withExceptT (CreateWalletError . Kernel.CreateWalletFailed) $ ExceptT $
                    restoreWallet
                      wallet
                      (pwd /= emptyPassphrase)
                      (Just (hdAddress ^. HD.hdAddressAddress . fromDb))
                      (HD.WalletName walletName)
                      hdAssuranceLevel
                      esk

                -- Return the wallet information, with an updated balance.
                let root' = mkRoot walletName (toAssuranceLevel hdAssuranceLevel) root
                updateSyncState wallet rootId (root' { V1.walBalance = V1.WalletCoin coins })

    mkRoot :: Text -> V1.AssuranceLevel -> HD.HdRoot -> V1.Wallet
    mkRoot v1WalletName v1AssuranceLevel hdRoot = V1.Wallet {
          walId                         = walletId
        , walName                       = v1WalletName
        , walBalance                    = V1.WalletCoin (mkCoin 0)
        , walHasSpendingPassword        = hasSpendingPassword
        , walSpendingPasswordLastUpdate = V1.WalletTimestamp lastUpdate
        , walCreatedAt                  = V1.WalletTimestamp createdAt
        , walAssuranceLevel             = v1AssuranceLevel
        , walSyncState                  = V1.Synced
        }
      where
        (hasSpendingPassword, lastUpdate) =
            case hdRoot ^. HD.hdRootHasPassword of
                 HD.NoSpendingPassword lr  -> (False, lr ^. fromDb)
                 HD.HasSpendingPassword lu -> (True, lu ^. fromDb)
        createdAt  = hdRoot ^. HD.hdRootCreatedAt . fromDb
        walletId   = toRootId $ hdRoot ^. HD.hdRootId

    mnemonic (V1.NewWallet (V1.BackupPhrase m) _ _ _ _) = m
    spendingPassword = maybe emptyPassphrase coerce


createEosWallet :: MonadIO m
                => Kernel.PassiveWallet
                -> V1.NewEosWallet
                -> m (Either CreateWalletError V1.EosWallet)
createEosWallet wallet req = runExceptT $ do
    let gap  = fromMaybe def (V1.neweoswalAddressPoolGap req)
    let accs = mkAccount <$> V1.neweoswalAccounts req
    let name = HD.WalletName $ V1.neweoswalName req
    let alvl = fromAssuranceLevel $ V1.neweoswalAssuranceLevel req

    hdRoot <- withExceptT CreateWalletError $ ExceptT $ liftIO $
        Kernel.createEosHdWallet wallet accs gap alvl name

    return $ V1.EosWallet
        { eoswalId             = toRootId $ hdRoot ^. HD.hdRootId
        , eoswalName           = V1.neweoswalName req
        , eoswalAddressPoolGap = gap
        , eoswalBalance        = V1.WalletCoin (mkCoin 0)
        , eoswalAssuranceLevel = V1.neweoswalAssuranceLevel req
        , eoswalCreatedAt      = V1.WalletTimestamp $ hdRoot ^. HD.hdRootCreatedAt . fromDb
        }
  where
    mkAccount (V1.AccountPublicKeyWithIx pk ix) =
        (pk, HD.HdAccountIx (V1.getAccIndex ix))

-- | Updates the 'SpendingPassword' for this wallet.
updateWallet :: MonadIO m
             => Kernel.PassiveWallet
             -> V1.WalletId
             -> V1.WalletUpdate
             -> m (Either UpdateWalletError V1.Wallet)
updateWallet wallet wId (V1.WalletUpdate v1Level v1Name) = runExceptT $ do
    rootId <- withExceptT UpdateWalletWalletIdDecodingFailed $ fromRootId wId
    v1wal <- fmap (uncurry toWallet) $
               withExceptT UpdateWalletError $ ExceptT $ liftIO $
                 Kernel.updateHdWallet wallet rootId newLevel newName
    updateSyncState wallet rootId v1wal
  where
    newLevel = fromAssuranceLevel v1Level
    newName  = HD.WalletName v1Name

-- | Updates the 'SpendingPassword' for this wallet.
updateWalletPassword :: MonadIO m
                     => Kernel.PassiveWallet
                     -> V1.WalletId
                     -> V1.PasswordUpdate
                     -> m (Either UpdateWalletPasswordError V1.Wallet)
updateWalletPassword wallet
                     wId
                     (V1.PasswordUpdate
                       (V1.WalletPassPhrase oldPwd)
                       (V1.WalletPassPhrase newPwd)) = runExceptT $ do
    rootId <- withExceptT UpdateWalletPasswordWalletIdDecodingFailed $
                fromRootId wId
    v1wal <- fmap (uncurry toWallet) $
              withExceptT UpdateWalletPasswordError $ ExceptT $ liftIO $
                Kernel.updatePassword wallet rootId oldPwd newPwd
    updateSyncState wallet rootId v1wal

-- | Deletes a wallet, together with every account & addresses belonging to it.
-- If this wallet was restoring, then the relevant async worker is correctly
-- canceled.
deleteWallet :: MonadIO m
             => Kernel.PassiveWallet
             -> V1.WalletId
             -> m (Either DeleteWalletError ())
deleteWallet wallet wId = runExceptT $ do
    rootId <- withExceptT DeleteWalletWalletIdDecodingFailed $ fromRootId wId
    withExceptT DeleteWalletError $ ExceptT $ liftIO $ do
        let nm = makeNetworkMagic (wallet ^. walletProtocolMagic)
        Kernel.removeRestoration wallet rootId
        Kernel.deleteTxMetas (wallet ^. walletMeta) rootId Nothing
        Kernel.deleteHdWallet nm wallet rootId

-- | Deletes external wallets. Please note that there's no actions in the
-- 'Keystore', because it contains only root secret keys.
deleteEosWallet :: MonadIO m
                => Kernel.PassiveWallet
                -> V1.WalletId
                -> m (Either DeleteWalletError ())
deleteEosWallet wallet wId = runExceptT $ do
    rootId <- withExceptT DeleteWalletWalletIdDecodingFailed $ fromRootId wId
    withExceptT DeleteWalletError $ ExceptT $ liftIO $ do
        Kernel.deleteTxMetas (wallet ^. walletMeta) rootId Nothing
        Kernel.deleteEosHdWallet wallet rootId

-- | Gets a specific wallet.
getWallet :: MonadIO m
          => Kernel.PassiveWallet
          -> V1.WalletId
          -> Kernel.DB
          -> m (Either GetWalletError V1.Wallet)
getWallet wallet wId db = runExceptT $ do
    rootId <- withExceptT GetWalletWalletIdDecodingFailed (fromRootId wId)
    v1wal <- fmap (toWallet db) $
                withExceptT GetWalletError $ exceptT $
                    Kernel.lookupHdRootId db rootId
    updateSyncState wallet rootId v1wal

-- | Gets a specific EOS-wallet.
getEosWallet
    :: MonadIO m
    => Kernel.PassiveWallet
    -> V1.WalletId
    -> Kernel.DB
    -> m (Either GetWalletError V1.EosWallet)
getEosWallet _wallet wId db = runExceptT $ do
    rootId <- withExceptT GetWalletWalletIdDecodingFailed (fromRootId wId)
    fmap (toEosWallet db) $ withExceptT GetWalletError $ exceptT $
        Kernel.lookupHdRootId db rootId

-- | Gets all the wallets known to this edge node.
--
-- NOTE: The wallet sync state is not set here; use 'updateSyncState' to
--       get a correct result.
--
-- TODO: Avoid IxSet creation [CBR-347].
getWallets :: MonadIO m
           => Kernel.PassiveWallet
           -> Kernel.DB
           -> m (IxSet V1.Wallet)
getWallets wallet db =
    fmap IxSet.fromList $ forM (IxSet.toList allRoots) $ \root -> do
        let rootId = root ^. HD.hdRootId
        updateSyncState wallet rootId (toWallet db root)
  where
    allRoots = db ^. dbHdWallets . HD.hdWalletsRoots

-- | Gets Utxos per account of a wallet.
getWalletUtxos
    :: V1.WalletId
    -> Kernel.DB
    -> Either GetUtxosError [(V1.Account, Utxo)]
getWalletUtxos wId db = runExcept $ do
    rootId <- withExceptT GetUtxosWalletIdDecodingFailed $
        fromRootId wId

    withExceptT GetUtxosGetAccountsError $ exceptT $ do
        _rootExists <- Kernel.lookupHdRootId db rootId
        return ()

    let accounts = Kernel.accountsByRootId db rootId

    forM (IxSet.toList accounts) $ \account ->
        withExceptT GetUtxosCurrentAvailableUtxoError $ exceptT $ do
            utxo <- Kernel.currentAvailableUtxo db (account ^. HD.hdAccountId)
            return (toAccount db account, utxo)

updateSyncState :: MonadIO m
                => Kernel.PassiveWallet
                -> HD.HdRootId
                -> V1.Wallet
                -> m V1.Wallet
updateSyncState wallet wId v1wal = liftIO $ do
    wss      <- Kernel.lookupRestorationInfo wallet wId
    progress <- traverse _wriProgress wss
    return v1wal { V1.walSyncState = toSyncState progress }
