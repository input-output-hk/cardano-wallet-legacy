{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}

module Cardano.Wallet.WalletLayer.Kernel.Wallets (
      createWallet
    , createEosWallet
    , updateWallet
    , updateEosWallet
    , updateWalletPassword
    , deleteWallet
    , deleteEosWallet
    , getWallet
    , getEosWallet
    , getWallets
    , getEosWallets
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
import           Pos.Util.Wlog (Severity (..))

import qualified Cardano.Mnemonic as Mnemonic
import qualified Cardano.Wallet.API.V1.Types as V1
import qualified Cardano.Wallet.Kernel.Accounts as Kernel
import           Cardano.Wallet.Kernel.Addresses (newHdAddress)
import qualified Cardano.Wallet.Kernel.DB.HdRootId as HD
import qualified Cardano.Wallet.Kernel.DB.HdWallet as HD
import           Cardano.Wallet.Kernel.DB.InDb (fromDb)
import qualified Cardano.Wallet.Kernel.DB.TxMeta.Types as Kernel
import           Cardano.Wallet.Kernel.DB.Util.IxSet (IxSet)
import qualified Cardano.Wallet.Kernel.DB.Util.IxSet as IxSet
import           Cardano.Wallet.Kernel.Internal (walletKeystore, walletMeta,
                     walletProtocolMagic, (<.>), _wriProgress)
import qualified Cardano.Wallet.Kernel.Internal as Kernel
import qualified Cardano.Wallet.Kernel.Keystore as Keystore
import           Cardano.Wallet.Kernel.Read (WalletOwnership (..),
                     addressPoolGapByRootId, eosAccountsByRootId,
                     getWalletOwnership, getWalletsByOwnership)
import qualified Cardano.Wallet.Kernel.Read as Kernel
import           Cardano.Wallet.Kernel.Restore (blundToResolvedBlock,
                     restoreWallet)
import qualified Cardano.Wallet.Kernel.Wallets as Kernel
import           Cardano.Wallet.WalletLayer (CreateWallet (..),
                     CreateWalletError (..), DeleteWalletError (..),
                     GetEosWalletError (..), GetUtxosError (..),
                     GetWalletError (..), UpdateEosWalletError (..),
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
    db <- liftIO $ Kernel.getWalletSnapshot wallet
    case getWalletOwnership rootId db of
        WalletExternallyOwned ->
            exceptT $ Left $ UpdateWalletError $ HD.UnknownHdRoot rootId
        WalletFullyOwned -> do
            v1wal <- fmap (uncurry toWallet) $
                       withExceptT UpdateWalletError $ ExceptT $ liftIO $
                         Kernel.updateHdWallet wallet rootId newLevel newName
            updateSyncState wallet rootId v1wal
  where
    newLevel = fromAssuranceLevel v1Level
    newName  = HD.WalletName v1Name

updateEosWallet
    :: MonadIO m
    => Kernel.PassiveWallet
    -> V1.WalletId
    -> V1.UpdateEosWallet
    -> m (Either UpdateEosWalletError V1.EosWallet)
updateEosWallet wallet wId (V1.UpdateEosWallet v1Level v1Name newGap) = runExceptT $ do
    rootId <- withExceptT UpdateEosWalletWalletIdDecodingFailed $ fromRootId wId
    db' <- liftIO $ Kernel.getWalletSnapshot wallet
    case eosAccountsByRootId rootId db' of
        Nothing -> exceptT $ Left $ UpdateEosWalletError $ HD.UnknownHdRoot rootId
        Just (Left e) -> exceptT $ Left $ UpdateEosWalletErrorAddressPoolGap e
        Just (Right (accountsData, _)) -> do
            (db, newHdRoot) <- withExceptT UpdateEosWalletError $ ExceptT $ liftIO $
                Kernel.updateHdWallet wallet rootId newLevel newName
            -- 'HdRoot' doesn't contain address pool gap, only corresponding
            -- 'HdAccount's contain it. So we have to update all these accounts.
            result <- forM accountsData $ \(accId, _) -> do
                res <- liftIO $ Kernel.updateAccountGap accId newGap wallet
                return $ either (Left . UpdateEosWalletAccountError) Right res
            exceptT $ case lefts result of
                []    -> Right $ toEosWallet db newHdRoot newGap
                (e:_) -> Left e
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
    db <- liftIO $ Kernel.getWalletSnapshot wallet
    case getWalletOwnership rootId db of
        WalletExternallyOwned ->
            exceptT
                $ Left
                $ UpdateWalletPasswordError
                $ Kernel.UpdateWalletPasswordUnknownHdRoot
                $ HD.UnknownHdRoot rootId
        WalletFullyOwned -> do
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
    db <- liftIO $ Kernel.getWalletSnapshot wallet
    case getWalletOwnership rootId db of
        WalletExternallyOwned ->
            exceptT $ Left $ DeleteWalletError $ HD.UnknownHdRoot rootId
        WalletFullyOwned ->
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
    db <- liftIO $ Kernel.getWalletSnapshot wallet
    case getWalletOwnership rootId db of
        WalletFullyOwned ->
            exceptT $ Left $ DeleteWalletError $ HD.UnknownHdRoot rootId
        WalletExternallyOwned ->
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
    hdRoot <- withExceptT GetWalletError $ exceptT $ Kernel.lookupHdRootId db rootId
    case getWalletOwnership rootId db of
        WalletExternallyOwned ->
            exceptT $ Left $ GetWalletError $ HD.UnknownHdRoot rootId
        WalletFullyOwned ->
            updateSyncState wallet rootId $ toWallet db hdRoot

-- | Gets a specific EOS-wallet.
getEosWallet
    :: MonadIO m
    => Kernel.PassiveWallet
    -> V1.WalletId
    -> Kernel.DB
    -> m (Either GetEosWalletError V1.EosWallet)
getEosWallet _wallet wId db = runExceptT $ do
    debug $ "looking for: " <.> wId
    rootId <- withExceptT GetEosWalletWalletIdDecodingFailed (fromRootId wId)
    hdRoot <- withExceptT GetEosWalletError $ exceptT $ Kernel.lookupHdRootId db rootId
    debug $ "successfully decoded HdRoot: " <.> hdRoot
    case addressPoolGapByRootId rootId db of
        Left _ ->
            -- No gap. Although wallet exists in DB, it's not EOS-wallet,
            -- so report about it as if this wallet doesn't exist.
            exceptT $ Left $ GetEosWalletError $ HD.UnknownHdRoot rootId
        Right gap -> do
            debug $ "successfully recovered gap from accounts: " <.> gap
            return $ toEosWallet db hdRoot gap
  where
    debug :: MonadIO m => Text -> m ()
    debug = Kernel.mkLogger _wallet "getEosWallet" Debug

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
    fmap IxSet.fromList $ forM foRoots $ \root -> do
        let rootId = root ^. HD.hdRootId
        updateSyncState wallet rootId (toWallet db root)
  where
    foRoots = getWalletsByOwnership WalletFullyOwned db

getEosWallets
    :: MonadIO m
    => Kernel.PassiveWallet
    -> Kernel.DB
    -> m (Either GetEosWalletError (IxSet V1.EosWallet))
getEosWallets _wallet db = do
    let result = traverse (\root -> either Left (Right . toEosWallet db root) $
                    addressPoolGapByRootId (root ^. HD.hdRootId) db)
                    eosRoots
    return $ case result of
        Left e           -> Left $ GetEosWalletErrorAddressPoolGap e
        Right eosWallets -> Right $ IxSet.fromList eosWallets
  where
    eosRoots = getWalletsByOwnership WalletExternallyOwned db

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
