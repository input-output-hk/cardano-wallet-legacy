module Cardano.Wallet.WalletLayer.Kernel.Internal (
    resetWalletState
  , importWallet
  ) where

import           Universum

import           Control.Concurrent.MVar (modifyMVar_)
import           Data.Acid.Advanced (update')
import           System.IO.Error (isDoesNotExistError)

import           Cardano.Wallet.API.V1.Types (Wallet, WalletImport (..),
                     derivationScheme)
import           Cardano.Wallet.Kernel.DB.AcidState (ClearDB (..))
import           Cardano.Wallet.Kernel.DB.TxMeta
import qualified Cardano.Wallet.Kernel.Internal as Kernel
import qualified Cardano.Wallet.Kernel.Keystore as Keystore
import qualified Cardano.Wallet.Kernel.Submission as Submission
import           Cardano.Wallet.WalletLayer (CreateWallet (..),
                     ImportWalletError (..))
import           Cardano.Wallet.WalletLayer.Kernel.Wallets (createWallet)


-- | Reset wallet state
resetWalletState :: MonadIO m => Kernel.PassiveWallet -> m ()
resetWalletState w = liftIO $ do
    -- TODO: reset also the wallet worker (CBR-415)

    -- stop restoration and empty it`s state.
    -- TODO: A restoration may start between this call and the db modification
    -- but as this is for testing only we keep it that way for now. (CBR-415)
    Kernel.stopAllRestorations w

    -- This pauses any effect the Submission worker can have.
    -- We don`t actually stop and restart the thread, but once
    -- we have the MVar the worker can have no effects.
    modifyMVar_ (w ^. Kernel.walletSubmission) $ \_ -> do

        -- clear both dbs.
        update' (w ^. Kernel.wallets) $ ClearDB
        clearMetaDB (w ^. Kernel.walletMeta)
        -- clear submission state.
        return Submission.emptyWalletSubmission

-- | Imports a 'Wallet' from a backup on disk.
importWallet :: MonadIO m
             => Kernel.PassiveWallet
             -> WalletImport
             -> m (Either ImportWalletError Wallet)
importWallet pw WalletImport{..} = liftIO $ do
    secretE <- try $ Keystore.readWalletSecret wiFilePath
    case secretE of
         Left e ->
             if isDoesNotExistError e
                 then return (Left $ ImportWalletFileNotFound wiFilePath)
                 else throwM e
         Right mbEsk -> do
             case mbEsk of
                 Nothing  -> return (Left $ ImportWalletNoWalletFoundInBackup wiFilePath)
                 Just esk -> do
                     res <- liftIO $ createWallet pw (ImportWalletFromESK esk wiSpendingPassword $ derivationScheme wiDerivationScheme)
                     return $ case res of
                          Left e               -> Left (ImportWalletCreationFailed e)
                          Right importedWallet -> Right importedWallet
