-- | Read-only access to the DB
module Cardano.Wallet.Kernel.Read (
    -- * Read-only access to the DB
    DB -- opaque
    -- ** Helper
  , getFOWallets
    -- ** The only effectful getter you will ever need
  , getWalletSnapshot
    -- ** Pure getters acting on a DB snapshot
  , module Getters
  ) where

import           Universum hiding (State)

import           Data.Acid.Advanced (query')
import qualified Data.Map.Strict as Map
import           Formatting (sformat, (%))
import           Serokell.Util (listJson)

import           Pos.Core.NetworkMagic (NetworkMagic, makeNetworkMagic)
import           Pos.Crypto (EncryptedSecretKey, ProtocolMagic)
import           Pos.Util.Wlog (Severity (..))

import           Cardano.Wallet.Kernel.DB.AcidState (DB, Snapshot (..))
import           Cardano.Wallet.Kernel.DB.HdRootId (HdRootId)
import           Cardano.Wallet.Kernel.DB.Read as Getters
import           Cardano.Wallet.Kernel.Internal
import qualified Cardano.Wallet.Kernel.Keystore as Keystore

-- | The only effectful query on this 'PassiveWallet'.
getWalletSnapshot :: PassiveWallet -> IO DB
getWalletSnapshot pw = query' (pw ^. wallets) Snapshot

-- | Get wallet credentials
--
-- For wallets without a corresponding secret key we log an error. This
-- indicates a bug somewhere, but there is not much we can do about it here,
-- since this runs in the context of applying a block.
getWalletCredentials
    :: DB
    -> Keystore.Keystore
    -> ProtocolMagic
    -> (Severity -> Text -> IO ())
    -> IO (Map HdRootId EncryptedSecretKey)
getWalletCredentials snapshot ks pm logger = do
    (creds, missing) <- fmap partitionEithers $
      forM (walletIds snapshot) $ \walletId ->
        aux walletId <$> Keystore.lookup nm walletId ks
    unless (null missing) $ logger Error (errMissing missing)
    return (Map.fromList creds)
  where
    nm :: NetworkMagic
    nm = makeNetworkMagic pm

    aux :: HdRootId
        -> Maybe EncryptedSecretKey
        -> Either (HdRootId, EncryptedSecretKey) HdRootId
    aux walletId Nothing    = Right walletId
    aux walletId (Just esk) = Left (walletId, esk)

    errMissing :: [HdRootId] -> Text
    errMissing = sformat ("Root key missing for " % listJson)

-- | Prefiltering Context for HdRnd wallets
getFOWallets
    :: PassiveWallet
    -> DB
    -> IO (Map HdRootId EncryptedSecretKey)
getFOWallets pw db
    = getWalletCredentials db
        (pw ^. walletKeystore)
        (pw ^. walletProtocolMagic)
        (pw ^. walletLogMessage)
