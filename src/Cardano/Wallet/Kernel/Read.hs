{-# LANGUAGE MultiWayIf #-}

-- | Read-only access to the DB
module Cardano.Wallet.Kernel.Read (
    -- * Read-only access to the DB
    DB -- opaque
    -- * Exceptions
  , GetAddressPoolGapError (..)
    -- ** Helper
  , addressPoolGapByRootId
  , getHdRndWallets
  , getEosPools
    -- ** The only effectful getter you will ever need
  , getWalletSnapshot
    -- ** Pure getters acting on a DB snapshot
  , module Getters
  ) where

import           Universum hiding (State)


import           Data.Acid.Advanced (query')
import qualified Data.Map.Strict as Map
import           Data.List (nub)
import           Formatting (bprint, build, sformat, (%))
import qualified Formatting.Buildable
import           Serokell.Util (listJson)

import           Pos.Core (Address)
import           Pos.Core.NetworkMagic (NetworkMagic, makeNetworkMagic)
import           Pos.Crypto (EncryptedSecretKey, ProtocolMagic, PublicKey)
import           Pos.Util.Wlog (Severity (..))

import           Cardano.Wallet.Kernel.AddressPool (AddressPool)
import           Cardano.Wallet.Kernel.AddressPoolGap (AddressPoolGap)
import           Cardano.Wallet.Kernel.DB.AcidState (DB, Snapshot (..),
                    dbHdWallets)
import           Cardano.Wallet.Kernel.DB.HdRootId (HdRootId)
import           Cardano.Wallet.Kernel.DB.HdWallet (HdAccountId, HdRoot,
                    HdAccountBase (..), hdAccountBase, hdRootId, hdWalletsRoots,
                    mkAddressPoolExisting)
import           Cardano.Wallet.Kernel.DB.Read as Getters
import qualified Cardano.Wallet.Kernel.DB.Util.IxSet as IxSet
import           Cardano.Wallet.Kernel.Internal
import qualified Cardano.Wallet.Kernel.Keystore as Keystore

-- | The only effectful query on this 'PassiveWallet'.
getWalletSnapshot :: PassiveWallet -> IO DB
getWalletSnapshot pw = query' (pw ^. wallets) Snapshot

-- | Get HD Random wallet roots along with the wallet ESK required for
-- prefiltering Hd Rnd wallets.
--
-- For wallets without a corresponding secret key we log an error. This
-- indicates a bug somewhere, but there is not much we can do about it here,
-- since this runs in the context of applying a block.
getHdRndWallets
    :: DB
    -> Keystore.Keystore
    -> ProtocolMagic
    -> (Severity -> Text -> IO ())
    -> IO (Map HdRootId EncryptedSecretKey)
getHdRndWallets snapshot ks pm logger = do
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


-- Get HD Sequential wallet accounts from Acidstate, along with the
-- associated AddressPool required for prefiltering each account.
getEosPools
    :: MonadIO m
    => DB
    -> m (Map HdAccountId (AddressPool Address))
getEosPools db
    = return . Map.fromList $ concatMap toAccountAddressPools' allRoots
  where
    allRoots = IxSet.toList $ db ^. dbHdWallets . hdWalletsRoots

    toAccountAddressPools' root
        = toAccountAddressPools root (addressPoolGapByRootId (root ^. hdRootId) db)

    mkPool gap (accId,pk)
        = case mkAddressPoolExisting (error "TODO @@@ mkAddress") pk gap (error "TODO @@@addrs") of
                Left invalidPoolErr -> error (sformat build invalidPoolErr)
                Right pool -> (accId, pool)

    toAccountAddressPools
        :: HdRoot
        -> Either GetAddressPoolGapError ([(HdAccountId, PublicKey)], AddressPoolGap)
        -> [(HdAccountId, AddressPool Address)]
    toAccountAddressPools _root eitherAccounts-- (accs,gap)
        = case eitherAccounts of
            Left getPoolErr -> error (sformat build getPoolErr)
            Right (accs,gap) -> map (mkPool gap) accs

addressPoolGapByRootId
    :: HdRootId
    -> DB
    -> Either GetAddressPoolGapError ([(HdAccountId, PublicKey)], AddressPoolGap)
addressPoolGapByRootId rootId db = do
    let accounts = IxSet.toList $ Getters.accountsByRootId db rootId
        bases = flip map accounts $ \hdAccount -> case hdAccount ^. hdAccountBase of
                    -- It is EOS-wallet, so all accounts must have EO-branch.
                    HdAccountBaseFO _       -> Left ()
                    HdAccountBaseEO accId accPk gap -> Right (accId,accPk,gap)
        (errors, accs) = partitionEithers bases
        gaps = map  (\(_,_,gap) -> gap) accs
        accs' = map (\(accId,accPk,_) -> (accId,accPk)) accs
    if | null accs             -> Left $ GetEosWalletErrorNoAccounts anId
       | not . null $ errors   -> Left $ GetEosWalletErrorWrongAccounts anId
       | length (nub gaps) > 1 -> Left $ GetEosWalletErrorGapsDiffer anId
       | otherwise             -> let [gap] = gaps in Right (accs',gap)
  where
    anId = sformat build rootId

data GetAddressPoolGapError =
      GetEosWalletErrorNoAccounts Text
    | GetEosWalletErrorWrongAccounts Text
    | GetEosWalletErrorGapsDiffer Text
    deriving Eq

instance Buildable GetAddressPoolGapError where
    build (GetEosWalletErrorNoAccounts txt) =
        bprint ("GetEosWalletErrorNoAccounts " % build) txt
    build (GetEosWalletErrorWrongAccounts txt) =
        bprint ("FO-accounts found in EOS-wallet " % build) txt
    build (GetEosWalletErrorGapsDiffer txt) =
        bprint ("Address pool gaps differ, for EOS-wallet " % build) txt
