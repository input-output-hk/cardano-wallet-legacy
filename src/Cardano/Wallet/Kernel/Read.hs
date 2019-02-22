{-# LANGUAGE LambdaCase #-}

-- | Read-only access to the DB
module Cardano.Wallet.Kernel.Read
    ( DB
    , getEncryptedSecretKeys
    , getAddressPools
    , ownershipFromHdRoot
    , getWalletsByOwnership
    , WalletOwnership (..)
    , getWalletSnapshot
    , module Getters
    ) where

import           Universum hiding (State)

import           Data.Acid.Advanced (query')
import qualified Data.Map.Strict as Map
import           Formatting (build, sformat)

import           Cardano.Wallet.Kernel.AddressPool (AddressPool,
                     ErrAddressPoolInvalid, initAddressPool)
import           Cardano.Wallet.Kernel.AddressPoolGap (AddressPoolGap)
import           Cardano.Wallet.Kernel.DB.AcidState (DB, Snapshot (..),
                     dbHdWallets)
import           Cardano.Wallet.Kernel.DB.HdRootId (HdRootId)
import           Cardano.Wallet.Kernel.DB.HdWallet (HdAccountId, HdRoot,
                     HdRootBase (..), getHdAddressIx, hdAddressAddress,
                     hdAddressId, hdAddressIdIx, hdRootBase, hdWalletsRoots)
import           Cardano.Wallet.Kernel.DB.InDb (fromDb)
import           Cardano.Wallet.Kernel.DB.Read as Getters
import           Cardano.Wallet.Kernel.DB.Util.IxSet (ixedIndexed)
import qualified Cardano.Wallet.Kernel.DB.Util.IxSet as IxSet
import           Cardano.Wallet.Kernel.Ed25519Bip44 (ChangeChain (..),
                     deriveAddressPublicKey)
import           Cardano.Wallet.Kernel.Internal
import qualified Cardano.Wallet.Kernel.Keystore as Keystore
import           Pos.Core (Address, makePubKeyAddressBoot)
import           Pos.Core.NetworkMagic (makeNetworkMagic)
import           Pos.Crypto (EncryptedSecretKey, ProtocolMagic, PublicKey)


-- | The only effectful query on this 'PassiveWallet'.
getWalletSnapshot :: PassiveWallet -> IO DB
getWalletSnapshot pw = query' (pw ^. wallets) Snapshot

{-------------------------------------------------------------------------------
    Get Prefiltering context for all HdRnd wallets
-------------------------------------------------------------------------------}

getEncryptedSecretKeys
    :: PassiveWallet
    -> DB
    -> IO (Map HdRootId (Maybe EncryptedSecretKey))
getEncryptedSecretKeys pw db =
    Map.fromList <$> forM (walletIds db) lookup'
  where
    lookup' :: HdRootId -> IO (HdRootId, Maybe EncryptedSecretKey)
    lookup' rootId = fmap (rootId,) $ Keystore.lookup
        (makeNetworkMagic $ pw ^. walletProtocolMagic)
        rootId
        (pw ^. walletKeystore)


{-------------------------------------------------------------------------------
    Get Prefiltering context for all EOS wallets
-------------------------------------------------------------------------------}

-- | Gather all address pools across the entire DB (one pool per account)
getAddressPools
    :: ProtocolMagic
    -> DB
    -> Map HdAccountId (Either ErrAddressPoolInvalid (AddressPool Address))
getAddressPools pm db = Map.unions $ do
    (gap, accounts) <- externallyOwned $ IxSet.toList $ db ^. dbHdWallets . hdWalletsRoots
    return $ flip Map.mapWithKey accounts $ \accountId pubKey ->
        initAddressPool gap (mkAddress pubKey) (addressesByAccountId' db accountId)
  where
    externallyOwned
        :: [HdRoot]
        -> [(AddressPoolGap, Map HdAccountId PublicKey)]
    externallyOwned = rights . map (\root -> case root ^. hdRootBase of
        HdRootFullyOwned{} ->
            Left ()
        HdRootExternallyOwned _ gap accounts ->
            Right (gap, accounts))

    addressesByAccountId'
        :: DB
        -> HdAccountId
        -> [(Address, Word32)]
    addressesByAccountId' _db accId = addressesByAccountId db accId
        & IxSet.toList
        & map (view ixedIndexed)
        & map (\x ->
            ( x ^. hdAddressAddress . fromDb
            , getHdAddressIx (x ^. hdAddressId . hdAddressIdIx)
            ))

    mkAddress
        :: PublicKey
        -> Word32
        -> Address
    mkAddress accPK ix = case deriveAddressPublicKey accPK ExternalChain ix of
        Just addrPK -> makePubKeyAddressBoot (makeNetworkMagic pm) addrPK
        Nothing     -> error $
            "Cardano.Wallet.Kernel.Read.mkAddress: maximum number of \
            \addresses reached when trying to create an address with \
            \at the following index: " <> sformat build ix

ownershipFromHdRoot
    :: HdRoot
    -> WalletOwnership
ownershipFromHdRoot root = case root ^. hdRootBase of
    HdRootFullyOwned{}      -> WalletFullyOwned
    HdRootExternallyOwned{} -> WalletExternallyOwned

getWalletsByOwnership
    :: WalletOwnership
    -> DB
    -> [HdRoot]
getWalletsByOwnership ownership db =
    flip filter allRoots $ (== ownership) . ownershipFromHdRoot
  where
    allRoots = IxSet.toList $ db ^. dbHdWallets . hdWalletsRoots

data WalletOwnership
    = WalletFullyOwned
    | WalletExternallyOwned
    deriving Eq
