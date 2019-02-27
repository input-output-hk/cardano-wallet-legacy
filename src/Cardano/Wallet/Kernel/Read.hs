{-# LANGUAGE MultiWayIf #-}

-- | Read-only access to the DB
module Cardano.Wallet.Kernel.Read
    ( DB
    , getEncryptedSecretKeys
    , getAddressPools
    , addressPoolGapByRootId
    , getWalletOwnership
    , getWalletsByOwnership
    , eosAccountsByRootId
    , WalletOwnership (..)
    , GetAddressPoolGapError (..)
    , getWalletSnapshot
    , module Getters
    ) where

import           Universum hiding (State)

import           Data.Acid.Advanced (query')
import           Data.List (nub)
import qualified Data.Map.Strict as Map
import           Formatting (bprint, build, sformat, (%))
import qualified Formatting.Buildable

import           Pos.Core (Address, makePubKeyAddressBoot)
import           Pos.Core.NetworkMagic (makeNetworkMagic)
import           Pos.Crypto (EncryptedSecretKey, ProtocolMagic, PublicKey)

import           Cardano.Wallet.Kernel.AddressPool (AddressPool,
                     ErrAddressPoolInvalid (..), initAddressPool)
import           Cardano.Wallet.Kernel.AddressPoolGap (AddressPoolGap)
import           Cardano.Wallet.Kernel.DB.AcidState (DB, Snapshot (..),
                     dbHdWallets)
import           Cardano.Wallet.Kernel.DB.HdRootId (HdRootId)
import           Cardano.Wallet.Kernel.DB.HdWallet (HdAccountBase (..),
                     HdAccountId, HdRoot, getHdAddressIx, hdAccountBase,
                     hdAddressAddress, hdAddressId, hdAddressIdIx, hdRootId,
                     hdWalletsRoots)
import           Cardano.Wallet.Kernel.DB.InDb (fromDb)
import           Cardano.Wallet.Kernel.DB.Read as Getters
import           Cardano.Wallet.Kernel.DB.Util.IxSet (ixedIndexed)
import qualified Cardano.Wallet.Kernel.DB.Util.IxSet as IxSet
import           Cardano.Wallet.Kernel.Ed25519Bip44 (ChangeChain (..),
                     deriveAddressPublicKey)
import           Cardano.Wallet.Kernel.Internal
import qualified Cardano.Wallet.Kernel.Keystore as Keystore

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

getAddressPools
    :: ProtocolMagic
    -> DB
    -> Map HdAccountId (Either ErrAddressPoolInvalid (AddressPool Address))
getAddressPools pm db = Map.unions $ do
    (gap, accounts) <- map externallyOwnedRoots $ IxSet.toList $ db ^. dbHdWallets . hdWalletsRoots
    return $ flip Map.mapWithKey accounts $ \accountId pubKey ->
        initAddressPool gap (mkAddress pubKey) (addressesByAccountId' db accountId)
  where
    -- NOTE:
    -- We arbitrarily take the maximum AddressPoolGap here. This is rather
    -- incorrect but will go away with the next PR as part of #357!
    externallyOwnedRoots
        :: HdRoot
        -> (AddressPoolGap, Map HdAccountId PublicKey)
    externallyOwnedRoots root = bimap maximum Map.unions $ unzip $ catMaybes $ do
        account <- IxSet.toList $ Getters.accountsByRootId db (root ^. hdRootId)
        return $ case account ^. hdAccountBase of
            HdAccountBaseFO{} ->
                Nothing
            (HdAccountBaseEO accId pubKey gap) ->
                Just (gap, Map.singleton accId pubKey)

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


-- | For a given rootId, returns either Nothing if this is not an Eo wallet,
-- or Just the address pool gap along with the account ids and public keys.
--
-- For both Eo and Fo wallets we return an exception if there are
-- no accounts at all or if the accounts are not all of the same type.
-- For Eo wallets we also return an exception if the gap is not consistent
-- for all accounts in the wallet.
eosAccountsByRootId
    :: HdRootId
    -> DB
    -> Maybe (Either GetAddressPoolGapError ([(HdAccountId, PublicKey)], AddressPoolGap))
eosAccountsByRootId rootId db = do
    let accounts = IxSet.toList $ Getters.accountsByRootId db rootId
        bases = flip map accounts $ \hdAccount -> case hdAccount ^. hdAccountBase of
                    -- It is EOS-wallet, so all accounts must have EO-branch.
                    HdAccountBaseFO _ -> Left ()
                    HdAccountBaseEO accId accPk gap -> Right (accId,accPk,gap)
        (accFOs, accEOs) = partitionEithers bases
        gaps = map (\(_,_,gap) -> gap) accEOs
        accs' = map (\(accId,accPk,_) -> (accId,accPk)) accEOs
    if | null bases ->
            Just . Left $ GetEosWalletErrorNoAccounts anId
       | mixedAccs accFOs accEOs ->
            Just . Left $ GetEosWalletErrorWrongAccounts anId
       | onlyA accFOs accEOs ->
            Nothing -- this is not an EO wallet
       | gapsDiffer gaps ->
            Just . Left $ GetEosWalletErrorGapsDiffer anId
       | otherwise ->
            let gap:_ = gaps in (Just . Right) (accs',gap)
  where
    anId = sformat build rootId

    mixedAccs a b = not $ (onlyA a b) || (onlyA b a)
    onlyA a b = ((not . null) a) && (null b)
    gapsDiffer gs = length (nub gs) > 1

addressPoolGapByRootId
    :: HdRootId
    -> DB
    -> Either GetAddressPoolGapError AddressPoolGap
addressPoolGapByRootId rootId db
    = case eosAccountsByRootId rootId db of
        Nothing -> -- not an EO wallet
            Left $ GetEosWalletErrorWrongAccounts (sformat build rootId)
        Just res -> snd <$> res

getWalletOwnership
    :: HdRootId
    -> DB
    -> WalletOwnership
getWalletOwnership rootId db =
    case addressPoolGapByRootId rootId db of
        Right _ -> WalletExternallyOwned
        Left _  -> WalletFullyOwned

getWalletsByOwnership
    :: WalletOwnership
    -> DB
    -> [HdRoot]
getWalletsByOwnership ownership db =
    flip filter allRoots $ \root ->
        getWalletOwnership (root ^. hdRootId) db == ownership
  where
    allRoots = IxSet.toList $ db ^. dbHdWallets . hdWalletsRoots

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

data WalletOwnership
    = WalletFullyOwned
    | WalletExternallyOwned
    deriving Eq
