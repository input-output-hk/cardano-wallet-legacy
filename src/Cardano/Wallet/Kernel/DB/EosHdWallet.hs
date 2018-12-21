{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE RankNTypes                 #-}
-- TODO: Not sure about the best way to avoid the orphan instances here
{-# OPTIONS_GHC -fno-warn-orphans -Wno-redundant-constraints #-}

-- | Externally-owned sequential (EOS) HD wallets
module Cardano.Wallet.Kernel.DB.EosHdWallet (
    -- * Supporting types
    EosHdWallets(..)
  , EosHdRootId
  , EosHdRoot(..)
  , EosHdAccountId(..)
  , EosHdAccount(..)
  , EosHdAddressId(..)
  , EosHdAddress(..)
    -- ** Initialiser
  , initEosHdWallets
    -- ** Lenses
  , eosHdWalletsRoots
  , eosHdWalletsAccounts
  , eosHdWalletsAddresses
  , eosHdRootId
  , eosHdRootName
  , eosHdRootAssurance
  , eosHdRootAddressPoolGap
  , eosHdAccountId
  , eosHdAccountPK
  , eosHdAccountRootId
  , eosHdAddressId
  , eosHdAddressAccountId
  , eosHdAddressRootId
    -- *** EOS Account ID
  , eosHdAccountIdParent
  , eosHdAccountIdIx
    -- *** EOS Address ID
  , eosHdAddressIdIx
  -- * Zoom to parts of the EOS HD wallet
  , zoomEosHdRootId
  , zoomEosHdAccountId
  , zoomEosHdAddressId
    -- * Zoom variations that create on request
  , zoomOrCreateEosHdAccount
  , assumeEosHdRootExists
  , embedUnknownEosHdRoot
  , embedUnknownEosHdAccount
    -- ** Auxiliary functions
  , genEosHdRootId
  , eosHdRootIdToText
  , decodeEosHdRootId
    -- ** Errors
  , UnknownEosHdRoot(..)
  , UnknownEosHdAccount(..)
  , UnknownEosHdAddress(..)
  ) where

import           Universum hiding ((:|))

import           Control.Lens (at)
import           Control.Lens.TH (makeLenses)
import qualified Data.IxSet.Typed as IxSet (Indexable (..))
import           Data.SafeCopy (base, deriveSafeCopy)
import           Data.UUID (UUID)
import qualified Data.UUID as Uuid
import           Data.UUID.V4 (nextRandom)

import           Formatting (bprint, build, (%))
import qualified Formatting.Buildable

import           Test.QuickCheck (Arbitrary (..), oneof)
import           Test.QuickCheck.Gen (chooseAny)

import qualified Pos.Core as Core
import qualified Pos.Crypto as Core

import           Cardano.Wallet.API.V1.Types (WalAddress (..))
import           Cardano.Wallet.Kernel.AddressPoolGap (AddressPoolGap)
import           Cardano.Wallet.Kernel.DB.HdWallet
import           Cardano.Wallet.Kernel.DB.InDb
import           Cardano.Wallet.Kernel.DB.Util.AcidState
import           Cardano.Wallet.Kernel.DB.Util.IxSet
import qualified Cardano.Wallet.Kernel.DB.Util.IxSet as IxSet hiding (Indexable)

-- | Since externally-owned sequential HD wallet doesn't store root keys,
-- we don't have 'HdRootId' for it. So currently we use UUID to identity EOS-wallet.
--
-- NOTE: UUID-based solution may change in the future.
newtype EosHdRootId = EosHdRootId UUID
    deriving (Eq, Ord)

instance Buildable EosHdRootId where
    build (EosHdRootId uuid) = bprint ("EosHdRootId " % build) $ Uuid.toText uuid

instance Arbitrary EosHdRootId where
    arbitrary = EosHdRootId <$> chooseAny

-- | Generator for new 'EosHdRootId'.
genEosHdRootId :: IO EosHdRootId
genEosHdRootId = EosHdRootId <$> nextRandom

eosHdRootIdToText :: EosHdRootId -> Text
eosHdRootIdToText (EosHdRootId uuid) = Uuid.toText uuid

decodeEosHdRootId :: Text -> Either Text EosHdRootId
decodeEosHdRootId rawUuid = case Uuid.fromText rawUuid of
    Nothing   -> Left "decodeEosHdRootId: invalid EOS-wallet id (not a UUID)."
    Just uuid -> Right . EosHdRootId $ uuid

-- | For 'acid-state'. We define it here to hide 'UUID' in this module only.
deriveSafeCopy 1 'base ''UUID
deriveSafeCopy 1 'base ''EosHdRootId

-- | Root of an externally-owned sequential HD wallet.
--
-- The wallet has sequentially assigned account indices and
-- sequentially assigned address indices.
data EosHdRoot = EosHdRoot {
      -- | Wallet ID
      _eosHdRootId             :: !EosHdRootId
      -- | Wallet name
    , _eosHdRootName           :: !WalletName
      -- | Assurance level
    , _eosHdRootAssurance      :: !AssuranceLevel
      -- | Address pool gap for this wallet
    , _eosHdRootAddressPoolGap :: !AddressPoolGap
    } deriving Eq

-- | Externally-owned sequential HD wallet account ID
data EosHdAccountId = EosHdAccountId {
      _eosHdAccountIdParent :: !EosHdRootId
    , _eosHdAccountIdIx     :: !HdAccountIx
    } deriving Eq

instance Buildable EosHdAccountId where
    build EosHdAccountId{..} = bprint
      ( "EosHdAccountId "
      % "{ parent " % build
      % ", ix     " % build
      % "}"
      )
      _eosHdAccountIdParent
      _eosHdAccountIdIx

instance Ord EosHdAccountId where
    compare a b =
           compare (_eosHdAccountIdIx     a) (_eosHdAccountIdIx     b)
        <> compare (_eosHdAccountIdParent a) (_eosHdAccountIdParent b)

instance Arbitrary EosHdAccountId where
    arbitrary = EosHdAccountId
        <$> arbitrary
        <*> arbitrary

-- | Account in externally-owned sequential HD wallet.
-- Since we receive account's public key during wallet creation,
-- we don't need any additional id or name to identify this account.
data EosHdAccount = EosHdAccount {
      -- | Account index
      _eosHdAccountId :: !EosHdAccountId
      -- | Account's public key.
    , _eosHdAccountPK :: !Core.PublicKey
    }

-- | Externally-owned sequential HD wallet address ID
data EosHdAddressId = EosHdAddressId {
      _eosHdAddressIdParent :: !EosHdAccountId
    , _eosHdAddressIdIx     :: !HdAddressIx
    } deriving Eq

instance Buildable EosHdAddressId where
    build EosHdAddressId{..} = bprint
      ( "EosHdAddressId "
      % " parent " % build
      % " ix     " % build
      % "}"
      )
      _eosHdAddressIdParent
      _eosHdAddressIdIx

instance Ord EosHdAddressId where
    compare a b =
           compare (_eosHdAddressIdIx     a) (_eosHdAddressIdIx     b)
        <> compare (_eosHdAddressIdParent a) (_eosHdAddressIdParent b)

instance Arbitrary EosHdAddressId where
    arbitrary = EosHdAddressId
        <$> arbitrary
        <*> arbitrary

-- | Address in an account of externally-owned sequential HD wallet.
data EosHdAddress = EosHdAddress {
      -- | Address ID
      _eosHdAddressId      :: !EosHdAddressId
      -- | The actual address
    , _eosHdAddressAddress :: !(InDb Core.Address)
    }

-- | All wallets, accounts and addresses in the EOS HD wallets.
data EosHdWallets = EosHdWallets {
    _eosHdWalletsRoots     :: !(IxSet EosHdRoot)
  , _eosHdWalletsAccounts  :: !(IxSet EosHdAccount)
  , _eosHdWalletsAddresses :: !(IxSet EosHdAddress)
  }

initEosHdWallets :: EosHdWallets
initEosHdWallets = EosHdWallets IxSet.empty IxSet.empty IxSet.empty

{-------------------------------------------------------------------------------
  Template Haskell splices
-------------------------------------------------------------------------------}

makeLenses ''EosHdRoot
makeLenses ''EosHdAccountId
makeLenses ''EosHdAccount
makeLenses ''EosHdAddressId
makeLenses ''EosHdAddress
makeLenses ''EosHdWallets

deriveSafeCopy 1 'base ''AddressPoolGap
deriveSafeCopy 1 'base ''EosHdRoot
deriveSafeCopy 1 'base ''EosHdAccountId
deriveSafeCopy 1 'base ''EosHdAccount
deriveSafeCopy 1 'base ''EosHdAddressId
deriveSafeCopy 1 'base ''EosHdAddress
deriveSafeCopy 1 'base ''EosHdWallets

{-------------------------------------------------------------------------------
  Derived lenses
-------------------------------------------------------------------------------}

eosHdAccountRootId :: Lens' EosHdAccount EosHdRootId
eosHdAccountRootId = eosHdAccountId . eosHdAccountIdParent

eosHdAddressAccountId :: Lens' EosHdAddress EosHdAccountId
eosHdAddressAccountId = eosHdAddressId . eosHdAddressIdParent

eosHdAddressRootId :: Lens' EosHdAddress EosHdRootId
eosHdAddressRootId = eosHdAddressAccountId . eosHdAccountIdParent

{-------------------------------------------------------------------------------
  IxSet instantiations
-------------------------------------------------------------------------------}

instance HasPrimKey EosHdRoot where
    type PrimKey EosHdRoot = EosHdRootId
    primKey = _eosHdRootId

instance HasPrimKey EosHdAccount where
    type PrimKey EosHdAccount = EosHdAccountId
    primKey = _eosHdAccountId

instance HasPrimKey EosHdAddress where
    type PrimKey EosHdAddress = EosHdAddressId
    primKey = _eosHdAddressId

type SecondaryEosHdRootIxs    = '[]
type SecondaryEosHdAccountIxs = '[EosHdRootId]
type SecondaryEosHdAddressIxs = '[EosHdRootId, EosHdAccountId, WalAddress]

type instance IndicesOf EosHdRoot    = SecondaryEosHdRootIxs
type instance IndicesOf EosHdAccount = SecondaryEosHdAccountIxs
type instance IndicesOf EosHdAddress = SecondaryEosHdAddressIxs

instance IxSet.Indexable (EosHdRootId ': SecondaryEosHdRootIxs)
                         (OrdByPrimKey EosHdRoot) where
    indices = ixList

instance IxSet.Indexable (EosHdAccountId ': SecondaryEosHdAccountIxs)
                         (OrdByPrimKey EosHdAccount) where
    indices = ixList
                (ixFun ((:[]) . view eosHdAccountRootId))

instance IxSet.Indexable (EosHdAddressId ': SecondaryEosHdAddressIxs)
                         (OrdByPrimKey EosHdAddress) where
    indices = ixList
                (ixFun ((:[]) . view eosHdAddressRootId))
                (ixFun ((:[]) . view eosHdAddressAccountId))
                (ixFun ((:[]) . WalAddress . view (eosHdAddressAddress . fromDb)))

{-------------------------------------------------------------------------------
  Zoom to existing parts of an EOS HD wallet
-------------------------------------------------------------------------------}

zoomEosHdRootId :: forall f e a. CanZoom f
                => (UnknownEosHdRoot -> e)
                -> EosHdRootId
                -> f e EosHdRoot a -> f e EosHdWallets a
zoomEosHdRootId embedErr rootId =
    zoomDef err (eosHdWalletsRoots . at rootId)
  where
    err :: f e EosHdWallets a
    err = missing $ embedErr (UnknownEosHdRoot rootId)

zoomEosHdAccountId :: forall f e a. CanZoom f
                   => (UnknownEosHdAccount -> e)
                   -> EosHdAccountId
                   -> f e EosHdAccount a -> f e EosHdWallets a
zoomEosHdAccountId embedErr accId =
    zoomDef err (eosHdWalletsAccounts . at accId)
  where
    err :: f e EosHdWallets a
    err = zoomEosHdRootId embedErr' (accId ^. eosHdAccountIdParent) $
            missing $ embedErr (UnknownEosHdAccount accId)

    embedErr' :: UnknownEosHdRoot -> e
    embedErr' = embedErr . embedUnknownEosHdRoot

zoomEosHdAddressId :: forall f e a. CanZoom f
                   => (UnknownEosHdAddress -> e)
                   -> EosHdAddressId
                   -> f e EosHdAddress a -> f e EosHdWallets a
zoomEosHdAddressId embedErr addrId =
    zoomDef err (eosHdWalletsAddresses . at addrId)
  where
    err :: f e EosHdWallets a
    err = zoomEosHdAccountId embedErr' (addrId ^. eosHdAddressIdParent) $
            missing $ embedErr (UnknownEosHdAddress addrId)

    embedErr' :: UnknownEosHdAccount -> e
    embedErr' = embedErr . embedUnknownEosHdAccount

{-------------------------------------------------------------------------------
  Unknown identifiers
-------------------------------------------------------------------------------}

-- | Unknown EOS root
data UnknownEosHdRoot =
    -- | Unknown root ID for EOS-wallet
    UnknownEosHdRoot EosHdRootId
    deriving Eq

instance Arbitrary UnknownEosHdRoot where
    arbitrary = oneof
        [ UnknownEosHdRoot <$> arbitrary
        ]

-- | Unknown account in EOS-wallet
data UnknownEosHdAccount =
    -- | Unknown root ID for EOS-wallet
    UnknownEosHdAccountRoot EosHdRootId

    -- | Unknown account (implies the root is known)
  | UnknownEosHdAccount EosHdAccountId
  deriving Eq

instance Arbitrary UnknownEosHdAccount where
    arbitrary = oneof
        [ UnknownEosHdAccountRoot <$> arbitrary
        , UnknownEosHdAccount <$> arbitrary
        ]

-- | Unknown address in account in EOS-wallet
data UnknownEosHdAddress =
    -- | Unknown root ID for EOS-wallet
    UnknownEosHdAddressRoot EosHdRootId

    -- | Unknown account (implies the root is known)
  | UnknownEosHdAddressAccount EosHdAccountId

    -- | Unknown address (implies the account is known)
  | UnknownEosHdAddress EosHdAddressId
  deriving Eq

instance Arbitrary UnknownEosHdAddress where
    arbitrary = oneof
        [ UnknownEosHdAddressRoot <$> arbitrary
        , UnknownEosHdAddressAccount <$> arbitrary
        , UnknownEosHdAddress <$> arbitrary
        ]

embedUnknownEosHdRoot :: UnknownEosHdRoot -> UnknownEosHdAccount
embedUnknownEosHdRoot = go
  where
    go (UnknownEosHdRoot rootId) = UnknownEosHdAccountRoot rootId

embedUnknownEosHdAccount :: UnknownEosHdAccount -> UnknownEosHdAddress
embedUnknownEosHdAccount = go
  where
    go (UnknownEosHdAccountRoot rootId) = UnknownEosHdAddressRoot rootId
    go (UnknownEosHdAccount accountId)  = UnknownEosHdAddressAccount accountId

deriveSafeCopy 1 'base ''UnknownEosHdRoot
deriveSafeCopy 1 'base ''UnknownEosHdAccount
deriveSafeCopy 1 'base ''UnknownEosHdAddress

{-------------------------------------------------------------------------------
  Zoom to parts of the EOS-wallet, creating them if they don't exist
-------------------------------------------------------------------------------}

-- | Creates the 'EosHdAccount' if it doesn't exist
--
-- Precondition: @newEosAccount ^. eosHdAccountId == accountId@
zoomOrCreateEosHdAccount :: (EosHdRootId -> Update' e EosHdWallets ())
                         -> EosHdAccount
                         -> EosHdAccountId
                         -> Update' e EosHdAccount a
                         -> Update' e EosHdWallets a
zoomOrCreateEosHdAccount checkEosRootExists newEosAccount accId upd = do
    checkEosRootExists $ newEosAccount ^. eosHdAccountRootId
    zoomCreate (return newEosAccount) (eosHdWalletsAccounts . at accId) $ upd

-- | Assume that the given EosHdRoot exists
--
-- Helper function which can be used as an argument to 'zoomOrCreateEosHdAccount'
assumeEosHdRootExists :: EosHdRootId -> Update' e EosHdWallets ()
assumeEosHdRootExists _id = return ()
