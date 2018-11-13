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
  , EosHdRootId(..)
  , EosHdRoot(..)
  , EosHdAccount(..)
    -- ** Initialiser
  , initEosHdWallets
    -- ** Lenses
  , eosHdWalletsRoots
  , eosHdWalletsAccounts
  , eosHdRootId
  , eosHdRootName
  , eosHdRootAssurance
  , eosHdRootAddressPoolGap
  , eosHdAccountPK
  , eosHdAccountRootId
  -- * Zoom to parts of the EOS HD wallet
  , zoomEosHdRootId
    -- * Zoom variations that create on request
  , zoomOrCreateEosHdAccount
  , assumeEosHdRootExists
  , embedUnknownEosHdRoot
    -- ** Errors
  , UnknownEosHdRoot(..)
  , UnknownEosHdAccount(..)
  ) where

import           Universum hiding ((:|))

import           Control.Lens (at)
import           Control.Lens.TH (makeLenses)
import qualified Data.IxSet.Typed as IxSet (Indexable (..))
import           Data.SafeCopy (base, deriveSafeCopy)
import           Data.UUID (UUID)
import qualified Data.UUID as Uuid
import           Formatting (bprint, build, (%))
import qualified Formatting.Buildable

import           Test.QuickCheck (Arbitrary (..), oneof)
import           Test.QuickCheck.Gen (chooseAny)

import qualified Pos.Crypto as Core

import           Cardano.Wallet.Kernel.DB.HdWallet
import           Cardano.Wallet.Kernel.DB.Util.AcidState
import           Cardano.Wallet.Kernel.DB.Util.IxSet
import qualified Cardano.Wallet.Kernel.DB.Util.IxSet as IxSet hiding (Indexable)

-- | Currently, during EOS-wallet creation we do not receive
-- root public key of this wallet, but only the list of accounts'
-- public keys. So we cannot create 'HdRootId'. Since we need some
-- unique identifier for EOS-wallet, we use UUID.
-- Please note that UUID-based solution may be changed in the future.
newtype EosHdRootId = EosHdRootId { getEosHdRootId :: UUID }
    deriving (Eq, Ord, Show)

instance Arbitrary EosHdRootId where
    arbitrary = EosHdRootId <$> chooseAny

instance Buildable EosHdRootId where
    build (EosHdRootId uuid) = bprint ("EosHdRootId " % build) (Uuid.toText uuid)

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
    , _eosHdRootAddressPoolGap :: !Word
    } deriving (Eq, Show)

-- | Account in externally-owned sequential HD wallet.
-- Since we receive account's public key during wallet creation,
-- we don't need any additional id or name to identify this account.
data EosHdAccount = EosHdAccount {
      -- | Account's public key.
      _eosHdAccountPK     :: !Core.PublicKey
      -- | Id of EOS-wallet this account belongs to.
    , _eosHdAccountRootId :: !EosHdRootId
    }

-- | All wallets and accounts in the EOS HD wallets
data EosHdWallets = EosHdWallets {
    _eosHdWalletsRoots    :: !(IxSet EosHdRoot)
  , _eosHdWalletsAccounts :: !(IxSet EosHdAccount)
  }

initEosHdWallets :: EosHdWallets
initEosHdWallets = EosHdWallets IxSet.empty IxSet.empty

{-------------------------------------------------------------------------------
  Template Haskell splices
-------------------------------------------------------------------------------}

makeLenses ''EosHdRoot
makeLenses ''EosHdAccount
makeLenses ''EosHdWallets

deriveSafeCopy 1 'base ''UUID
deriveSafeCopy 1 'base ''EosHdRootId
deriveSafeCopy 1 'base ''EosHdRoot
deriveSafeCopy 1 'base ''EosHdAccount
deriveSafeCopy 1 'base ''EosHdWallets

{-------------------------------------------------------------------------------
  IxSet instantiations
-------------------------------------------------------------------------------}

instance HasPrimKey EosHdRoot where
    type PrimKey EosHdRoot = EosHdRootId
    primKey = _eosHdRootId

instance HasPrimKey EosHdAccount where
    type PrimKey EosHdAccount = Core.PublicKey
    primKey = _eosHdAccountPK

type SecondaryEosHdRootIxs    = '[]
type SecondaryEosHdAccountIxs = '[EosHdRootId]

type instance IndicesOf EosHdRoot    = SecondaryEosHdRootIxs
type instance IndicesOf EosHdAccount = SecondaryEosHdAccountIxs

instance IxSet.Indexable (EosHdRootId ': SecondaryEosHdRootIxs)
                         (OrdByPrimKey EosHdRoot) where
    indices = ixList

instance IxSet.Indexable (Core.PublicKey ': SecondaryEosHdAccountIxs)
                         (OrdByPrimKey EosHdAccount) where
    indices = ixList
                (ixFun ((:[]) . view eosHdAccountRootId))

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

{-------------------------------------------------------------------------------
  Unknown identifiers
-------------------------------------------------------------------------------}

-- | Unknown EOS root
data UnknownEosHdRoot =
    -- | Unknown root ID for EOS-wallet
    UnknownEosHdRoot EosHdRootId
    deriving Eq

instance Arbitrary UnknownEosHdRoot where
    arbitrary = oneof [ UnknownEosHdRoot <$> arbitrary
                      ]

-- | Unknown account in EOS-wallet
data UnknownEosHdAccount =
    -- | Unknown root ID for EOS-wallet
    UnknownEosHdAccountRoot EosHdRootId

    -- | Unknown account (implies the root is known)
  | UnknownEosHdAccount HdAccountId
  deriving Eq

instance Arbitrary UnknownEosHdAccount where
    arbitrary = oneof [ UnknownEosHdAccountRoot <$> arbitrary
                      , UnknownEosHdAccount <$> arbitrary
                      ]

embedUnknownEosHdRoot :: UnknownEosHdRoot -> UnknownEosHdAccount
embedUnknownEosHdRoot = go
  where
    go (UnknownEosHdRoot rootId) = UnknownEosHdAccountRoot rootId

deriveSafeCopy 1 'base ''UnknownEosHdRoot
deriveSafeCopy 1 'base ''UnknownEosHdAccount

{-------------------------------------------------------------------------------
  Zoom to parts of the EOS-wallet, creating them if they don't exist
-------------------------------------------------------------------------------}

-- | Creates the 'EosHdAccount' if it doesn't exist
--
-- Precondition: @newEosAccount ^. eosHdAccountId == accountId@
zoomOrCreateEosHdAccount :: (EosHdRootId -> Update' e EosHdWallets ())
                         -> EosHdAccount
                         -> Core.PublicKey
                         -> Update' e EosHdAccount a
                         -> Update' e EosHdWallets a
zoomOrCreateEosHdAccount checkEosRootExists newEosAccount accPK upd = do
    checkEosRootExists $ newEosAccount ^. eosHdAccountRootId
    zoomCreate (return newEosAccount) (eosHdWalletsAccounts . at accPK) $ upd

-- | Assume that the given EosHdRoot exists
--
-- Helper function which can be used as an argument to 'zoomOrCreateEosHdAccount'
assumeEosHdRootExists :: EosHdRootId -> Update' e EosHdWallets ()
assumeEosHdRootExists _id = return ()
