{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE RankNTypes                 #-}

-- TODO: Not sure about the best way to avoid the orphan instances here
{-# OPTIONS_GHC -fno-warn-orphans -Wno-redundant-constraints #-}

-- | HD wallets
module Cardano.Wallet.Kernel.DB.HdWallet (
    -- * Supporting types
    WalletName(..)
  , AccountName(..)
  , HdAccountIx(..)
  , HdAddressIx(..)
  , AssuranceLevel(..)
  , assuredBlockDepth
  , HasSpendingPassword(..)
    -- * HD wallet types proper
  , HdWallets(..)
  , HdAccountId(..)
  , HdAddressId(..)
  , HdRoot(..)
  , HdRootBase(..)
  , HdAccount(..)
  , HdAddress(..)
    -- * HD Wallet state
  , HdAccountState(..)
  , HdAccountUpToDate(..)
  , HdAccountIncomplete(..)
  , WithAccountState(..)
  , CombinedWithAccountState
  , finishRestoration
    -- ** Initialiser
  , initHdWallets
  , initHdAddress
    -- ** Lenses
    -- *** Wallet collection
  , hdWalletsRoots
  , hdWalletsAccounts
  , hdWalletsAddresses
    -- *** Account ID
  , hdAccountIdParent
  , hdAccountIdIx
  , hdAccountAutoPkCounter
    -- ** Address ID
  , hdAddressIdParent
  , hdAddressIdIx
    -- *** Root
  , hdRootId
  , hdRootBase
  , hdRootName
  , hdRootHasPassword
  , hdRootAssurance
  , hdRootCreatedAt
    -- *** Account
  , hdAccountId
  , hdAccountName
  , hdAccountState
  , hdAccountStateCurrent
  , hdAccountStateCurrentCombined
  , hdAccountStateUpToDate
  , hdAccountRestorationState
    -- *** Account state: up to date
  , hdUpToDateCheckpoints
    -- *** Account state: under restoration
  , hdIncompleteCurrent
  , hdIncompleteHistorical
    -- *** Address
  , hdAddressId
  , hdAddressAddress
    -- ** Composite lenses
  , hdAccountRootId
  , hdAddressRootId
  , hdAddressAccountId
    -- * Unknown identifiers
  , UnknownHdRoot(..)
  , UnknownHdAccount(..)
  , UnknownHdAddress(..)
  , embedUnknownHdRoot
  , embedUnknownHdAccount
    -- * Zoom to parts of the HD wallet
  , zoomHdRootId
  , zoomHdAccountId
  , zoomHdAddressId
  , zoomHdCardanoAddress
  , matchHdAccountState
  , zoomHdAccountCheckpoints
  , zoomHdAccountCurrent
  , zoomHdAccountCurrentCombine
  , matchHdAccountCheckpoints
    -- * Zoom variations that create on request
  , zoomOrCreateHdRoot
  , zoomOrCreateHdAccount
  , zoomOrCreateHdAddress
  , assumeHdRootExists
  , assumeHdAccountExists
    -- * IsOurs
  , IsOurs(..)
  ) where

import           Universum hiding ((:|))

import           Control.Lens (Getter, at, lazy, lens, to, (+~), (?~), _Wrapped)
import           Control.Lens.TH (makeLenses)
import qualified Data.IxSet.Typed as IxSet (Indexable (..))
import qualified Data.Map as Map
import           Data.SafeCopy (base, deriveSafeCopy)

import           Test.QuickCheck (Arbitrary (..), oneof)

import           Formatting (bprint, build, (%))
import qualified Formatting.Buildable
import           Serokell.Util (listJson)

import qualified Pos.Core as Core
import           Pos.Core.Chrono (NewestFirst (..))
import           Pos.Crypto (HDPassphrase)
import qualified Pos.Crypto as Core

import           Cardano.Wallet.API.V1.Types (WalAddress (..))
import           Cardano.Wallet.Kernel.AddressPool (AddressPool,
                     lookupAddressPool)
import           Cardano.Wallet.Kernel.AddressPoolGap (AddressPoolGap)
import           Cardano.Wallet.Kernel.DB.BlockContext
import           Cardano.Wallet.Kernel.DB.HdRootId (HdRootId)
import           Cardano.Wallet.Kernel.DB.InDb
import           Cardano.Wallet.Kernel.DB.Spec
import           Cardano.Wallet.Kernel.DB.Util.AcidState
import           Cardano.Wallet.Kernel.DB.Util.IxSet hiding (foldl')
import qualified Cardano.Wallet.Kernel.DB.Util.IxSet as IxSet hiding (Indexable)
import qualified Cardano.Wallet.Kernel.DB.Util.Zoomable as Z
import           Cardano.Wallet.Kernel.NodeStateAdaptor (SecurityParameter (..))
import qualified Cardano.Wallet.Kernel.Util.StrictList as SL
import           Cardano.Wallet.Kernel.Util.StrictNonEmpty (StrictNonEmpty (..))
import qualified Cardano.Wallet.Kernel.Util.StrictNonEmpty as SNE
import           Cardano.Wallet.Util (buildTrunc)
import           UTxO.Util (liftNewestFirst, modifyAndGetOld)

{-------------------------------------------------------------------------------
  Supporting types
-------------------------------------------------------------------------------}

-- | Wallet name
newtype WalletName = WalletName { getWalletName :: Text }
    deriving (Eq, Show, Arbitrary)

-- | Account name
newtype AccountName = AccountName { getAccountName :: Text }

-- | Account index
newtype HdAccountIx = HdAccountIx { getHdAccountIx :: Word32 }
  deriving (Eq, Ord, Generic)

instance NFData HdAccountIx

-- NOTE(adn) if we need to generate only @hardened@ account indexes, we
-- need to extend this arbitrary instance accordingly.
instance Arbitrary HdAccountIx where
    arbitrary = HdAccountIx <$> arbitrary

-- | Address index
newtype HdAddressIx = HdAddressIx { getHdAddressIx :: Word32 }
  deriving (Eq, Ord)

instance Arbitrary HdAddressIx where
    arbitrary = HdAddressIx <$> arbitrary

-- | Wallet assurance level
data AssuranceLevel =
    AssuranceLevelNormal
  | AssuranceLevelStrict
  deriving (Show, Eq)

instance Arbitrary AssuranceLevel where
    arbitrary = oneof [ pure AssuranceLevelNormal
                      , pure AssuranceLevelStrict
                      ]

-- | Interpretation of 'AssuranceLevel'
--
-- This is adopted from the legacy wallet, which claims that these values
-- are taken from <https://cardanodocs.com/cardano/transaction-assurance/>
assuredBlockDepth :: AssuranceLevel -> Core.BlockCount
assuredBlockDepth AssuranceLevelNormal = 9
assuredBlockDepth AssuranceLevelStrict = 15

-- | Does this wallet have a spending password
data HasSpendingPassword =
    -- | No spending password set. We need a timestamp here to record
    -- when the password was removed (i.e. password was replaced by
    -- an empty string). By default we store wallet creation time here.
    NoSpendingPassword !(InDb Core.Timestamp)

    -- | If there is a spending password, we record when it was last updated.
  | HasSpendingPassword !(InDb Core.Timestamp)
  deriving (Show, Eq)

instance Arbitrary HasSpendingPassword where
    arbitrary = oneof [ NoSpendingPassword . InDb <$> arbitrary
                      , HasSpendingPassword . InDb <$> arbitrary
                      ]

deriveSafeCopy 1 'base ''WalletName
deriveSafeCopy 1 'base ''AccountName
deriveSafeCopy 1 'base ''HdAccountIx
deriveSafeCopy 1 'base ''HdAddressIx
deriveSafeCopy 1 'base ''AssuranceLevel
deriveSafeCopy 1 'base ''HasSpendingPassword

{-------------------------------------------------------------------------------
  General-utility functions
-------------------------------------------------------------------------------}

eskToHdPassphrase :: Core.EncryptedSecretKey -> HDPassphrase
eskToHdPassphrase = Core.deriveHDPassphrase . Core.encToPublic

{-------------------------------------------------------------------------------
  HD wallets
-------------------------------------------------------------------------------}

-- | HD wallet account ID
data HdAccountId = HdAccountId {
      _hdAccountIdParent :: !HdRootId
    , _hdAccountIdIx     :: !HdAccountIx
    }
  deriving (Eq, Generic)

instance NFData HdAccountId

-- | We make sure to compare the account index first to avoid doing an
-- unnecessary comparison of the root ID
instance Ord HdAccountId where
  compare a b =
       compare (_hdAccountIdIx     a) (_hdAccountIdIx     b)
    <> compare (_hdAccountIdParent a) (_hdAccountIdParent b)

instance Arbitrary HdAccountId where
  arbitrary = HdAccountId <$> arbitrary <*> arbitrary

-- | HD wallet address ID
data HdAddressId = HdAddressId {
      _hdAddressIdParent :: !HdAccountId
    , _hdAddressIdIx     :: !HdAddressIx
    }
  deriving Eq

-- | We make sure to compare the address index first to avoid doing an
-- unnecessary comparison of the account ID
instance Ord HdAddressId where
  compare a b =
       compare (_hdAddressIdIx     a) (_hdAddressIdIx     b)
    <> compare (_hdAddressIdParent a) (_hdAddressIdParent b)

instance Arbitrary HdAddressId where
  arbitrary = HdAddressId <$> arbitrary <*> arbitrary

-- | Root of a HD wallet
--
-- The wallet has sequentially assigned account indices and randomly assigned
-- address indices.
--
-- NOTE: We do not store the encrypted key of the wallet.
data HdRoot = HdRoot {
      -- | A root that is either fully owned or externally owned.
      -- Externally owned root have a few more details attached to them.
      _hdRootBase        :: !HdRootBase

      -- | Wallet name
    , _hdRootName        :: !WalletName

      -- | Does this wallet have a spending password?
      --
      -- NOTE: We do not store the spending password itself, but merely record
      -- whether there is one. Updates to the spending password affect only the
      -- external key storage, not the wallet DB proper.
    , _hdRootHasPassword :: !HasSpendingPassword

      -- | Assurance level
    , _hdRootAssurance   :: !AssuranceLevel

      -- | When was this wallet created?
    , _hdRootCreatedAt   :: !(InDb Core.Timestamp)
    } deriving (Eq)

instance Arbitrary HdRoot where
    arbitrary = HdRoot <$> arbitrary
                       <*> arbitrary
                       <*> arbitrary
                       <*> arbitrary
                       <*> (fmap InDb arbitrary)

data HdRootBase
    = HdRootFullyOwned HdRootId
    | HdRootExternallyOwned
        { _hdRootBaseId
            :: !HdRootId
        , _hdRootBaseGap
            :: !AddressPoolGap
        , _hdRootBaseAccounts
            :: Map HdAccountId Core.PublicKey
        }
    deriving (Eq, Ord)

instance Arbitrary HdRootBase where
    arbitrary = oneof
        [ HdRootFullyOwned <$> arbitrary
        , HdRootExternallyOwned <$> arbitrary <*> arbitrary <*> arbitrary
        ]

-- Key derivation is cheap
data HdAccount = HdAccount {
      -- | Account index
      _hdAccountId            :: !HdAccountId

      -- | Account name
    , _hdAccountName          :: !AccountName

      -- | Account state
      --
      -- When the account is up to date with the blockchain, the account state
      -- coincides with the state of a " wallet " as mandated by the formal
      -- spec.
    , _hdAccountState         :: !HdAccountState

      -- | A local counter used to generate new 'AutoIncrementKey' for
      -- addresses.
    , _hdAccountAutoPkCounter :: !AutoIncrementKey
    }

-- | Address in an account of a HD wallet
data HdAddress = HdAddress {
      -- | Address ID
      _hdAddressId      :: !HdAddressId

      -- | The actual address
    , _hdAddressAddress :: !(InDb Core.Address)
    } deriving (Eq, Ord)

-- | New address in the specified account
--
-- Since the DB does not contain the private key of the wallet, we cannot
-- do the actual address derivation here; this will be the responsibility of
-- the caller (which will require the use of the spending password, if
-- one exists).
--
-- Similarly, it will be the responsibility of the caller to pick a random
-- address index, as we do not have access to a random number generator here.
initHdAddress :: HdAddressId
              -> Core.Address
              -> HdAddress
initHdAddress addrId address = HdAddress {
      _hdAddressId      = addrId
    , _hdAddressAddress = InDb address
    }

{-------------------------------------------------------------------------------
  Account state
-------------------------------------------------------------------------------}

-- | Account state (essentially, how much historical data do we have?)
data HdAccountState =
      HdAccountStateUpToDate   !HdAccountUpToDate
    | HdAccountStateIncomplete !HdAccountIncomplete

-- | Account state for an account which has complete historical data
data HdAccountUpToDate = HdAccountUpToDate {
      _hdUpToDateCheckpoints :: !(Checkpoints Checkpoint)
    }

-- | Account state for an account which is lacking some historical checkpoints
-- (which is currently being restored)
data HdAccountIncomplete = HdAccountIncomplete {
      -- | Current checkpoints
      --
      -- During wallet restoration we always track the underlying node, but may
      -- lack historical checkpoints. We synchronously construct a partial
      -- checkpoint for the current tip, and then as we get new blocks from
      -- the BListener, we add new partial checkpoints.
      _hdIncompleteCurrent    :: !(Checkpoints PartialCheckpoint)

      -- | Historical full checkpoints
      --
      -- Meanwhile, we asynchronously construct full checkpoints, starting
      -- from genesis. Once this gets to within k slots of the tip, we start
      -- keeping all of these.
    , _hdIncompleteHistorical :: !(Checkpoints Checkpoint)
    }

makeLenses ''HdAccountUpToDate
makeLenses ''HdAccountIncomplete

data WithAccountState a b = UpToDate a | Incomplete b

type CombinedWithAccountState a = WithAccountState a a


-- | Restoration is complete when we have all historical checkpoints
--
-- NOTE:
--
-- * The local block metadata in the partial checkpoints /already/
--   accumulates (the local block metadata in the next partial checkpoint
--   includes the local block metadata in the previous). Therefore we get the
--   most recent /full/ checkpoint, and use that as the basis for constructing
--   full block metadata for /all/ partial checkpoints.
--
-- * We do NOT use the oldest partial checkpoint, since it will have its
--   UTxO set from the underlying node's UTxO rather than from a block, and
--   will therefore not have valid block metadata associated with it.
--   (It is also possible that the initial checkpoint was created later, with
--   an empty UTxO, if we discover it /during/ restoration).
--
-- * We verify that the second-oldest partial checkpoint's previous pointer (if
--   one exists) lines up with the most recent historical checkpoint.
finishRestoration :: SecurityParameter
                  -> HdAccountIncomplete
                  -> HdAccountUpToDate
finishRestoration (SecurityParameter k) (HdAccountIncomplete (Checkpoints partial) (Checkpoints historical)) =
    case SL.last initPartial of
      Nothing ->
        HdAccountUpToDate $ Checkpoints $ takeNewest k $ NewestFirst $
          (mostRecentHistorical :| olderHistorical)
      Just secondLast -> let checkpoint = secondLast ^. pcheckpointContext in
        if checkpoint `blockContextSucceeds` (mostRecentHistorical ^. checkpointContext . lazy)
          then HdAccountUpToDate $ Checkpoints $ takeNewest k $ NewestFirst $
                 SNE.prependList
                   (mkFull <$> initPartial)
                   (mostRecentHistorical :| olderHistorical)
          else error "finishRestoration: checkpoints do not line up!"
  where
    (initPartial, _oldestPartial)           = SNE.splitLast $ getNewestFirst partial
    mostRecentHistorical :| olderHistorical =                 getNewestFirst historical

    mkFull :: PartialCheckpoint -> Checkpoint
    mkFull = toFullCheckpoint (mostRecentHistorical ^. checkpointBlockMeta)

    takeNewest :: Int -> NewestFirst StrictNonEmpty a -> NewestFirst StrictNonEmpty a
    takeNewest = liftNewestFirst . SNE.take

{-------------------------------------------------------------------------------
  Template Haskell splices
-------------------------------------------------------------------------------}

makeLenses ''HdAccountId
makeLenses ''HdAddressId

makeLenses ''HdRoot
makeLenses ''HdAccount
makeLenses ''HdAddress

deriveSafeCopy 1 'base ''HdAccountId
deriveSafeCopy 1 'base ''HdAddressId

deriveSafeCopy 1 'base ''HdRoot
deriveSafeCopy 1 'base ''HdRootBase
deriveSafeCopy 1 'base ''HdAccount
deriveSafeCopy 1 'base ''HdAddress

deriveSafeCopy 1 'base ''HdAccountState
deriveSafeCopy 1 'base ''HdAccountUpToDate
deriveSafeCopy 1 'base ''HdAccountIncomplete

{-------------------------------------------------------------------------------
  Derived lenses
-------------------------------------------------------------------------------}

hdRootId :: Lens' HdRoot HdRootId
hdRootId = hdRootBase . hdRootBaseId

hdRootBaseId :: Lens' HdRootBase HdRootId
hdRootBaseId = lens getter (flip setter)
    where
        getter = \case
            (HdRootFullyOwned rootId)          -> rootId
            (HdRootExternallyOwned rootId _ _) -> rootId
        setter rootId = \case
            (HdRootFullyOwned _)          -> HdRootFullyOwned rootId
            (HdRootExternallyOwned _ a b) -> HdRootExternallyOwned rootId a b


hdAccountRootId :: Lens' HdAccount HdRootId
hdAccountRootId = hdAccountId . hdAccountIdParent

hdAddressAccountId :: Lens' HdAddress HdAccountId
hdAddressAccountId = hdAddressId . hdAddressIdParent

hdAddressRootId :: Lens' HdAddress HdRootId
hdAddressRootId = hdAddressAccountId . hdAccountIdParent

hdAccountStateCurrent :: (forall c. IsCheckpoint c => Getter c a) -> Getter HdAccountState a
hdAccountStateCurrent g = to $ \case
    HdAccountStateUpToDate st ->
      st ^. hdUpToDateCheckpoints . unCheckpoints . _Wrapped . SNE.head . g
    HdAccountStateIncomplete st ->
      st ^. hdIncompleteCurrent   . unCheckpoints . _Wrapped . SNE.head . g

hdAccountStateCurrentCombined :: (a -> a -> a)
                              -> (forall c. IsCheckpoint c => Getter c a)
                              -> Getter HdAccountState a
hdAccountStateCurrentCombined combine g = to $ \case
    HdAccountStateUpToDate st ->
      st ^. hdUpToDateCheckpoints . unCheckpoints . _Wrapped . SNE.head . g
    HdAccountStateIncomplete st ->
      combine (st ^. hdIncompleteCurrent    . unCheckpoints . _Wrapped . SNE.head . g)
              (st ^. hdIncompleteHistorical . unCheckpoints . _Wrapped . SNE.head . g)

{-------------------------------------------------------------------------------
  Predicates and tests
-------------------------------------------------------------------------------}

hdAccountStateUpToDate :: HdAccount -> Bool
hdAccountStateUpToDate a = case a ^. hdAccountState of
    HdAccountStateUpToDate   _ -> True
    HdAccountStateIncomplete _ -> False

hdAccountRestorationState :: HdAccount -> Maybe (Maybe BlockContext, BlockContext)
hdAccountRestorationState a = case a ^. hdAccountState of
    HdAccountStateUpToDate   _                      -> Nothing
    HdAccountStateIncomplete HdAccountIncomplete{..} -> Just $
      ( _hdIncompleteHistorical ^. currentCheckpoint . cpContext . lazy,
        _hdIncompleteCurrent    ^. oldestCheckpoint  . pcheckpointContext)

{-------------------------------------------------------------------------------
  Address relationship: isOurs?
-------------------------------------------------------------------------------}

class IsOurs s where
    -- For the given state, check whether an address is "ours"
    isOurs :: Core.Address -> s -> (Maybe HdAddress, s)

{-------------------------------------------------------------------------------
  isOurs for Hd Random wallets
-------------------------------------------------------------------------------}

-- | NOTE: We could modify the given state here to actually store decrypted
-- addresses in a `Map` to trade a decryption against a map lookup for already
-- decrypted addresses.
instance IsOurs (Map HdRootId Core.EncryptedSecretKey) where
    isOurs addr s = (,s) $ foldl' (<|>) Nothing $ flip Map.mapWithKey s $ \rootId esk -> do
        (accountIx, addressIx) <- decryptHdLvl2DerivationPath (eskToHdPassphrase esk) addr
        let accId = HdAccountId rootId accountIx
        let addrId = HdAddressId accId addressIx
        return $ HdAddress addrId (InDb addr)

decryptHdLvl2DerivationPath
    :: Core.HDPassphrase
    -> Core.Address
    -> Maybe (HdAccountIx, HdAddressIx)
decryptHdLvl2DerivationPath hdPass addr = do
    hdPayload <- Core.aaPkDerivationPath $ Core.addrAttributesUnwrapped addr
    derPath <- Core.unpackHDAddressAttr hdPass hdPayload
    case derPath of
        [a,b] -> Just (HdAccountIx a, HdAddressIx b)
        _     -> Nothing


{-------------------------------------------------------------------------------
  isOurs for Hd Sequential wallets
-------------------------------------------------------------------------------}

-- | Search for an address in a map of AddressPools indexed by HdAccountId.
--  This map represents the State context.
--  If an Address is found, we update the state with a possibly updated AddressPool
--  (since the address pool may have been extended in response to the address discovery)
--  Otherwise if we don't find the address, we return the state unchanged.
instance IsOurs (Map HdAccountId (AddressPool Core.Address)) where
    isOurs addr pools = case addrMatch of
        (Just (hdAddr, pool')) ->
            (Just hdAddr, pools & at (accountId hdAddr) ?~ pool')
        Nothing ->
            (Nothing,pools)
        where
            addrMatch
                = foldl' (<|>) Nothing $ map lookupAddressInPool' (Map.toList pools)

            accountId a = a ^. hdAddressId . hdAddressIdParent

            lookupAddressInPool'
                :: (HdAccountId, AddressPool Core.Address)
                -> Maybe (HdAddress, AddressPool Core.Address)
            lookupAddressInPool' (accId, pool)
                = case lookupAddressPool addr pool of
                    (Nothing, _) -> Nothing
                    (Just (_, ix), pool') -> (Just (mkHdAddress accId ix, pool'))

            mkHdAddress :: HdAccountId -> Word32 -> HdAddress
            mkHdAddress accId_ ix_
                = initHdAddress (HdAddressId accId_ (HdAddressIx ix_)) addr


{-------------------------------------------------------------------------------
  Unknown identifiers
-------------------------------------------------------------------------------}

-- | Unknown root
data UnknownHdRoot =
    -- | Unknown root ID
    UnknownHdRoot HdRootId
    deriving Eq

instance Arbitrary UnknownHdRoot where
    arbitrary = oneof [ UnknownHdRoot <$> arbitrary
                      ]

-- | Unknown account
data UnknownHdAccount =
    -- | Unknown root ID
    UnknownHdAccountRoot HdRootId

    -- | Unknown account (implies the root is known)
  | UnknownHdAccount HdAccountId
  deriving Eq

instance Arbitrary UnknownHdAccount where
    arbitrary = oneof [ UnknownHdAccountRoot <$> arbitrary
                      , UnknownHdAccount <$> arbitrary
                      ]

-- | Unknown address
data UnknownHdAddress =
    -- | Unknown root ID
    UnknownHdAddressRoot HdRootId

    -- | Unknown account (implies the root is known)
  | UnknownHdAddressAccount HdAccountId

    -- | Unknown address (implies the account is known)
  | UnknownHdAddress HdAddressId

    -- | Unknown address (implies it was not derived from the given Address)
  | UnknownHdCardanoAddress Core.Address

instance Arbitrary UnknownHdAddress where
    arbitrary = oneof [ UnknownHdAddressRoot <$> arbitrary
                      , UnknownHdAddressAccount <$> arbitrary
                      , UnknownHdAddress <$> arbitrary
                      , UnknownHdCardanoAddress <$> arbitrary
                      ]

embedUnknownHdRoot :: UnknownHdRoot -> UnknownHdAccount
embedUnknownHdRoot = go
  where
    go (UnknownHdRoot rootId) = UnknownHdAccountRoot rootId

embedUnknownHdAccount :: UnknownHdAccount -> UnknownHdAddress
embedUnknownHdAccount = go
  where
    go (UnknownHdAccountRoot rootId) = UnknownHdAddressRoot rootId
    go (UnknownHdAccount accountId)  = UnknownHdAddressAccount accountId

deriveSafeCopy 1 'base ''UnknownHdRoot
deriveSafeCopy 1 'base ''UnknownHdAddress
deriveSafeCopy 1 'base ''UnknownHdAccount

{-------------------------------------------------------------------------------
  IxSet instantiations
-------------------------------------------------------------------------------}

instance HasPrimKey HdRoot where
    type PrimKey HdRoot = HdRootId
    primKey = view hdRootId

instance HasPrimKey HdAccount where
    type PrimKey HdAccount = HdAccountId
    primKey = view hdAccountId

instance HasPrimKey (Indexed HdAddress) where
    type PrimKey (Indexed HdAddress) = HdAddressId
    primKey = _hdAddressId . _ixedIndexed

type SecondaryHdRootIxs           = '[]
type SecondaryHdAccountIxs        = '[HdRootId]
type SecondaryIndexedHdAddressIxs = '[AutoIncrementKey, HdRootId, HdAccountId, WalAddress]

type instance IndicesOf HdRoot              = SecondaryHdRootIxs
type instance IndicesOf HdAccount           = SecondaryHdAccountIxs
type instance IndicesOf (Indexed HdAddress) = SecondaryIndexedHdAddressIxs

instance IxSet.Indexable (HdRootId ': SecondaryHdRootIxs)
                         (OrdByPrimKey HdRoot) where
    indices = ixList

instance IxSet.Indexable (HdAccountId ': SecondaryHdAccountIxs)
                         (OrdByPrimKey HdAccount) where
    indices = ixList
                (ixFun ((:[]) . view hdAccountRootId))

instance IxSet.Indexable (HdAddressId ': SecondaryIndexedHdAddressIxs)
                         (OrdByPrimKey (Indexed HdAddress)) where
    indices = ixList
                (ixFun ((:[]) . view ixedIndex))
                (ixFun ((:[]) . view (ixedIndexed . hdAddressRootId)))
                (ixFun ((:[]) . view (ixedIndexed . hdAddressAccountId)))
                (ixFun ((:[]) . WalAddress . view (ixedIndexed . hdAddressAddress . fromDb)))

{-------------------------------------------------------------------------------
  Top-level HD wallet structure
-------------------------------------------------------------------------------}

-- | All wallets, accounts and addresses in the HD wallets
--
-- We use a flat "relational" structure rather than nested maps so that we can
-- go from address to wallet just as easily as the other way around.
data HdWallets = HdWallets {
    _hdWalletsRoots     :: !(IxSet HdRoot)
  , _hdWalletsAccounts  :: !(IxSet HdAccount)
  , _hdWalletsAddresses :: !(IxSet (Indexed HdAddress))
  }

deriveSafeCopy 1 'base ''HdWallets
makeLenses ''HdWallets

initHdWallets :: HdWallets
initHdWallets = HdWallets IxSet.empty IxSet.empty IxSet.empty

{-------------------------------------------------------------------------------
  Zoom to existing parts of a HD wallet
-------------------------------------------------------------------------------}

zoomHdRootId :: forall f e a. CanZoom f
             => (UnknownHdRoot -> e)
             -> HdRootId
             -> f e HdRoot a -> f e HdWallets a
zoomHdRootId embedErr rootId =
    zoomDef err (hdWalletsRoots . at rootId)
  where
    err :: f e HdWallets a
    err = missing $ embedErr (UnknownHdRoot rootId)

zoomHdAccountId :: forall f e a. CanZoom f
                => (UnknownHdAccount -> e)
                -> HdAccountId
                -> f e HdAccount a -> f e HdWallets a
zoomHdAccountId embedErr accId =
    zoomDef err (hdWalletsAccounts . at accId)
  where
    err :: f e HdWallets a
    err = zoomHdRootId embedErr' (accId ^. hdAccountIdParent) $
            missing $ embedErr (UnknownHdAccount accId)

    embedErr' :: UnknownHdRoot -> e
    embedErr' = embedErr . embedUnknownHdRoot

zoomHdAddressId :: forall f e a. CanZoom f
                => (UnknownHdAddress -> e)
                -> HdAddressId
                -> f e HdAddress a -> f e HdWallets a
zoomHdAddressId embedErr addrId =
    zoomDef err (hdWalletsAddresses . at addrId) . zoom ixedIndexed
  where
    err :: f e HdWallets a
    err = zoomHdAccountId embedErr' (addrId ^. hdAddressIdParent) $
            missing $ embedErr (UnknownHdAddress addrId)

    embedErr' :: UnknownHdAccount -> e
    embedErr' = embedErr . embedUnknownHdAccount

-- | Zoom to the specified Cardano address
--
-- This is defined on queries only for now. In principle we could define this
-- more generally, but that would require somehow taking advantage of the  fact
-- that there cannot be more than one 'HdAddress' with a given 'Core.Address'.
zoomHdCardanoAddress :: forall e a.
                        (UnknownHdAddress -> e)
                     -> Core.Address
                     -> Query' e HdAddress a -> Query' e HdWallets a
zoomHdCardanoAddress embedErr addr =
    localQuery findAddress
  where
    findAddress :: Query' e HdWallets HdAddress
    findAddress = do
        addresses <- view hdWalletsAddresses
        maybe err return $ (fmap _ixedIndexed $ getOne $ getEQ (WalAddress addr) addresses)

    err :: Query' e HdWallets x
    err = missing $ embedErr (UnknownHdCardanoAddress addr)

-- | Pattern match on the state of the account
matchHdAccountState :: CanZoom f
                    => f e HdAccountUpToDate   a
                    -> f e HdAccountIncomplete a
                    -> f e HdAccount           a
matchHdAccountState updUpToDate updIncomplete = withZoomableConstraints $
    Z.wrap $ \acc ->
      case acc ^. hdAccountState of
        HdAccountStateUpToDate st -> do
          let upd st' = acc & hdAccountState .~ HdAccountStateUpToDate  st'
          second upd $ Z.unwrap updUpToDate st
        HdAccountStateIncomplete st -> do
          let upd st' = acc & hdAccountState .~ HdAccountStateIncomplete st'
          second upd $ Z.unwrap updIncomplete st

-- | Zoom to the current checkpoints of the wallet
zoomHdAccountCheckpoints :: CanZoom f
                         => (   forall c. IsCheckpoint c
                             => f e (Checkpoints c) a )
                         -> f e HdAccount a
zoomHdAccountCheckpoints upd = matchHdAccountCheckpoints upd upd

-- | Zoom to the current checkpoints of the wallet. If the account is under
--   restoration we combine the current historical and partial checkpoint.
zoomHdAccountCheckpointsCombine :: CanZoom f
                                => (a -> a -> a)
                                -> (   forall c. IsCheckpoint c
                                    => f e (Checkpoints c) a )
                                -> f e HdAccount (CombinedWithAccountState a)
zoomHdAccountCheckpointsCombine combine upd =
    matchHdAccountCheckpointsCombine combine upd upd

-- | Zoom to the most recent checkpoint
zoomHdAccountCurrent :: CanZoom f
                     => (forall c. IsCheckpoint c => f e c a)
                     -> f e HdAccount a
zoomHdAccountCurrent upd = withZoomableConstraints $
    zoomHdAccountCheckpoints $
      Z.wrap $ \cps -> do
        let l :: Lens' (Checkpoints c) c
            l = unCheckpoints . _Wrapped . SNE.head
        second (\cp' -> cps & l .~ cp') $ Z.unwrap upd (cps ^. l)

-- | Zoom to the most recent checkpoint. If the account is under
--   restoration we combine the current historical and partial checkpoint.
zoomHdAccountCurrentCombine :: CanZoom f
                          => (a -> a -> a)
                          -> (forall c. IsCheckpoint c => f e c a)
                          -> f e HdAccount (CombinedWithAccountState a)
zoomHdAccountCurrentCombine combine upd = withZoomableConstraints $
    zoomHdAccountCheckpointsCombine combine $
      Z.wrap $ \cps -> do
        let l :: Lens' (Checkpoints c) c
            l = unCheckpoints . _Wrapped . SNE.head
        second (\cp' -> cps & l .~ cp') $ Z.unwrap upd (cps ^. l)

-- | Variant of 'zoomHdAccountCheckpoints' that distinguishes between
-- full checkpoints (wallet is up to date) and partial checkpoints
-- (wallet is still recovering historical data)
matchHdAccountCheckpoints :: CanZoom f
                          => f e (Checkpoints Checkpoint)        a
                          -> f e (Checkpoints PartialCheckpoint) a
                          -> f e HdAccount a
matchHdAccountCheckpoints updFull updPartial =
    matchHdAccountState
      (zoom hdUpToDateCheckpoints updFull)
      (zoom hdIncompleteCurrent   updPartial)

-- | Like matchHdAccountCheckpoints, but if also returns the state and if
--   the account is under restoration we combine the current historical
--   and partial checkpoint.
matchHdAccountCheckpointsCombine :: CanZoom f
                               => (a -> a -> a)
                               -> f e (Checkpoints Checkpoint)        a
                               -> f e (Checkpoints PartialCheckpoint) a
                               -> f e HdAccount (CombinedWithAccountState a)
matchHdAccountCheckpointsCombine combine updFull updPartial =
    matchHdAccountState
      (withZoomableConstraints $
          UpToDate <$> zoom hdUpToDateCheckpoints updFull)
      (withZoomableConstraints $
          Incomplete <$> (combine <$> zoom hdIncompleteCurrent updPartial
                                  <*> zoom hdIncompleteHistorical updFull))

{-------------------------------------------------------------------------------
  Zoom to parts of the wallet, creating them if they don't exist
-------------------------------------------------------------------------------}

-- | Variation on 'zoomHdRootId' that creates the 'HdRoot' if it doesn't exist
--
-- Precondition: @newRoot ^. hdRootId == rootId@
zoomOrCreateHdRoot :: HdRoot
                   -> HdRootId
                   -> Update' e HdRoot    a
                   -> Update' e HdWallets a
zoomOrCreateHdRoot newRoot rootId upd =
    zoomCreate (return newRoot) (hdWalletsRoots . at rootId) $ upd

-- | Variation on 'zoomHdAccountId' that creates the 'HdAccount' if it doesn't exist
--
-- Precondition: @newAccount ^. hdAccountId == accountId@
zoomOrCreateHdAccount :: (HdRootId -> Update' e HdWallets ())
                      -> HdAccount
                      -> HdAccountId
                      -> Update' e HdAccount a
                      -> Update' e HdWallets a
zoomOrCreateHdAccount checkRootExists newAccount accId upd = do
    checkRootExists $ accId ^. hdAccountIdParent
    zoomCreate (return newAccount) (hdWalletsAccounts . at accId) $ upd

-- | Variation on 'zoomHdAddressId' that creates the 'HdAddress' if it doesn't exist
zoomOrCreateHdAddress :: (HdAccountId -> Update' e HdWallets ())
                      -> HdAddress
                      -> Update' e HdAddress a
                      -> Update' e HdWallets a
zoomOrCreateHdAddress checkAccountExists newAddress upd = do
    checkAccountExists accId
    zoomCreate createAddress
               (hdWalletsAddresses . at addrId) . zoom ixedIndexed $ upd
    where
        addrId :: HdAddressId
        addrId = newAddress ^. hdAddressId
        accId :: HdAccountId
        accId = addrId ^. hdAddressIdParent

        createAddress :: Update' e HdWallets (Indexed HdAddress)
        createAddress = do
            let err = "zoomOrCreateHdAddress: we checked that the account existed "
                   <> "before calling this function, but the DB lookup failed nonetheless."
            acc <- zoomHdAccountId (error err) accId $ do
                      modifyAndGetOld (hdAccountAutoPkCounter +~ 1)
            return $ Indexed (acc ^. hdAccountAutoPkCounter) newAddress

-- | Assume that the given HdRoot exists
--
-- Helper function which can be used as an argument to 'zoomOrCreateHdAccount'
assumeHdRootExists :: HdRootId -> Update' e HdWallets ()
assumeHdRootExists _id = return ()

-- | Assume that the given HdAccount exists
--
-- Helper function which can be used as an argument to 'zoomOrCreateHdAddress'
assumeHdAccountExists :: HdAccountId -> Update' e HdWallets ()
assumeHdAccountExists _id = return ()

{-------------------------------------------------------------------------------
  Pretty printing
-------------------------------------------------------------------------------}

instance Buildable WalletName where
    build (WalletName wName) = bprint build wName

instance Buildable AccountName where
    build (AccountName txt) = bprint build txt

instance Buildable AssuranceLevel where
    build AssuranceLevelNormal = "normal"
    build AssuranceLevelStrict = "strict"

instance Buildable HasSpendingPassword where
    build (NoSpendingPassword (InDb lastRemove)) =
        bprint ("updated " % build) lastRemove
    build (HasSpendingPassword (InDb lastUpdate)) =
        bprint ("updated " % build) lastUpdate

instance Buildable HdRoot where
    build root@HdRoot{..} = bprint
      ( "HdRoot "
      % "{ id:          " % build
      % ", name:        " % build
      % ", hasPassword: " % build
      % ", assurance:   " % build
      % ", createdAt:   " % build
      % "}"
      )
      (root ^. hdRootId)
      _hdRootName
      _hdRootHasPassword
      _hdRootAssurance
      (_fromDb _hdRootCreatedAt)

instance Buildable HdAccount where
    build HdAccount{..} = bprint
      ( "HdAccount "
      % "{ id            " % build
      % ", name          " % build
      % ", state         " % build
      % ", autoPkCounter " % build
      % "}"
      )
      _hdAccountId
      _hdAccountName
      _hdAccountState
      _hdAccountAutoPkCounter

instance Buildable HdAccountState where
    build (HdAccountStateUpToDate st) =
        bprint ("HdAccountStateUpToDate " % build) st
    build (HdAccountStateIncomplete st) =
        bprint ("HdAccountStateIncomplete " % build) st

instance Buildable HdAccountUpToDate where
    build HdAccountUpToDate{..} = bprint
        ( "HdAccountUpToDate "
        % "{ checkpoints: " % listJson
        % "}"
        )
        _hdUpToDateCheckpoints

instance Buildable HdAccountIncomplete where
    build HdAccountIncomplete{..} = bprint
        ( "HdAccountIncomplete "
        % "{ current:    " % listJson
        % ", historical: " % listJson
        % "}"
        )
        _hdIncompleteCurrent
        _hdIncompleteHistorical

instance Buildable HdAddress where
    build HdAddress{..} = bprint
        (buildTrunc build % "@" % build)
        (_fromDb _hdAddressAddress)
        _hdAddressId

instance Buildable HdAccountId where
    build HdAccountId{..} = bprint
        (buildTrunc build % "/" % build)
        _hdAccountIdParent
        (getHdAccountIx _hdAccountIdIx)

instance Buildable HdAddressId where
    build HdAddressId{..} = bprint
        (build % "/" % build)
        _hdAddressIdParent
        (getHdAddressIx _hdAddressIdIx)

instance Buildable HdAccountIx where
    build (HdAccountIx ix) = bprint ("HdAccountIx " % build) ix

instance Buildable HdAddressIx where
    build (HdAddressIx ix) = bprint ("HdAddressIx " % build) ix

instance Buildable UnknownHdRoot where
    build (UnknownHdRoot rootId) = bprint ("UnknownHdRoot " % build) rootId

instance Buildable UnknownHdAccount where
    build (UnknownHdAccountRoot rootId) =
      bprint ("UnknownHdAccountRoot " % build) rootId
    build (UnknownHdAccount accountId) =
      bprint ("UnknownHdAccount accountId " % build) accountId

instance Buildable UnknownHdAddress where
    build (UnknownHdAddressRoot rootId) =
        bprint ("UnknownHdAddressRoot " % build) rootId
    build (UnknownHdAddressAccount accountId) =
        bprint ("UnknownHdAddress accountId " % build) accountId
    build (UnknownHdAddress addressId) =
        bprint ("UnknownHdAddress " % build) addressId
    build (UnknownHdCardanoAddress coreAddress) =
        bprint ("UnknownHdCardanoAddress " % build) coreAddress
