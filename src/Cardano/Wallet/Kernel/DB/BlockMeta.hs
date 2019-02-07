{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE StandaloneDeriving         #-}

-- | Block metadata conform the wallet specification
module Cardano.Wallet.Kernel.DB.BlockMeta (
    -- * Block metadata
    BlockMeta(..)
  , AddressMeta(..)
  , addressMeta
    -- * Local block metadata
  , LocalBlockMeta(..)
    -- ** Lenses
  , addressMetaIsUsed
  , blockMetaAddressMeta
  , blockMetaSlotId
  ) where

import           Universum

import           Cardano.Wallet.Util (buildIndent, buildMap, buildTrunc)
import           Control.Lens (at, non)
import           Control.Lens.TH (makeLenses, makeWrapped)
import qualified Data.Map.Strict as Map
import           Data.SafeCopy (base, deriveSafeCopy)
import           Formatting (bprint, build, (%))
import qualified Formatting.Buildable

import qualified Pos.Chain.Txp as Txp
import qualified Pos.Core as Core

import           Cardano.Wallet.Kernel.DB.InDb


{-------------------------------------------------------------------------------
  Address metadata
-------------------------------------------------------------------------------}

-- | Address metadata
data AddressMeta = AddressMeta {
      -- | Whether or not an Address has been 'used'
      _addressMetaIsUsed   :: Bool
    } deriving Eq
makeLenses ''AddressMeta
deriveSafeCopy 1 'base ''AddressMeta

instance Semigroup AddressMeta where
    (AddressMeta used) <> (AddressMeta used') = AddressMeta (used || used')

instance Monoid AddressMeta where
  mempty  = AddressMeta False
  mappend = (<>)

instance Buildable AddressMeta where
    build AddressMeta{..} = bprint
        ("AddressMeta isUsed: " % build)
        _addressMetaIsUsed


{-------------------------------------------------------------------------------
  Block metadata
-------------------------------------------------------------------------------}

-- | Block metadata
data BlockMeta = BlockMeta {
      -- | Slot each transaction got confirmed in
      _blockMetaSlotId      :: !(InDb (Map Txp.TxId Core.SlotId))
      -- | Address metadata
    , _blockMetaAddressMeta :: !(Map (InDb Core.Address) AddressMeta)
    } deriving Eq
makeLenses ''BlockMeta
deriveSafeCopy 1 'base ''BlockMeta

instance Buildable BlockMeta where
    build BlockMeta{..} = bprint
        ( "BlockMeta"
        % "\n  slotId:      \n" % buildIndent 4 (buildMap build "=>" build)
        % "\n  addressMeta: \n" % buildIndent 4 (buildMap (buildTrunc build) "=>" build)
        )
        (_fromDb _blockMetaSlotId)
        (_blockMetaAddressMeta)

-- In the typical case, we have 'BlockMeta' for the chain so far, then derive
-- local blockmeta for the new block that just arrived and want to combine this
-- with the existing 'BlockMeta'.
instance Semigroup BlockMeta where
    BlockMeta (InDb a1) b1 <> BlockMeta (InDb a2) b2 =
        BlockMeta (InDb (a1 <> a2)) (Map.unionWith (<>) b1 b2)

instance Monoid BlockMeta where
    mempty = BlockMeta (InDb mempty) mempty

-- | Address metadata for the specified address
--
-- When the block metadata does not contain any information about this address,
-- we assume 'mempty'.
addressMeta :: Core.Address -> Lens' BlockMeta AddressMeta
addressMeta addr = blockMetaAddressMeta . at (InDb addr) . non mempty


{-------------------------------------------------------------------------------
  Local block metadata
-------------------------------------------------------------------------------}

-- | Local block metadata
--
-- Local block metadata is block metadata derived from a single block, or
-- possibly a few blocks, without access to the entire chain. The underlying
-- 'BlockMeta' type is the same; 'LocalBlockMeta' serves merely as a marker that
-- this data is potentially incomplete.
newtype LocalBlockMeta = LocalBlockMeta { localBlockMeta :: BlockMeta }
        deriving stock Eq
        deriving newtype (Semigroup, Monoid, Buildable)
makeWrapped ''LocalBlockMeta
deriveSafeCopy 1 'base ''LocalBlockMeta
