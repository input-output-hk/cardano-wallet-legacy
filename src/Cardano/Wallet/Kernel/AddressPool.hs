{-# LANGUAGE TemplateHaskell #-}

{-------------------------------------------------------------------------------

  The AddressPool module enables BIP44-style address and account discovery
  for Externally Owned Sequential (EOS) wallets.

  For EOS wallets, we can't add new accounts and therefore can't discover any
  accounts that were not provided upfront.

  In this style of address discovery we can make no assumptions about the sequence
  of accounts, we can only discover addresses in the given wallets (and indeed
  add new addresses to be discovered)

-------------------------------------------------------------------------------}

module Cardano.Wallet.Kernel.AddressPool
    (
    -- * Types
      AddressPool
    , ErrAddressPoolInvalid(..)

    -- * Construction
    , emptyAddressPool
    , initAddressPool

    -- * Manipulation
    , lookupAddressPool

    -- * Inspection
    , getAddressPoolSize
    , getAddressPoolGap
    , getKnownAddresses
    , verifyPool
    ) where

import           Universum

import           Control.Lens (at, makeLenses)
import qualified Data.Map as Map
import           Formatting (bprint, build, int, (%))
import qualified Formatting.Buildable
import           Serokell.Util (listJson)
import           Test.QuickCheck (Arbitrary (..), elements)

import           Cardano.Wallet.Kernel.AddressPoolGap (AddressPoolGap)


{-------------------------------------------------------------------------------
  Types
-------------------------------------------------------------------------------}

-- Note: the account and address types are polymorphic in order to support
-- varying wallet types
data AddressPool address = AddressPool
    { _addresses -- all addresses currently in the pool, discovered or not
        :: !(Map address Word32)

    , _gap -- the "gap" to maintain after the last discovered address in a pool
        :: !AddressPoolGap

    , _nextAddresses -- compute next 'gap' addresses from an index
        :: Word32 -> Map address Word32
    }
makeLenses ''AddressPool

instance Buildable address => Buildable (AddressPool address) where
    build pool = bprint
        ("AddressPool (gap: "%int%") (total: "%int%") "%listJson)
        (pool ^. gap)
        (Map.size $ pool ^. addresses)
        (map (uncurry $ bprint (build%":"%build)) $ Map.toList $ pool ^. addresses)


data ErrAddressPoolInvalid
    = ErrIndexesAreNotSequential
    | ErrNotEnoughAddresses
    deriving (Eq, Show)

instance Buildable ErrAddressPoolInvalid where
    build ErrIndexesAreNotSequential =
        bprint "ErrIndexesAreNotSequential"
    build ErrNotEnoughAddresses =
        bprint "ErrNotEnoughAddresses"

instance Exception ErrAddressPoolInvalid

instance Arbitrary ErrAddressPoolInvalid where
    arbitrary = elements
        [ ErrIndexesAreNotSequential
        , ErrNotEnoughAddresses
        ]


{-------------------------------------------------------------------------------
  Constructions
-------------------------------------------------------------------------------}

emptyAddressPool
    :: (Ord address)
    => AddressPoolGap
    -> (Word32 -> address)
    -> AddressPool address
emptyAddressPool g newAddress = AddressPool
    { _addresses = (mkNextAddresses g newAddress) 0
    , _gap = g
    , _nextAddresses = mkNextAddresses g newAddress
    }

initAddressPool
    :: (Ord address)
    => AddressPoolGap
    -> (Word32 -> address)
    -> [(address, Word32)]
    -> Either ErrAddressPoolInvalid (AddressPool address)
initAddressPool g newAddress addrs0 = verifyPool $ AddressPool
    { _addresses = Map.fromList addrs0
    , _gap = g
    , _nextAddresses = mkNextAddresses g newAddress
    }


{-------------------------------------------------------------------------------
  Manipulation
-------------------------------------------------------------------------------}

-- | Lookup an address in the pool. When we find an address in a pool, the pool
-- may be amended in the following ways
--
--   * if the address was discovered near the edge, the pool is extended
--
-- It is also possible that the pool is not amended at all - this happens in the
-- case that an address is discovered 'far' from the edge.
lookupAddressPool
    :: (Ord address)
    => address
    -> AddressPool address
    -> (Maybe (address, Word32), AddressPool address)
lookupAddressPool target pool =
    case pool ^. (addresses . at target) of
        Just ix ->
            (Just (target, ix), extendAddressPool ix pool)
        Nothing ->
            (Nothing, pool)


{-------------------------------------------------------------------------------
  Inspection
-------------------------------------------------------------------------------}
--
-- | Check some invariant on the given pool
verifyPool
    :: AddressPool address
    -> Either ErrAddressPoolInvalid (AddressPool address)
verifyPool pool@(AddressPool addrs g _) = do
    when pIndexesAreNotSequential $ Left ErrIndexesAreNotSequential
    when pNotEnoughAddresses $ Left ErrNotEnoughAddresses
    pure pool
  where
    ixs = sort $ Map.elems addrs
    pNotEnoughAddresses = Map.size addrs < fromIntegral g
    pIndexesAreNotSequential = not (null ixs) && [0..(fromIntegral (length ixs) - 1)] /= ixs

-- | Get the underlying pool's size
getAddressPoolSize
    :: AddressPool address -> Int
getAddressPoolSize pool = Map.size $ pool ^. addresses

-- | Get the underlying pool's gap
getAddressPoolGap
    :: AddressPool address -> AddressPoolGap
getAddressPoolGap pool = pool ^. gap

getKnownAddresses
    :: AddressPool address
    -> [(address, Word32)]
getKnownAddresses pool =
    let xs = sortOn snd $ Map.toList (pool ^. addresses)
    in take (length xs - fromIntegral (pool ^. gap))  xs


{-------------------------------------------------------------------------------
  Internals
-------------------------------------------------------------------------------}

-- | If an address is discovered near the edge, we extend the address sequence,
--   otherwise we return the pool untouched.
extendAddressPool
    :: (Ord address)
    => Word32
    -> AddressPool address
    -> AddressPool address
extendAddressPool ix pool
    | isOnEdge  = pool & addresses %~ (next <>)
    | otherwise = pool
  where
    edge = Map.size (pool ^. addresses)
    isOnEdge = fromIntegral edge - ix <= fromIntegral (pool ^. gap)
    next = (pool ^. nextAddresses) (ix + 1)

mkNextAddresses
    :: (Ord address)
    => AddressPoolGap
    -> (Word32 -> address)
    -> Word32
    -> Map address Word32
mkNextAddresses g newAddress fromIx =
    invariant (toIx > fromIx) "nextAddresses: toIx should be greater than fromIx"
    $ Map.fromList
    $ map (\ix -> (newAddress ix, ix))
    [fromIx .. toIx]
  where
    toIx = fromIx + (fromIntegral g) - 1

-- | Fails hard if an invariant does not hold, or proceed.
invariant
    :: Bool
    -> Text
    -> a
    -> a
invariant predicate msg action =
    if predicate then action else error msg
