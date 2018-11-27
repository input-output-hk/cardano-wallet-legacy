module Cardano.Wallet.Kernel.AddressPool
    ( addressInPool
    , initAddressPool
    , AddressPool
    ) where

import           Universum

import qualified Data.List as L
import           Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE

import           Cardano.Wallet.Kernel.AddressPoolGap (AddressPoolGap)

{-------------------------------------------------------------------------------

  The AddressPool module enables BIP44-style address and account discovery
  for Externally Owned Sequential (EOS) wallets.

  For EOS wallets, we can't add new accounts and therefor can't discover any
  accounts that were not provided upfront.

  In this style of address discovery we can make no assumptions about the sequence
  of accounts, we can only discover addresses in the given wallets (and indeed
  add new addresses to be discovered)

-------------------------------------------------------------------------------}

{-------------------------------------------------------------------------------
  AddressPool Types
-------------------------------------------------------------------------------}

-- | When an address is discovered in a sequence of addresses, if that
--   address is "close enough" to the end of the sequence, we want to generate
--   more addresses in the sequence (to increase the range of addresses that
--   may be discovered). The AddressPoolGap expresses this notion of "close enough".

-- | Address pool per account is considered _live_ if any address in it has already been
--   discovered, otherwise the pool is _pending_.
data AddressPoolStatus = LivePool | PendingPool
    deriving (Eq, Ord)

-- | The pool of "our" addresses for an account

-- Note: the account and address types are polymorphic in order to support
-- varying wallet types
data (Eq account, Ord address) => AddressPoolPerAccount account address =
    AddressPoolPerAccount
        { _accPoolAccountId :: account
          -- can be in Live or Pending state
        , _accPoolStatus    :: !AddressPoolStatus
          -- the sequence of "our" addresses for this account
        , _accPoolAddresses :: NonEmpty (address, Word)
        }

-- | An 'AddressPool' contains the full collection of addresses for a wallet by
--   maintaining an AddressPoolPerAccount for each wallet account we are tracking.
data (Eq account, Ord address) => AddressPool account address =
    AddressPool
        { -- address pools per account
          _addrPoolAccounts   :: [AddressPoolPerAccount account address]
          -- the "gap" to maintain after the last discovered address in a pool
        , _addrPoolGap        :: AddressPoolGap
          -- a constructor for a new addresses using index
        , _addrPoolNewAddress :: (Word -> address)
        }

{-------------------------------------------------------------------------------
  Address Pool Creation
-------------------------------------------------------------------------------}

-- | EOS-wallets are constructed from a set of account public keys. We make no assumptions
--   about the sequence of these accounts or whether these are in fact all the accounts
--   in this wallet. We simply track the accounts that we are given, and don't create
--   any new accounts in the discovery process (because we don't have the wallet key
--   required for that).
initAddressPool
    :: (Eq account, Ord address)
    => AddressPoolGap
    -> (Word -> address)
    -> [account]
    -> AddressPool account address
initAddressPool gap newAddress accountPublicKeys
    = AddressPool {
        _addrPoolAccounts   = map (\accountPublicKey ->
                                      AddressPoolPerAccount
                                          accountPublicKey
                                          LivePool
                                          (newAddressRange gap firstAddressIx newAddress))
                                  accountPublicKeys
      , _addrPoolGap        = gap
      , _addrPoolNewAddress = newAddress
    }
  where
    -- The first address range for a pool starts at non-hardened index 0
    firstAddressIx = 0

{-------------------------------------------------------------------------------
  Address Discovery
-------------------------------------------------------------------------------}

-- | Search for an address in the whole address pool.
addressInPool
    :: (Eq account, Ord address)
    => AddressPool account address
    -> address
    -> (AddressPool account address, Maybe (account, Word))
addressInPool pool ourAddress = (probablyUpdatedPool, foundAddressInfo)
  where
    probablyUpdatedPool = pool {_addrPoolAccounts = allAccPools}
    foundAddressInfo = case catMaybes addrInfo of
        [infoAboutOurAddress] -> Just infoAboutOurAddress
        _                     -> Nothing

    (allAccPools, addrInfo) = L.unzip accPoolsWithAddrInfo

    -- Search our address in all internal address pools per accounts.
    accPoolsWithAddrInfo = map (addressInAccPool pool ourAddress) (_addrPoolAccounts pool)

-- | When we find an address in a pool, the pool may be amended in the following ways
--   * if the pool was in Pending state, it will change to Live
--   * if the address was discovered within the "gap", the pool is extended
--
-- It is also possible that the pool is not amended at all - this happens in the
-- case that an address is discovered outside of the address sequence gap, and the
-- pool is already live.
addressInAccPool
    :: (Eq account, Ord address)
    => AddressPool account address
    -> address
    -> AddressPoolPerAccount account address
    -> (AddressPoolPerAccount account address, Maybe (account, Word))
addressInAccPool pool ourAddress accPool =
    let addressIsHere = L.lookup ourAddress (NE.toList $ _accPoolAddresses accPool) in
    case addressIsHere of
        Just addressIx ->
            let accPool' = ensureAccPoolIsLive pool addressIx accPool in
            ( accPool'
            , Just (_accPoolAccountId accPool, addressIx)
            )
        Nothing ->
            ( accPool
            , Nothing
            )

-- | When an address is discovered in an AddressPoolPerAccount, ensure the correct status
--   and address sequence "gap":
-- * if the pool is already Live, ensure that the address sequence gap is honoured
-- * if the pool is in Pending state, make the pool Live and create the _next_
--   account pool if relevant.
ensureAccPoolIsLive
    :: (Eq account, Ord address)
    => AddressPool account address
    -> Word
    -> AddressPoolPerAccount account address
    -> AddressPoolPerAccount account address
ensureAccPoolIsLive pool foundAddressIx currentAccPool =
    case _accPoolStatus currentAccPool of
        LivePool    -> ensureAddressIsInGap pool foundAddressIx currentAccPool
        PendingPool -> updatedAccPool
  where
    updatedAccPool = currentAccPool {_accPoolStatus = LivePool}

-- | If an address is discovered within the gap, we extend the address sequence,
--   otherwise we return the pool with no amendment.
ensureAddressIsInGap
    :: (Eq account, Ord address)
    => AddressPool account address
    -> Word
    -> AddressPoolPerAccount account address
    -> AddressPoolPerAccount account address
ensureAddressIsInGap pool foundAddressIx currentAccPool =
    if addressInGap ixOfLastAddress foundAddressIx
        then updatedAccPool
        else currentAccPool
  where
    addressInGap lastIx foundIx = lastIx - foundIx <= fromIntegral gap
    updatedAccPool       = currentAccPool {_accPoolAddresses = extendedAddressRange}
    currentAddressRange  = _accPoolAddresses currentAccPool
    ixOfLastAddress      = snd . last $ currentAddressRange
    extendedAddressRange = currentAddressRange <> (newAddressRange gap (ixOfLastAddress + 1) newAddress)
    gap = _addrPoolGap pool
    newAddress = _addrPoolNewAddress pool

{-------------------------------------------------------------------------------
  Utils
-------------------------------------------------------------------------------}

-- | Construct a sequenced range of addresses, starting at index 'fromIx'
-- TODO @uroboros PRECONDITION - range >= 2
newAddressRange
    :: AddressPoolGap
    -> Word
    -> (Word -> address)
    -> NonEmpty (address, Word)
newAddressRange gap fromIx newAddress = NE.fromList addressRange
  where
    addressRange = map (\ix -> (newAddress ix, ix)) [fromIx, fromIx + 1 .. toIx]
    toIx = fromIx + (fromIntegral gap) - 1
