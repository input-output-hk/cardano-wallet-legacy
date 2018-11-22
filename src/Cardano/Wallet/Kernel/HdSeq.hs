{-# LANGUAGE AllowAmbiguousTypes        #-}

module Cardano.Wallet.Kernel.HdSeq
       (
         addrInPool
       ,
         firstAddressRange
       ,
         initEOSAddressPool
       ,
         initFOSAddressPool
       ,
         newAccountPool
       ,
         selectOurs
       ,
         AccountPoolStatus (..)
       ,
         AccountPool (..)
       ,
         AddressPool (..)
       ,
         AccountPoolAmendment (..)
       ) where

import           Universum

import qualified Data.Map as Map
import qualified Data.List.NonEmpty as NE

import           Cardano.Wallet.Kernel.DB.HdWallet(HdAddressIx (..))

{-------------------------------------------------------------------------------
  The AddressPool module enables BIP44-style address and account discovery
  for 2 different styles of wallets:

  * Externally Owned Sequential (EOS) wallets *

  For EOS wallets, we can't add new accounts and therefor can't discover any
  accounts that were not provided upfront.

  In this style of address discovery we can make no assumptions about the sequence
  of accounts, we can only discover addresses in the given wallets (and indeed
  add new addresses to be discovered)

  * Fully Owned Sequential (FOS) wallets *

  For FOS wallets, we can automatically create (and thereby discover) new accounts,
  since we have the necessary wallet credentials.

  In this style of address/account discovery, the accounts are considered to be
  in sequence, and we can discover all accounts for the wallet with this algorithm.
-------------------------------------------------------------------------------}

{-------------------------------------------------------------------------------
  AddressPool Types
-------------------------------------------------------------------------------}

-- | When an address is discovered in a sequence of addresses, if that
--   address is "close enough" to the end of the sequence, we want to generate
--   more addresses in the sequence (to increase the range of addresses that
--   may be discovered). The AddressPoolGap expresses this notion of "close enough".
type AddressPoolGap = Word32

-- | An AccountPool is considered _live_ if any address in the pool has already been
--   discovered, otherwise the pool is _pending_
data AccountPoolStatus = LivePool | PendingPool
    deriving (Eq, Ord, Show)

-- | Constructor for creating a new sequential Hd account, takes as argument
--   the previous Hd account in the sequence (this is only relevant to FOS wallets)
type NewAccount account = account -> account

-- | Constructor for creating a new sequential Hd address, takes as argument
--   the address index
type NewAddress address = HdAddressIx -> address

-- | The pool of "our" addresses for an account

-- Note: the account and address types are polymorphic in order to support
-- varying wallet types
data AccountPool account address = AccountPool {
    _accPoolAccountId :: account
  ,
    -- can be in Live or Pending state
    _accPoolStatus    :: !AccountPoolStatus
  ,
    -- the sequence of "our" addresses for this account
    _accPoolAddrs     :: NonEmpty (address,HdAddressIx)
}

-- | When an address is discovered in an account pool, there are a few possible
--   outcomes based on whether the address was discovered within the "gap" range
--   or whether a new account pool must be added to the discovery process.
data AccountPoolAmendment account address
    = -- If the address was discovered in a Live account, outside of the gap range,
      -- then the account pool is left unchanged
       AmendmentNone (AccountPool account address)
      -- If the address was discovered within the gap range, then the address range
      -- of the account pool must be extended
      | AmendmentRangeExtended (AccountPool account address)
      -- If the address was discovered in a Pending account pool, we need to
      -- change it to Live (in this case we assume we were not able to create a new
      -- account due to lacking the wallet key)
      | AmendmentGoLive (AccountPool account address)
      -- If the address was discovered in a Pending account pool, we need to
      -- mark it as Live and create the next Pending account pool
      | AmendmentGoLiveWithNew (AccountPool account address) (AccountPool account address)


-- | An AddressPool contains the full collection of addresses for a wallet by
--   maintaining an AccountPool (address pool per account) for each wallet account
--   we are tracking.
--
-- NOTE: For sequential Hd wallets, the spending password is required
-- in order to create an account, hence we allow for wallets that are
-- not able to create new accounts
data AddressPool account address = AddressPool {
    -- address pools per account
    _addrPoolAccounts   :: [AccountPool account address]
  ,
    -- the "gap" to maintain after the last discovered address in a pool
    _addrPoolGap        :: AddressPoolGap
  ,
    -- a constructor for new addresses
    _addrPoolNewAddress :: NewAddress address
  ,
    -- optional constructor for new accounts
    _addrPoolNewAccount :: Maybe (NewAccount account)
}

{-------------------------------------------------------------------------------
  Address Range
-------------------------------------------------------------------------------}

-- | Construct a sequenced range of addresses, starting at index 'fromIx'
-- TODO @uroboros PRECONDITION - range >= 2
newAddressRange :: AddressPoolGap
                -> HdAddressIx
                -> NewAddress address
                -> NonEmpty (address,HdAddressIx)
newAddressRange range (HdAddressIx fromIx) newAddress
    = (f fromIx) NE.:| (map f [fromIx + 1 .. fromIx + range - 1])
    where
        f ix = let ix_ = (HdAddressIx ix) in
                    (newAddress ix_, ix_)

-- | The first address range for a pool starts at non-hardened index 0
--
-- NOTE: we default the range size to twice the gap size (otherwise the first
-- address discovered would immediately trigger a range extension)
firstAddressRange :: AddressPoolGap
                  -> NewAddress address
                  -> NonEmpty (address,HdAddressIx)
firstAddressRange gap
    = newAddressRange range firstAddressIx
    where
        range = gap * 2 -- TODO @uroboros discuss
        firstAddressIx = HdAddressIx 0

-- | Construct an address range from the given index, in this way we can seamlessly
--   extend an existing address range.
nextAddressRange :: AddressPoolGap
                 -> HdAddressIx
                 -> NewAddress address
                 -> NonEmpty (address,HdAddressIx)
nextAddressRange gap lastIx
    = newAddressRange gap (incrAddressIx lastIx)
    where
        incrAddressIx :: HdAddressIx -> HdAddressIx
        incrAddressIx (HdAddressIx ix) = HdAddressIx (ix + 1)

{-------------------------------------------------------------------------------
  Account Pool Creation
-------------------------------------------------------------------------------}

-- | Create an AccountPool for the given account with an initial address range
newAccountPool :: AddressPoolGap
               -> NewAddress address
               -> AccountPoolStatus
               -> account
               -> AccountPool account address
newAccountPool gap newAddress status accountPK
    = AccountPool {
          _accPoolAccountId = accountPK
        ,
          _accPoolStatus    = status
        ,
          _accPoolAddrs     = firstAddressRange gap newAddress
        }

-- | For FOS wallets (where we own the wallet keys and can create new accounts)
--   we want to create a new AccountPool based on the previous pool.
nextAccountPool :: AddressPoolGap
                -> NewAccount account
                -> NewAddress address
                -> AccountPool account address
                -> AccountPool account address
nextAccountPool gap newAccount newAddress prevPool
    = newAccountPool gap newAddress PendingPool nextAccountPK
    where
        nextAccountPK = newAccount (_accPoolAccountId prevPool)

{-------------------------------------------------------------------------------
  Address Pool Creation
-------------------------------------------------------------------------------}

-- | EOS wallets are constructed from a set of account keys. We make no assumptions
--   about the sequence of these accounts or whether these are in fact all the accounts
--   in the wallet. We simply track the accounts that we are given, and don't create
--   any new accounts in the discovery process (because we don't have the wallet key
--   required for that)
initEOSAddressPool :: AddressPoolGap
                   -> NewAddress address
                   -> [account]
                   -> AddressPool account address
initEOSAddressPool gap newAddress accountPKs
    = AddressPool {
        -- All pools are created Live (since Pending state is used to trigger
        -- the autocreation of an account)
        _addrPoolAccounts   = map (newAccountPool gap newAddress LivePool) accountPKs
      ,
        _addrPoolGap        = gap
      ,
        -- the constructor to extend the address pool range for an account
        _addrPoolNewAddress = newAddress
      ,
        -- for EOS wallets we cannot create new accounts to be discovered
        _addrPoolNewAccount = Nothing
    }

-- | FOS wallets consist of a sequence of accounts. When discovering FOS wallet
--   addresses we can discover all the accounts for the wallet because we
--   are able to create new accounts during the discovery process.
initFOSAddressPool :: AddressPoolGap
                   -> NewAccount account
                   -> NewAddress address
                   -> account
                   -> AddressPool account address
initFOSAddressPool gap newAccount newAddress accountPK
    = AddressPool {
        -- we start with a single account in Pending state
        _addrPoolAccounts   = [newAccountPool gap newAddress PendingPool accountPK]
      ,
        _addrPoolGap        = gap
      ,
        -- the constructor to extend the address pool range for an account
        _addrPoolNewAddress = newAddress
      ,
        -- the constructor to create the next account in a sequence (given the
        -- previous account)
        _addrPoolNewAccount = Just newAccount
    }


{-------------------------------------------------------------------------------
  Address Discovery
-------------------------------------------------------------------------------}

-- | When we find an address in a pool, the pool may be amended in the following ways
--   * if the pool was in Pending state, it will change to Live
--   * if we are dealing with a sequence of accounts (in the case of FOS wallets),
--     we possibly want to create the _next_ account pool to discover
--   * if the address was discovered within the "gap", the pool is extended
--
-- It is also possible that the pool is not amended at all - this happens in the
-- case that an address is discovered outside of the address sequence gap, and the
-- pool is already live.
addrInPool :: Ord address
           => AddressPoolGap
           -> Maybe (NewAccount account)
           -> NewAddress address
           -> AccountPool account address
           -> address
           -> Maybe (AccountPoolAmendment account address, (account, HdAddressIx))
addrInPool gap newAccount newAddress pool addr
    = f <$> Map.lookup addr (Map.fromList . NE.toList $ _accPoolAddrs pool)
    where
        f ix = (
                 ensureLive gap newAccount newAddress ix pool
               ,
                 (_accPoolAccountId pool, ix)
               )

-- | If an address is discovered within the gap, we extend the address sequence,
--   otherwise we return the pool with no amendment.
ensureGap :: AddressPoolGap
          -> NewAddress address
          -> HdAddressIx
          -> AccountPool account address
          -> AccountPoolAmendment account address
ensureGap gap newAddress foundIx pool
    = if withinGap lastIx foundIx
        then AmendmentRangeExtended (pool {_accPoolAddrs = extendedAddressRange})
        else AmendmentNone pool
    where
        currentAddrRange      = _accPoolAddrs pool
        lastIx                = snd . last $ currentAddrRange
        extendedAddressRange  = currentAddrRange <> (nextAddressRange gap lastIx newAddress)

        withinGap :: HdAddressIx -> HdAddressIx -> Bool
        withinGap (HdAddressIx lastIx_) (HdAddressIx foundIx_)
            = lastIx_ - foundIx_ <= gap

-- | When an address is discovered in an AccountPool, ensure the correct status
--   and address sequence "gap":
-- * if the pool is already Live, ensure that the address sequence gap is honoured
-- * if the pool is in Pending state, make the pool Live and create the _next_
--   account pool if relevant.
ensureLive :: AddressPoolGap
           -> Maybe (NewAccount account)
           -> NewAddress address
           -> HdAddressIx
           -> AccountPool account address
           -> AccountPoolAmendment account address
ensureLive gap newAccount newAddress foundIx pool
    = case _accPoolStatus pool of
        LivePool ->
            ensureGap gap newAddress foundIx pool
        PendingPool -> case newAccount of
            Just newAccount_ ->
                -- go live and create the next pool
                AmendmentGoLiveWithNew (goLive pool) (nextAccountPool gap newAccount_ newAddress pool)
            Nothing ->
                -- go live without creating the next pool
                AmendmentGoLive (goLive pool)
    where
        goLive p = p {_accPoolStatus = LivePool}

-- | Search for an address in a list of account pools.
--   (Since the address can only appear in one account, we can return the
--    result as soon as we have a match).
addrInPools :: (Eq account, Ord address)
            => AddressPoolGap
            -> Maybe (NewAccount account)
            -> NewAddress address
            -> [AccountPool account address]
            -> address
            -> Maybe (AccountPoolAmendment account address, (account, HdAddressIx))
addrInPools _ _ _ [] _ = Nothing
addrInPools gap newAccount newAddress (p:ps) addr
    = addrInPool gap newAccount newAddress p addr <|> addrInPools gap newAccount newAddress ps addr

-- | Discover "our" addresses in a list of account pools.
--
--   With each address discovered, the pools may be amended (and new ones added).
--   In this way, we search for each address and pass the amended pools
--   as argument to the search for the next address.
--
--   In other words, the address pools we match against may change as we
--   discover new addresses.
addrsInPools :: (Eq account, Ord address)
             => AddressPoolGap
             -> Maybe (NewAccount account)
             -> NewAddress address
             -> [AccountPool account address]
             -> [address]
             -> [(account, HdAddressIx)]
addrsInPools _ _ _ _ [] = []
addrsInPools gap newAccount newAddress pools (a:as)
    = case addrInPools gap newAccount newAddress pools a of
        Just (amended, (accountId, addressIx)) ->
            -- the address was found in one of the pools
            (accountId, addressIx) : addrsInPools gap newAccount newAddress (amendedPools amended) as
        Nothing ->
            -- address not found, continue the search for the remaining addresses
            addrsInPools gap newAccount newAddress pools as
    where
        -- continue search with the same pools
        amendedPools (AmendmentNone _)               = pools
        -- replace the amended pool before continuing the search
        amendedPools (AmendmentRangeExtended p)      = replacePool p
        -- replace the amended pool before continuing the search
        amendedPools (AmendmentGoLive p)             = replacePool p
        -- replace the amended pool and add the new pool before continuing the search
        amendedPools (AmendmentGoLiveWithNew p newP) = newP : replacePool p

        replacePool p' = map (\x -> if _accPoolAccountId x == _accPoolAccountId p' then p' else x) pools

-- | Discover "our" addresses in a wallet's AddressPool.
selectOurs :: (Eq account, Ord address)
            => AddressPool account address
            -> [address]
            -> [(account, HdAddressIx)]
selectOurs pool = addrsInPools (_addrPoolGap pool) (_addrPoolNewAccount pool) (_addrPoolNewAddress pool) (_addrPoolAccounts pool)
