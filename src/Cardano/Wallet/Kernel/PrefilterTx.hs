{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE TypeFamilies        #-}

module Cardano.Wallet.Kernel.PrefilterTx
       ( PrefilteredBlock (..)
       , PrefilterKey (..)
       , AddrWithId
       , UtxoWithAddrId
       -- Prefilter API
       , filterOursHdRnd        -- TODO #34 deprecate -> use filterOurs
       , prefilterBlockHdRnd    -- TODO #34 deprecate -> use prefilterBlock
       , prefilterUtxoHdRnd     -- TODO #34 deprecate -> use prefilterUtxo
       -- HdRnd Helpers         -- TODO #34 move to PrefilterHdRnd
       , addressIdFromMeta
       , toHdRndPrefKey
       , toHdRndPrefKey'
       , toHdRndPrefKeys
       -- Auxiliary
       , emptyPrefilteredBlock
       , toPrefilteredUtxoHdRnd -- TODO #34 deprecate -> use toPrefilteredUtxo
       ) where

import           Universum

import           Data.List (nub)
import qualified Data.List.NonEmpty as NE

import qualified Data.Map as Map
import qualified Data.Set as Set
import           Formatting (bprint, (%))
import qualified Formatting.Buildable
import           Serokell.Util (listJson, mapJson)

import           Data.SafeCopy (base, deriveSafeCopy)

import           Pos.Chain.Txp (TxId, TxIn (..), TxOut (..), TxOutAux (..),
                     Utxo)
import           Pos.Core (Address (..), Coin, SlotId)
import           Pos.Core.NetworkMagic (NetworkMagic)
import           Pos.Crypto (EncryptedSecretKey,PublicKey)

import qualified Cardano.Wallet.API.V1.Types as V1
import           Cardano.Wallet.Kernel.DB.BlockContext
import           Cardano.Wallet.Kernel.DB.BlockMeta
import           Cardano.Wallet.Kernel.DB.HdWallet
import           Cardano.Wallet.Kernel.DB.InDb (InDb (..), fromDb)
import           Cardano.Wallet.Kernel.DB.Resolved (ResolvedBlock,
                     ResolvedInput, ResolvedTx, rbContext, rbTxs,
                     resolvedToTxMeta, rtxInputs, rtxMeta, rtxOutputs)
import           Cardano.Wallet.Kernel.DB.Spec.Pending (Pending)
import qualified Cardano.Wallet.Kernel.DB.Spec.Pending as Pending
import           Cardano.Wallet.Kernel.DB.TxMeta.Types
import           Cardano.Wallet.Kernel.Decrypt (WalletDecrCredentials,
                     decryptAddress, eskToWalletDecrCredentials)
import           Cardano.Wallet.Kernel.Types (AccountId (..), AddressId (..),
                    WalletId (..), addrIdToAccountId)
import           Cardano.Wallet.Kernel.Util.Core

{-------------------------------------------------------------------------------
 Core prefiltering types
-------------------------------------------------------------------------------}

-- | Address extended with an AddressId, which includes a reference to the parent
--   AccountId in the HD hierarchy.
-- TODO #34 deprecate -> replace with AddrWithId__
type AddrWithId = (HdAddressId,Address)

-- | Prefiltered block
--
-- A prefiltered block is a block that contains only inputs and outputs from
-- the block that are relevant to the wallet.
-- TODO #34 deprecate -> replace with PrefilteredBlock__
data PrefilteredBlock = PrefilteredBlock {
      -- | Relevant inputs
      pfbInputs        :: !(Set TxIn)

      -- | Relevant foreign inputs
    , pfbForeignInputs :: !(Set TxIn)

      -- | Relevant outputs
    , pfbOutputs       :: !Utxo

      -- | all output addresses present in the Utxo
    , pfbAddrs         :: ![AddrWithId]

      -- | Prefiltered block metadata
    , pfbMeta          :: !LocalBlockMeta

      -- | Block context
    , pfbContext       :: !BlockContext
    }

deriveSafeCopy 1 'base ''PrefilteredBlock

-- | Empty prefiltered block
--
-- An empty prefiltered block is what we get when we filter a block for a
-- particular account and there is nothing in the block that is of
-- relevance to that account
emptyPrefilteredBlock :: BlockContext -> PrefilteredBlock
emptyPrefilteredBlock context = PrefilteredBlock {
      pfbInputs         = Set.empty
    , pfbForeignInputs  = Set.empty
    , pfbOutputs        = Map.empty
    , pfbAddrs          = []
    , pfbMeta           = emptyLocalBlockMeta
    , pfbContext        = context
    }

-- | Summary of an address as it appears in a transaction.
--   NOTE: Since an address can occur in multiple transactions, there could be
--   multiple valid summaries for an address.
data AddressSummary = AddressSummary {
      addrSummaryAddr        :: Address
    ,
      addrSummaryId          :: AddressId
    ,
      addrSummaryTxId        :: TxId
    ,
      -- | indicates whether _all_ the inputs of the transaction are "ours"
      addrSummaryOnlyOurInps :: Bool
    ,
      -- | indicates whether _all_ the outputs of the transaction are "ours"
      addrSummaryOnlyOurOuts :: Bool
    }

-- | Extended Utxo with each output paired with an HdAddressId, required for
--   discovering new Addresses during prefiltering
-- TODO #34 deprecate -> replace with UtxoWithAddrId__
type UtxoWithAddrId = Map TxIn (TxOutAux,HdAddressId)

-- | Extended Utxo where each output is paired with an AddressSummary. Provides
--   the required metadata for computing address meta data for BlockMeta.
type UtxoSummaryRaw = Map TxIn (TxOutAux,AddressSummary)

{-------------------------------------------------------------------------------
 These types use abstract Account/Address ids and will replace
 the Deprecated Types above
 -------------------------------------------------------------------------------}

type AddrWithId__ = (AddressId,Address)

data PrefilteredBlock__ = PrefilteredBlock__ {
      -- | Relevant inputs
      pfbInputs__        :: !(Set TxIn)

      -- | Relevant foreign inputs
    , pfbForeignInputs__ :: !(Set TxIn)

      -- | Relevant outputs
    , pfbOutputs__       :: !Utxo

      -- | all output addresses present in the Utxo
    , pfbAddrs__         :: ![AddrWithId__]

      -- | Prefiltered block metadata
    , pfbMeta__          :: !LocalBlockMeta

      -- | Block context
    , pfbContext__       :: !BlockContext
    }

type UtxoWithAddrId__ = Map TxIn (TxOutAux,AddressId)

{-------------------------------------------------------------------------------
 Core definitions of discovery of "our" addresses
-------------------------------------------------------------------------------}

-- | We assume that the AddressId provides access to AccountId
type IsOurs = Address -> Maybe AddressId

-- | For each wallet type, captures all the data necessary to discover "our" addresses.
--   This type provides the basis for defining a polymorphic isOurs for different
--   wallet types.
data PrefilterKey
    = PrefilterKeyHdRnd WalletId WalletDecrCredentials
    -- ^ for HdRnd wallets, the WalletId is based on the HdRootId
    -- and the credentials are derived from the EncryptedSecretKey
    | PrefilterKeyEOS V1.EosWalletId PublicKey
    -- ^ for EOS wallets, we need the root EosWalletId and the account public key

-- | Filter items for addresses given an isOurs predicate.
--   Returns the matching AddressId, which embeds the parent AccountId
--   discovered for the matching item.
--
-- TODO(@uroboros/ryan) `selectOwnAddresses` calls `decryptAddress`, which extracts
-- the AccountId from the Tx Attributes. This is not sufficient since it
-- doesn't actually _verify_ that the Tx belongs to the AccountId.
-- We need to add verification (see `deriveLvl2KeyPair`).
filterOurs
    :: IsOurs
    -> (item -> Address)      -- ^ address getter
    -> [item]                 -- ^ list to filter
    -> [(item, AddressId)]  -- ^ matching items
filterOurs ours selectAddr items
    = mapMaybe ours_ items
    where
        ours_ item = (item,) <$> ours (selectAddr item)

filterOursWithKey
    :: PrefilterKey
    -> (item -> Address)      -- ^ address getter
    -> [item]                 -- ^ list to filter
    -> [(item, AddressId)]  -- ^ matching items
filterOursWithKey prefKey
    = filterOurs (isOurs prefKey)

{-------------------------------------------------------------------------------
 Pre-filter Tx Inputs and Outputs to those that belong to the given Wallet.
 Includes prefiltering of Utxo.
-------------------------------------------------------------------------------}

-- | Prefilter the inputs and outputs of a resolved transaction.
--   Prefiltered inputs and outputs are indexed by accountId.
--   The output Utxo is extended with address summary information
--   This returns a list of TxMeta, because TxMeta also includes
--   AccountId information, so the same Tx may belong to multiple
--   Accounts.
prefilterTx
    :: IsOurs
    -> ResolvedTx
    -- prefiltered inputs, prefiltered output utxo, extended with address summary
    -> ( ( Map AccountId (Set (TxIn, TxId))
         , Map AccountId UtxoSummaryRaw)
       , [TxMeta])
prefilterTx ours tx
    = ((prefInps',prefOuts'),metas)
    where
        inps = toList (tx ^. rtxInputs  . fromDb)
        outs =         tx ^. rtxOutputs . fromDb

        (onlyOurInps,prefInps) = prefilterInputs ours inps
        (onlyOurOuts,prefOuts) = prefilterUtxoWithOurs  ours outs

        prefOuts' = Map.map (extendWithSummary (onlyOurInps,onlyOurOuts))
                            prefOuts
        txId = fst $ tx ^. rtxMeta . fromDb
        -- this Set.map does not change the number of elements because TxIn's are unique.
        prefInps' = map (Set.map (\(txin, _) -> (txin, txId))) prefInps

        (prefInCoins  :: (Map AccountId Coin)) = map (sumCoinsUnsafe . map snd . Set.toList) prefInps
        (prefOutCoins :: (Map AccountId Coin)) = map (\mp -> sumCoinsUnsafe $ map (toCoin . fst) mp) prefOuts'

        allAccounts = toList $ Map.keysSet prefInps' <> Map.keysSet prefOuts
        metas = map (\acc -> resolvedToTxMeta tx
                                (nothingToZero acc prefInCoins)
                                (nothingToZero acc prefOutCoins)
                                (onlyOurInps && onlyOurOuts)
                                (coerceHdAccountId acc))
                    allAccounts

-- | Prefilter the transaction with each isOurs predicate respectively and
--   combine the results.
--
-- NOTE: we can rely on a Monoidal fold here to combine the maps
-- 'Map HdAccountId a' since the accounts will be unique accross wallet keys.
-- The function decomposes a resolved block into input and output transactions and meta for given wallets
-- In case of input transactions the two kinds are differentiated:
-- (a) the input transactions belonging to some wallet
-- (b) the foreign transactions.
-- The foreign transactions are identified by picking the input transactions from the resolved one
-- that happen to be in foreign pending set.
prefilterTxForWallets
    :: [IsOurs]
    -> Map TxIn AccountId
    -> ResolvedTx
    -> ((Map AccountId (Set (TxIn, TxId), Set (TxIn, TxId))
        , Map AccountId UtxoSummaryRaw)
       , [TxMeta])
prefilterTxForWallets oursForWallets foreignPendingByTransaction tx =
    ((extend inputsE foreignInputsE, outputs),meta)
  where
    ((inputs,outputs),meta) = mconcat $ map ((flip prefilterTx) tx) oursForWallets

    txId :: TxId
    txId = fst $ tx ^. rtxMeta . fromDb

    --NOTE: to find the foreign inputs in the transaction, we need to look at _all_ the inputs, since they will not be present in the prefiltered inputs
    allInputs :: Set (TxIn, TxId)
    allInputs = Set.fromList $ map ((, txId) . fst) $ toList (tx ^. rtxInputs  . fromDb)

    foreignInputs :: Map AccountId (Set (TxIn, TxId))
    foreignInputs = Map.map (Set.map (, txId)) $ reindexByAccount $
                        Map.filterWithKey
                            (\txin _ -> Set.member (txin, txId) allInputs)
                            foreignPendingByTransaction

    inputsE, foreignInputsE :: Map AccountId (Set (TxIn, TxId), Set (TxIn, TxId))
    inputsE = Map.map (, Set.empty) inputs
    foreignInputsE =  Map.map (Set.empty,) foreignInputs

    extend
        :: Map AccountId (Set (TxIn, TxId), Set (TxIn, TxId))
        -> Map AccountId (Set (TxIn, TxId), Set (TxIn, TxId))
        -> Map AccountId (Set (TxIn, TxId), Set (TxIn, TxId))
    extend inputs_ foreignInputs_ =
        Map.unionWith (\inp fInp -> (fst inp, snd fInp)) inputs_ foreignInputs_

    reindexByAccount
        :: Map TxIn AccountId
        -> Map AccountId (Set TxIn)
    reindexByAccount byTxIn =
        Map.fromListWith Set.union $ Map.elems $ Map.mapWithKey f byTxIn
      where
          f :: TxIn -> AccountId -> (AccountId, Set TxIn)
          f txin accId = (accId, Set.singleton txin)

-- | Prefilter inputs of a transaction with the isOurs predicate
prefilterInputs
    :: IsOurs
    -> [(TxIn, ResolvedInput)]
    -> (Bool, Map AccountId (Set (TxIn,Coin)))
prefilterInputs ours inps
    = prefilterResolvedTxPairs ours mergeF inps
    where
        mergeF = Map.fromListWith Set.union . (map f)

        f ((txIn, out),addrId) = ( addrIdToAccountId addrId
                                 , Set.singleton (txIn, toCoin out))

-- | Prefilter utxo with a prefilter key
prefilterUtxoWithKey
    :: PrefilterKey
    -> Utxo
    -> Map AccountId (Utxo,[AddrWithId__])
prefilterUtxoWithKey prefKey@(PrefilterKeyHdRnd _ _) utxo
    = map toPrefilteredUtxo
          (snd $ prefilterUtxoWithOurs (isOurs prefKey) utxo)
prefilterUtxoWithKey _ _
    = error "TODO Implement for EOS"

-- | Prefilter utxo with an isOurs predicate
prefilterUtxoWithOurs
    :: IsOurs
    -> Utxo
    -> (Bool, Map AccountId UtxoWithAddrId__)
prefilterUtxoWithOurs ours utxo
    = prefilterResolvedTxPairs ours mergeF (Map.toList utxo)
    where
        mergeF = Map.fromListWith Map.union . (map f)

        f ((txIn, txOut),addrId) = (addrIdToAccountId addrId,
                                    Map.singleton txIn (txOut, addrId))

-- | Produce Utxo along with all (extended) addresses occurring in the Utxo
toPrefilteredUtxo
    :: UtxoWithAddrId__
    -> (Utxo,[AddrWithId__])
toPrefilteredUtxo utxoWithAddrs = (Map.fromList utxoL, addrs)
    where
        toUtxo (txIn,(txOutAux,_))         = (txIn,txOutAux)
        toAddr (_   ,(txOutAux,addressId)) = (addressId, txOutAddress . toaOut $ txOutAux)

        toSummary :: (TxIn,(TxOutAux,AddressId))
                  -> ((TxIn,TxOutAux), AddrWithId__)
        toSummary item = (toUtxo item, toAddr item)

        utxoSummary = map toSummary $ Map.toList utxoWithAddrs
        (utxoL, addrs) = unzip utxoSummary

-- | Prefilter resolved transaction pairs.
--   Also returns a Boolean indicating whether @all@ pairs are "ours"
prefilterResolvedTxPairs
    :: IsOurs
    -> ([((TxIn, TxOutAux), AddressId)] -> a)
    -> [(TxIn, TxOutAux)]
    -> (Bool, a)
prefilterResolvedTxPairs ours mergeF pairs
    = (onlyOurs, mergeF prefTxPairs)
    where
        selectAddr = txOutAddress . toaOut . snd
        prefTxPairs = filterOurs ours selectAddr pairs
        -- | if prefiltering excluded nothing, then all the pairs are "ours"
        onlyOurs = (length prefTxPairs == length pairs)

extendWithSummary
    :: (Bool, Bool)
    -- ^ Bools that indicate whether the inputs and outsputs are all "ours"
    -> Map TxIn (TxOutAux,AddressId)
    -- ^ Utxo extended with HdAddressId
    -> Map TxIn (TxOutAux,AddressSummary)
    -- ^ Utxo extended with AddressSummary
extendWithSummary (onlyOurInps,onlyOurOuts) utxoWithAddrId
    = Map.fromList $ mapMaybe toAddrSummary (Map.toList utxoWithAddrId)
    where
        toAddrSummary (txIn,(txOutAux,addressId))
            = case txIn of
                (TxInUtxo txId _) -> Just (txIn,(txOutAux,addrSummary txId))
                (TxInUnknown _ _) -> Nothing -- NOTE: we ignore addresses with 'unknown' inputs
            where
                addrSummary txId' = AddressSummary (txOutAddress . toaOut $ txOutAux)
                                                    addressId
                                                    txId'
                                                    onlyOurInps
                                                    onlyOurOuts

{-------------------------------------------------------------------------------
 Pre-filter a block of transactions, adorn each prefiltered block with block metadata
 and Transaction metadata.
-------------------------------------------------------------------------------}

prefilterBlockWithKeys
    :: [PrefilterKey]
    -> Map AccountId Pending
    -> ResolvedBlock
    -> (Map AccountId PrefilteredBlock__, [TxMeta])
prefilterBlockWithKeys prefKeys
    = prefilterBlockWithOurs (map isOurs prefKeys)

-- | Prefilter the transactions of a resolved block for the given wallets.
--
--   Returns prefiltered blocks indexed by HdAccountId.
prefilterBlockWithOurs
    :: [IsOurs]
    -> Map AccountId Pending
    -> ResolvedBlock
    -> (Map AccountId PrefilteredBlock__, [TxMeta])
prefilterBlockWithOurs oursForWallets foreignPendingByAccount block =
      (Map.fromList
    $ map (mkPrefBlock (block ^. rbContext) inpAll outAll)
    $ Set.toList accountIds
    , metas)
  where
    foreignPendingByTransaction :: Map TxIn AccountId
    foreignPendingByTransaction = reindexByTransaction $ Map.map Pending.txIns foreignPendingByAccount

    inps :: [Map AccountId (Set (TxIn, TxId), Set (TxIn, TxId))]
    outs :: [Map AccountId UtxoSummaryRaw]
    (ios, conMetas) = unzip $ map (prefilterTxForWallets oursForWallets foreignPendingByTransaction) (block ^. rbTxs)
    (inps, outs) = unzip ios
    metas = concat conMetas

    inpAll :: Map AccountId (Set (TxIn, TxId), Set (TxIn, TxId))
    outAll :: Map AccountId UtxoSummaryRaw
    inpAll = Map.unionsWith (\pair1 pair2 -> (Set.union (fst pair1) (fst pair2),Set.union (snd pair1) (fst pair2))) inps
    outAll = Map.unionsWith Map.union outs

    accountIds = Map.keysSet inpAll `Set.union` Map.keysSet outAll

    reindexByTransaction :: Map AccountId (Set TxIn) -> Map TxIn AccountId
    reindexByTransaction byAccount = Map.fromList $ Set.toList $ Set.unions $ Map.elems $ Map.mapWithKey f byAccount
        where
            f :: AccountId -> Set TxIn -> Set (TxIn, AccountId)
            f accId = Set.map (, accId)

mkPrefBlock
    :: BlockContext
    -> Map AccountId (Set (TxIn, TxId), Set (TxIn, TxId))
    -> Map AccountId (Map TxIn (TxOutAux, AddressSummary))
    -> AccountId
    -> (AccountId, PrefilteredBlock__)
mkPrefBlock context inps outs accId = (accId, PrefilteredBlock__ {
        pfbInputs__         = walletInps'
      , pfbForeignInputs__  = foreignInps'
      , pfbOutputs__        = outs'
      , pfbAddrs__          = addrs'
      , pfbMeta__           = blockMeta'
      , pfbContext__        = context
      })
    where
        fromAddrSummary :: AddressSummary -> AddrWithId__
        fromAddrSummary AddressSummary{..} = (addrSummaryId,addrSummaryAddr)

        byAccountId accId'' def dict = fromMaybe def $ Map.lookup accId'' dict

        walletInps = Map.map fst $
                     Map.filter (not . Set.null . fst) inps
        foreignInps = Map.map snd $
                      Map.filter (not . Set.null . snd) inps
        walletInps'           = Set.map fst  $ byAccountId accId Set.empty walletInps
        foreignInps'          = Set.map fst  $ byAccountId accId Set.empty foreignInps

        allInps = (Map.map fst inps)
        inpsWithtxId = byAccountId accId Set.empty allInps
        -- this Set.map may reduce the number of elements. But this is okey, since we
        -- don't care about repetitions on txIds.

        txIdsFromInputs = Set.map snd inpsWithtxId
        (outs' , addrsFromOutputs) = fromUtxoSummary (byAccountId accId Map.empty outs)

        addrs'    = nub $ map fromAddrSummary addrsFromOutputs
        blockMeta' = mkBlockMeta (context ^. bcSlotId . fromDb) addrsFromOutputs txIdsFromInputs

mkBlockMeta :: SlotId -> [AddressSummary] -> Set TxId -> LocalBlockMeta
mkBlockMeta slotId addrs_ txIds = LocalBlockMeta BlockMeta{..}
    where
        txIds' = (Set.toList txIds) <> (nub $ map addrSummaryTxId addrs_)

        indexedAddrs = indexByAddr addrs_

        _blockMetaSlotId      = InDb . Map.fromList . map (,slotId) $ txIds'
        _blockMetaAddressMeta = Map.map mkAddressMeta indexedAddrs

-- | This function is called once for each address found in a particular block of
--   transactions. The collection of address summaries passed to this function
--   corresponds to occurances of a given address in transactions in a block.
--   Since the collection was made by indexing the block of transactions by address,
--   we can be sure that the address occurs in at least one transaction and
--   hence that there are at least one or more summaries passed to this function
--   for a given address.
mkAddressMeta :: NE.NonEmpty AddressSummary -> AddressMeta
mkAddressMeta addrs
    = AddressMeta isUsed isChange
    where
        occurs = NE.length addrs

        -- An address is considered "used" if
        -- (1) it is "our" address: we are only dealing with prefiltered transactions
        --     here and can at this stage assume that the address is indeed "ours".
        -- (2) the transaction is confirmed: we are dealing here with transactions that
        --     appear in a block and can assume that they are confirmed.
        isUsed = True

        -- An address is considered "change" if
        -- (1) it is "our" address: as with `isUsed` above, we can assume the address is "ours"
        -- (2) the address occurs in exactly one transaction in this block
        -- (3) for the (single) transaction in which this address appears, the
        --     outputs must not all be to "our" addresses (the transaction must have
        --     an output to at least one address that is not "ours")
        -- (4) all the inputs of the transaction in which this address appears
        --     must be "ours"
        isChange = (occurs == 1)                    -- (2)
                    && addrSummaryOnlyOurInps       -- (3)
                    && not addrSummaryOnlyOurOuts   -- (4)
            where AddressSummary{..} = NE.head addrs

-- | Index the list of address summaries by Address.
--   NOTE: Since there will be at least one AddressSummary per Address,
--   we can safely use NE.fromList.
indexByAddr :: [AddressSummary] -> Map (InDb Address) (NE.NonEmpty AddressSummary)
indexByAddr addrs =
    Map.map NE.fromList (Map.fromListWith (++) addrs')
    where
        fromAddrSummary addrSummary = (InDb (addrSummaryAddr addrSummary), [addrSummary])
        addrs' = map fromAddrSummary addrs

fromUtxoSummary :: Map TxIn (TxOutAux,AddressSummary)
                -> (Utxo,[AddressSummary])
fromUtxoSummary summary = (Map.fromList utxoL, addrs)
    where
        toUtxo (txIn,(txOutAux,_))           = (txIn,txOutAux)
        toAddr (_   ,(_       ,addrSummary)) = addrSummary

        unpackSummary item = (toUtxo item, toAddr item)

        (utxoL, addrs) = unzip $ map unpackSummary (Map.toList summary)

{-------------------------------------------------------------------------------
  Pretty-printing
-------------------------------------------------------------------------------}

instance Buildable PrefilteredBlock where
  build PrefilteredBlock{..} = bprint
    ( "PrefilteredBlock "
    % "{ inputs:  " % listJson
    % "{ foreignInputs:  " % listJson
    % ", outputs: " % mapJson
    % "}"
    )
    (Set.toList pfbInputs)
    (Set.toList pfbForeignInputs)
    pfbOutputs

{-------------------------------------------------------------------------------
  Helpers for Prefiltering HdRnd wallets
  TODO move out of this module
-------------------------------------------------------------------------------}

addressIdFromMeta :: WalletId -> V1.WAddressMeta -> HdAddressId
addressIdFromMeta (WalletIdEOS _) _ = error "WalletId must represent an HdRnd wallet"
addressIdFromMeta (WalletIdHdRnd rootId) meta'
  = HdAddressId accountId addressIx
  where
    accountIx = HdAccountIx (V1._wamAccountIndex meta')
    accountId = HdAccountId rootId accountIx
    addressIx = HdAddressIx (V1._wamAddressIndex meta')

toHdRndPrefKey'
    :: NetworkMagic
    -> (WalletId, EncryptedSecretKey)
    -> PrefilterKey
toHdRndPrefKey' nm (wid,esk)
    = PrefilterKeyHdRnd wid (eskToWalletDecrCredentials nm esk)

toHdRndPrefKey
    :: NetworkMagic
    -> HdRootId
    -> EncryptedSecretKey
    -> PrefilterKey
toHdRndPrefKey nm rootId esk
    = toHdRndPrefKey' nm (WalletIdHdRnd rootId,esk)

toHdRndPrefKeys
    :: NetworkMagic
    -> [(WalletId, EncryptedSecretKey)]
    -> [PrefilterKey]
toHdRndPrefKeys nm rawKeys
    = map (toHdRndPrefKey' nm) rawKeys

{-------------------------------------------------------------------------------
  Temporary HdRnd wrappers for Prefiltering API (to limit the impact of these
  changes on the rest of the wallet Kernel)
  These wrappers translate from the abstract Id types back to HdRnd id types
  to maintain compatibility with the rest of the Kernel.

  TODO deprecate these wrappers for the generic underlying functions (this will
  require generalising the calling code to use abstract AccountId/AddressId)
-------------------------------------------------------------------------------}

filterOursHdRnd
    :: PrefilterKey
    -> (a -> Address)      -- ^ address getter
    -> [a]                 -- ^ list to filter
    -> [(a, HdAddressId)]  -- ^ matching items
filterOursHdRnd prefKey selectAddr rtxs
    = (filterOursWithKey prefKey selectAddr rtxs)
        & map (over _2 coerceHdAddressId)

toPrefilteredUtxoHdRnd
    :: UtxoWithAddrId
    -> (Utxo,[AddrWithId])
toPrefilteredUtxoHdRnd utxoWithAddrs
    = (toPrefilteredUtxo $ coerceUtxoWithAddrId' utxoWithAddrs)
        & _2 %~ map coerceAddrWithId

-- | Prefilter utxo using wallet key
prefilterUtxoHdRnd
    :: PrefilterKey
    -> Utxo
    -> Map HdAccountId (Utxo,[AddrWithId])
prefilterUtxoHdRnd prefKey@(PrefilterKeyHdRnd _ _) utxo
    = Map.mapKeys coerceHdAccountId . (map (over _2 (map coerceAddrWithId)))
        $ prefilterUtxoWithKey prefKey utxo
prefilterUtxoHdRnd _ _
    = error "prefilterUtxoHdRnd - Invalid PrefilterKey"

prefilterBlockHdRnd
    :: [PrefilterKey]
    -> Map HdAccountId Pending
    -> ResolvedBlock
    -> (Map HdAccountId PrefilteredBlock, [TxMeta])
prefilterBlockHdRnd prefKeys foreignPendingByAccount block
    = (prefilterBlockWithKeys prefKeys foreignPendingByAccount' block)
        & _1 %~ (Map.mapKeys coerceHdAccountId) . (map coercePrefilteredBlock)
    where
        foreignPendingByAccount' = Map.mapKeys AccountIdHdRnd foreignPendingByAccount

{-------------------------------------------------------------------------------
 isOurs defined polymorphically for different wallet types

 This function enables polymorphic prefiltering of transactions, UtXo and blocks
 -------------------------------------------------------------------------------}

-- | Check if the address was derived from the given WalletKey.
--   Returns the matching HdAddressIx and the HdAccountId in which the
--   address was discovered.
--
-- TODO(@uroboros/ryan) `decryptAddress` extracts the AccountId from
-- the Tx Attributes. This is not sufficient since it doesn't actually
-- _verify_ that the Tx belongs to the AccountId.
--
-- We need to add verification (see `deriveLvl2KeyPair`).
isOursHdRnd
    :: (WalletId, WalletDecrCredentials)
    -> IsOurs
isOursHdRnd (wid,creds) address
    = (AddressIdHdRnd . addressIdFromMeta wid) <$> decryptAddress creds address

isOurs
    :: PrefilterKey
    -> IsOurs
isOurs (PrefilterKeyHdRnd wid creds)
    = isOursHdRnd (wid,creds)
isOurs (PrefilterKeyEOS _ _)
    = error "TODO Implement EOS isOurs"

{-------------------------------------------------------------------------------
  Scaffolding to limit impact of these changes
   TODO @uroboros #34 remove scaffolding
-------------------------------------------------------------------------------}

coerceHdAccountId :: AccountId -> HdAccountId
coerceHdAccountId (AccountIdHdRnd accountId) = accountId
coerceHdAccountId _ = error "coerceHdAccountId"

-- TODO: this will be removed in #34, tmp stopgap to limit impact of PR-part1
coerceHdAddressId :: AddressId -> HdAddressId
coerceHdAddressId (AddressIdHdRnd addr) = addr
coerceHdAddressId _ = error "coerceHdAddressId"

coerceUtxoWithAddrId' :: UtxoWithAddrId -> UtxoWithAddrId__
coerceUtxoWithAddrId' utxo = map f utxo
    where
        f :: (TxOutAux,HdAddressId) -> (TxOutAux,AddressId)
        f (out,addrId) = (out, AddressIdHdRnd addrId)

coerceAddrWithId :: AddrWithId__ -> AddrWithId
coerceAddrWithId addr = addr & _1 %~ coerceHdAddressId

coercePrefilteredBlock :: PrefilteredBlock__ -> PrefilteredBlock
coercePrefilteredBlock PrefilteredBlock__{..}
    = PrefilteredBlock {
        pfbInputs = pfbInputs__
      , pfbForeignInputs = pfbForeignInputs__
      , pfbOutputs = pfbOutputs__
      , pfbAddrs = map coerceAddrWithId pfbAddrs__
      , pfbMeta = pfbMeta__
      , pfbContext = pfbContext__
    }
