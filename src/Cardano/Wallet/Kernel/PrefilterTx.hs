{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE TypeFamilies        #-}

module Cardano.Wallet.Kernel.PrefilterTx
       ( PrefilteredBlock(..)
       , emptyPrefilteredBlock
       , AddrWithId
       , prefilterBlock
       , prefilterUtxo
       , UtxoWithAddrId
       , prefilterUtxo'
       , filterOurs
       , toHdAddressId
       , WalletKey
       , toPrefilteredUtxo
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
import           Pos.Crypto (EncryptedSecretKey)

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
                     eskToWalletDecrCredentials, selectOwnAddresses)
import           Cardano.Wallet.Kernel.Types (AccountId (..), AddressId (..),
                    WalletId (..), addrIdToAccountId)
import           Cardano.Wallet.Kernel.Util.Core

{-------------------------------------------------------------------------------
 Pre-filter Tx Inputs and Outputs; pre-filter a block of transactions.
-------------------------------------------------------------------------------}

-- | Address extended with an HdAddressId, which embeds information that places
--   the Address in the context of the Wallet/Accounts/Addresses hierarchy.
type AddrWithId = (HdAddressId,Address)

-- TODO @uroboros #34-part2 this must replace AddrWithId
type AddrWithId__ = (AddressId,Address)

-- | Prefiltered block
--
-- A prefiltered block is a block that contains only inputs and outputs from
-- the block that are relevant to the wallet.
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

-- TODO @uroboros #34-part2 this must replace PrefilteredBlock
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

type WalletKey = (WalletId, WalletDecrCredentials)

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
type UtxoWithAddrId = Map TxIn (TxOutAux,HdAddressId)

-- TODO @uroboros #34-part2 this must replace UtxoWithAddrId
type UtxoWithAddrId__ = Map TxIn (TxOutAux,AddressId)

-- | Extended Utxo where each output is paired with an AddressSummary. Provides
--   the required metadata for computing address meta data for BlockMeta.
type UtxoSummaryRaw = Map TxIn (TxOutAux,AddressSummary)

{-------------------------------------------------------------------------------
 Pre-filter Tx Inputs and Outputs to those that belong to the given Wallet.
-------------------------------------------------------------------------------}

-- | Prefilter the inputs and outputs of a resolved transaction.
--   Prefiltered inputs and outputs are indexed by accountId.
--   The output Utxo is extended with address summary information
--   This returns a list of TxMeta, because TxMeta also includes
--   AccountId information, so the same Tx may belong to multiple
--   Accounts.
prefilterTx :: WalletKey
            -> ResolvedTx
            -> ((Map AccountId (Set (TxIn, TxId))
              , Map AccountId UtxoSummaryRaw)
              , [TxMeta])
            -- ^ prefiltered inputs, prefiltered output utxo, extended with address summary
prefilterTx wKey tx = ((prefInps',prefOuts'),metas)
    where
        inps = toList (tx ^. rtxInputs  . fromDb)
        outs =         tx ^. rtxOutputs . fromDb

        (onlyOurInps,prefInps) = prefilterInputs wKey inps
        (onlyOurOuts,prefOuts) = prefilterUtxoWithKey__  wKey outs

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
                                (forceToHdAccountId acc))
                    allAccounts

-- | Prefilter the transaction with each wallet key respectively and
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
    :: [WalletKey]
    -> Map TxIn AccountId
    -> ResolvedTx
    -> ((Map AccountId (Set (TxIn, TxId), Set (TxIn, TxId))
        , Map AccountId UtxoSummaryRaw)
       , [TxMeta])
prefilterTxForWallets wKeys foreignPendingByTransaction tx =
    ((extend inputsE foreignInputsE, outputs),meta)
  where
    ((inputs,outputs),meta) = mconcat $ map ((flip prefilterTx) tx) wKeys

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


-- | Prefilter inputs of a transaction
prefilterInputs :: WalletKey
                -> [(TxIn, ResolvedInput)]
                -> (Bool, Map AccountId (Set (TxIn,Coin)))
prefilterInputs wKey inps
    = prefilterResolvedTxPairs wKey mergeF inps
    where
        mergeF = Map.fromListWith Set.union . (map f)

        f ((txIn, out),addrId) = ( addrIdToAccountId addrId
                                 , Set.singleton (txIn, toCoin out))

-- | Prefilter utxo using wallet key
prefilterUtxo' :: WalletKey -> Utxo -> (Bool, Map HdAccountId UtxoWithAddrId)
prefilterUtxo' wKey utxo
    = (prefilterUtxoWithKey__ wKey utxo)
        & _2 %~ Map.mapKeys forceToHdAccountId . (map coerceUtxoWithAddrId)

-- | Prefilter utxo using walletId and esk
prefilterUtxo :: NetworkMagic -> HdRootId -> EncryptedSecretKey -> Utxo -> Map HdAccountId (Utxo,[AddrWithId])
prefilterUtxo nm rootId esk utxo
    = Map.mapKeys forceToHdAccountId
        $ map f (prefilterUtxo__ nm rootId esk utxo)
    where
        f x = x & _2 %~ map coerceAddrWithId

toPrefilteredUtxo :: UtxoWithAddrId -> (Utxo,[AddrWithId])
toPrefilteredUtxo utxoWithAddrs
    = (toPrefilteredUtxo__ $ coerceUtxoWithAddrId' utxoWithAddrs)
        & _2 %~ map coerceAddrWithId

-- | Prefilter utxo using wallet key
-- TODO @uroboros #34-part2 this must replace prefilterUtxo'
prefilterUtxoWithKey__
    :: WalletKey
    -> Utxo
    -> (Bool, Map AccountId UtxoWithAddrId__)
prefilterUtxoWithKey__ wKey utxo
    = prefilterResolvedTxPairs wKey mergeF (Map.toList utxo)
    where
        mergeF = Map.fromListWith Map.union . (map f)

        f ((txIn, txOut),addrId) = (addrIdToAccountId addrId,
                                    Map.singleton txIn (txOut, addrId))

-- | Prefilter utxo using walletId and esk
-- TODO @uroboros #34-part2 this must replace prefilterUtxo
prefilterUtxo__
    :: NetworkMagic
    -> HdRootId
    -> EncryptedSecretKey
    -> Utxo
    -> Map AccountId (Utxo,[AddrWithId__])
prefilterUtxo__ nm rootId esk utxo
    = map toPrefilteredUtxo__ prefUtxo
    where
        prefUtxo = snd $ prefilterUtxoWithKey__ (mkWalletKey nm rootId esk) utxo

mkWalletKey
    :: NetworkMagic
    -> HdRootId
    -> EncryptedSecretKey
    -> WalletKey
mkWalletKey nm rootId esk
    = (WalletIdHdRnd rootId, eskToWalletDecrCredentials nm esk)

-- | Produce Utxo along with all (extended) addresses occurring in the Utxo
-- TODO @uroboros #34-part2 this must replace toPrefilteredUtxo
toPrefilteredUtxo__ :: UtxoWithAddrId__ -> (Utxo,[AddrWithId__])
toPrefilteredUtxo__ utxoWithAddrs = (Map.fromList utxoL, addrs)
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
prefilterResolvedTxPairs :: WalletKey
                         -> ([((TxIn, TxOutAux), AddressId)] -> a)
                         -> [(TxIn, TxOutAux)]
                         -> (Bool, a)
prefilterResolvedTxPairs wKey mergeF pairs
    = (onlyOurs, mergeF prefTxPairs)
    where
        selectAddr = txOutAddress . toaOut . snd
        prefTxPairs = filterOurs__ wKey selectAddr pairs
        -- | if prefiltering excluded nothing, then all the pairs are "ours"
        onlyOurs = (length prefTxPairs == length pairs)

-- | Filter items for addresses that were derived from the given WalletKey.
--   Returns the matching HdAddressId, which embeds the parent HdAccountId
--   discovered for the matching item.
--
-- TODO(@uroboros/ryan) `selectOwnAddresses` calls `decryptAddress`, which extracts
-- the AccountId from the Tx Attributes. This is not sufficient since it
-- doesn't actually _verify_ that the Tx belongs to the AccountId.
-- We need to add verification (see `deriveLvl2KeyPair`).
filterOurs__ :: WalletKey
           -> (a -> Address)      -- ^ address getter
           -> [a]                 -- ^ list to filter
           -> [(a, AddressId)]  -- ^ matching items
filterOurs__ (wid,wdc) selectAddr rtxs
    = map f $ selectOwnAddresses wdc selectAddr rtxs
    where f (addr,meta) = (addr, AddressIdHdRnd (toHdAddressId wid meta))

filterOurs :: WalletKey
           -> (a -> Address)      -- ^ address getter
           -> [a]                 -- ^ list to filter
           -> [(a, HdAddressId)]  -- ^ matching items
filterOurs (wid,wdc) selectAddr rtxs
    = map f $ selectOwnAddresses wdc selectAddr rtxs
    where f (addr,meta) = (addr, toHdAddressId wid meta)

-- TODO (@mn): move this into Util or something
toHdAddressId :: WalletId -> V1.WAddressMeta -> HdAddressId
toHdAddressId (WalletIdEOS _) _ = error "WalletId must represent an HdRnd wallet"
toHdAddressId (WalletIdHdRnd rootId) meta' = HdAddressId accountId addressIx
  where
    accountIx = HdAccountIx (V1._wamAccountIndex meta')
    accountId = HdAccountId rootId accountIx
    addressIx = HdAddressIx (V1._wamAddressIndex meta')

extendWithSummary :: (Bool, Bool)
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

prefilterBlock
    :: NetworkMagic
    -> Map HdAccountId Pending
    -> ResolvedBlock
    -> [(WalletId, EncryptedSecretKey)]
    -> (Map HdAccountId PrefilteredBlock, [TxMeta])
prefilterBlock nm foreignPendingByAccount block rawKeys
    = (prefilterBlock__ nm foreignPendingByAccount' block rawKeys)
        & _1 %~ (Map.mapKeys forceToHdAccountId) . (map coercePrefilteredBlock)
    where
        foreignPendingByAccount' = Map.mapKeys AccountIdHdRnd foreignPendingByAccount

-- | Prefilter the transactions of a resolved block for the given wallets.
--
--   Returns prefiltered blocks indexed by HdAccountId.
prefilterBlock__
    :: NetworkMagic
    -> Map AccountId Pending
    -> ResolvedBlock
    -> [(WalletId, EncryptedSecretKey)]
    -> (Map AccountId PrefilteredBlock__, [TxMeta])
prefilterBlock__ nm foreignPendingByAccount block rawKeys =
      (Map.fromList
    $ map (mkPrefBlock (block ^. rbContext) inpAll outAll)
    $ Set.toList accountIds
    , metas)
  where
    wKeys :: [WalletKey]
    wKeys = map toWalletKey rawKeys

    foreignPendingByTransaction :: Map TxIn AccountId
    foreignPendingByTransaction = reindexByTransaction $ Map.map Pending.txIns foreignPendingByAccount

    inps :: [Map AccountId (Set (TxIn, TxId), Set (TxIn, TxId))]
    outs :: [Map AccountId UtxoSummaryRaw]
    (ios, conMetas) = unzip $ map (prefilterTxForWallets wKeys foreignPendingByTransaction) (block ^. rbTxs)
    (inps, outs) = unzip ios
    metas = concat conMetas

    inpAll :: Map AccountId (Set (TxIn, TxId), Set (TxIn, TxId))
    outAll :: Map AccountId UtxoSummaryRaw
    inpAll = Map.unionsWith (\pair1 pair2 -> (Set.union (fst pair1) (fst pair2),Set.union (snd pair1) (fst pair2))) inps
    outAll = Map.unionsWith Map.union outs

    accountIds = Map.keysSet inpAll `Set.union` Map.keysSet outAll

    toWalletKey :: (WalletId, EncryptedSecretKey) -> WalletKey
    toWalletKey (wid, esk) = (wid, eskToWalletDecrCredentials nm esk)

    reindexByTransaction :: Map AccountId (Set TxIn) -> Map TxIn AccountId
    reindexByTransaction byAccount = Map.fromList $ Set.toList $ Set.unions $ Map.elems $ Map.mapWithKey f byAccount
        where
            f :: AccountId -> Set TxIn -> Set (TxIn, AccountId)
            f accId = Set.map (, accId)


mkPrefBlock :: BlockContext
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
  Scaffolding to limit impact of these changes
  TODO @uroboros #34 remove scaffolding in #34
-------------------------------------------------------------------------------}

forceToHdAccountId :: AccountId -> HdAccountId
forceToHdAccountId (AccountIdHdRnd accountId) = accountId
forceToHdAccountId _ = error "forceToHdAccountId"

-- TODO: this will be removed in #34, tmp stopgap to limit impact of PR-part1
forceToHdAddressId :: AddressId -> HdAddressId
forceToHdAddressId (AddressIdHdRnd addr) = addr
forceToHdAddressId _ = error "forceToHdAddressId"

coerceUtxoWithAddrId :: UtxoWithAddrId__ -> UtxoWithAddrId
coerceUtxoWithAddrId utxo = map f utxo
    where
        f :: (TxOutAux,AddressId) -> (TxOutAux,HdAddressId)
        f (out,addrId) = (out, forceToHdAddressId addrId)

coerceUtxoWithAddrId' :: UtxoWithAddrId -> UtxoWithAddrId__
coerceUtxoWithAddrId' utxo = map f utxo
    where
        f :: (TxOutAux,HdAddressId) -> (TxOutAux,AddressId)
        f (out,addrId) = (out, AddressIdHdRnd addrId)

coerceAddrWithId :: AddrWithId__ -> AddrWithId
coerceAddrWithId addr = addr & _1 %~ forceToHdAddressId

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
