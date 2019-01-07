{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE TypeFamilies        #-}

module Cardano.Wallet.Kernel.PrefilterTx
       ( PrefilteredBlock(..)
       , emptyPrefilteredBlock
       , prefilterBlock
       , prefilterUtxo
       , UtxoWithAddrId
       , prefilterUtxo'
       , filterOurs
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
import           Pos.Crypto (EncryptedSecretKey)

import           Cardano.Wallet.Kernel.DB.BlockContext
import           Cardano.Wallet.Kernel.DB.BlockMeta
import           Cardano.Wallet.Kernel.DB.HdWallet
import           Cardano.Wallet.Kernel.DB.HdWallet.Create (initHdAddress)
import           Cardano.Wallet.Kernel.DB.InDb (InDb (..), fromDb)
import           Cardano.Wallet.Kernel.DB.Resolved (ResolvedBlock,
                     ResolvedInput, ResolvedTx, rbContext, rbTxs,
                     resolvedToTxMeta, rtxInputs, rtxMeta, rtxOutputs)
import           Cardano.Wallet.Kernel.DB.Spec.Pending (Pending)
import qualified Cardano.Wallet.Kernel.DB.Spec.Pending as Pending
import           Cardano.Wallet.Kernel.DB.TxMeta.Types
import           Cardano.Wallet.Kernel.Util.Core

{-------------------------------------------------------------------------------
 Pre-filter Tx Inputs and Outputs; pre-filter a block of transactions.
-------------------------------------------------------------------------------}

-- | Prefiltered block
--
-- A prefiltered block is a block that contains only inputs and outputs from
-- the block that are relevant to the wallet.
data PrefilteredBlock = PrefilteredBlock {
      -- | Relevant inputs
      pfbInputs  :: !(Set TxIn)

      -- | Relevant outputs
    , pfbOutputs :: !Utxo

      -- | all output addresses present in the Utxo
    , pfbAddrs   :: ![HdAddress]

      -- | Prefiltered block metadata
    , pfbMeta    :: !LocalBlockMeta

      -- | Block context
    , pfbContext :: !BlockContext
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
    , pfbOutputs        = Map.empty
    , pfbAddrs          = []
    , pfbMeta           = mempty
    , pfbContext        = context
    }

-- | Summary of an address as it appears in a transaction.
--   NOTE: Since an address can occur in multiple transactions, there could be
--   multiple valid summaries for an address.
data AddressSummary = AddressSummary {
      addrSummaryAddr        :: Address
    ,
      addrSummaryId          :: HdAddressId
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
prefilterTx :: (HdRootId, EncryptedSecretKey)
            -> ResolvedTx
            -> ((Map HdAccountId (Set (TxIn, TxId))
              , Map HdAccountId UtxoSummaryRaw)
              , [TxMeta])
            -- ^ prefiltered inputs, prefiltered output utxo, extended with address summary
prefilterTx wKey tx = ((prefInps',prefOuts'),metas)
    where
        inps = toList (tx ^. rtxInputs  . fromDb)
        outs =         tx ^. rtxOutputs . fromDb

        (onlyOurInps,prefInps) = prefilterInputs wKey inps
        (onlyOurOuts,prefOuts) = prefilterUtxo'  wKey outs

        prefOuts' = Map.map (extendWithSummary (onlyOurInps,onlyOurOuts))
                            prefOuts
        txId = fst $ tx ^. rtxMeta . fromDb
        -- this Set.map does not change the number of elements because TxIn's are unique.
        prefInps' = map (Set.map (\(txin, _) -> (txin, txId))) prefInps

        (prefInCoins  :: (Map HdAccountId Coin)) = map (sumCoinsUnsafe . map snd . Set.toList) prefInps
        (prefOutCoins :: (Map HdAccountId Coin)) = map (\mp -> sumCoinsUnsafe $ map (toCoin . fst) mp) prefOuts'

        allAccounts = toList $ Map.keysSet prefInps' <> Map.keysSet prefOuts
        metas = map (\acc -> resolvedToTxMeta tx
            (nothingToZero acc prefInCoins)
            (nothingToZero acc prefOutCoins)
            (onlyOurInps && onlyOurOuts) acc) allAccounts

-- | Prefilter the transaction with each wallet key respectively and
--   combine the results.
--
-- NOTE: we can rely on a Monoidal fold here to combine the maps
-- 'Map HdAccountId a' since the accounts will be unique accross wallet keys.
prefilterTxForWallets
    :: [(HdRootId, EncryptedSecretKey)]
    -> ResolvedTx
    -> ((Map HdAccountId (Set (TxIn, TxId))
        , Map HdAccountId UtxoSummaryRaw)
       , [TxMeta])
prefilterTxForWallets wKeys tx =
    mconcat $ map ((flip prefilterTx) tx) wKeys

-- | Prefilter inputs of a transaction
prefilterInputs :: (HdRootId, EncryptedSecretKey)
                -> [(TxIn, ResolvedInput)]
                -> (Bool, Map HdAccountId (Set (TxIn,Coin)))
prefilterInputs wKey inps
    = prefilterResolvedTxPairs wKey mergeF inps
    where
        mergeF = Map.fromListWith Set.union . (map f)

        f ((txIn, out),addrId) = (addrId ^. hdAddressIdParent,
                                     Set.singleton (txIn, toCoin out))

-- | Prefilter utxo using wallet key
prefilterUtxo' :: (HdRootId, EncryptedSecretKey) -> Utxo -> (Bool, Map HdAccountId UtxoWithAddrId)
prefilterUtxo' wKey utxo
    = prefilterResolvedTxPairs wKey mergeF (Map.toList utxo)
    where
        mergeF = Map.fromListWith Map.union . (map f)

        f ((txIn, txOut),addrId) = (addrId ^. hdAddressIdParent,
                                    Map.singleton txIn (txOut, addrId))

-- | Prefilter utxo using walletId and esk
prefilterUtxo :: HdRootId -> EncryptedSecretKey -> Utxo -> Map HdAccountId (Utxo,[HdAddress])
prefilterUtxo rootId esk utxo = map toPrefilteredUtxo prefUtxo
    where
        (_,prefUtxo) = prefilterUtxo' (rootId, esk) utxo

-- | Produce Utxo along with all (extended) addresses occurring in the Utxo
toPrefilteredUtxo :: UtxoWithAddrId -> (Utxo,[HdAddress])
toPrefilteredUtxo utxoWithAddrs = (Map.fromList utxoL, addrs)
    where
        toUtxo (txIn,(txOutAux,_))         = (txIn,txOutAux)
        toAddr (_   ,(txOutAux,addressId)) = initHdAddress addressId (txOutAddress . toaOut $ txOutAux)

        toSummary :: (TxIn,(TxOutAux,HdAddressId))
                  -> ((TxIn,TxOutAux), HdAddress)
        toSummary item = (toUtxo item, toAddr item)

        utxoSummary = map toSummary $ Map.toList utxoWithAddrs
        (utxoL, addrs) = unzip utxoSummary

-- | Prefilter resolved transaction pairs.
--   Also returns a Boolean indicating whether @all@ pairs are "ours"
prefilterResolvedTxPairs :: (HdRootId, EncryptedSecretKey)
                         -> ([((TxIn, TxOutAux), HdAddressId)] -> a)
                         -> [(TxIn, TxOutAux)]
                         -> (Bool, a)
prefilterResolvedTxPairs wKey mergeF pairs
    = (onlyOurs, mergeF prefTxPairs)
    where
        selectAddr = txOutAddress . toaOut . snd
        prefTxPairs = filterOurs wKey selectAddr pairs
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
filterOurs :: (HdRootId, EncryptedSecretKey)
           -> (a -> Address)      -- ^ address getter
           -> [a]                 -- ^ list to filter
           -> [(a, HdAddressId)]  -- ^ matching items
filterOurs creds selectAddr rtxs = flip evalState [creds] $ do
    fmap catMaybes $ forM rtxs $ \a -> runMaybeT $ do
        addr <- MaybeT $ state $ isOurs $ selectAddr a
        return (a, addr ^. hdAddressId)

extendWithSummary :: (Bool, Bool)
                  -- ^ Bools that indicate whether the inputs and outsputs are all "ours"
                  -> Map TxIn (TxOutAux,HdAddressId)
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

-- | Prefilter the transactions of a resolved block for the given wallets.
--
--   Returns prefiltered blocks indexed by HdAccountId.
prefilterBlock
    :: Map HdAccountId Pending
    -> ResolvedBlock
    -> [(HdRootId, EncryptedSecretKey)]
    -> (Map HdAccountId PrefilteredBlock, [TxMeta])
prefilterBlock foreignInps block wKeys =
      (Map.fromList
    $ map (mkPrefBlock (block ^. rbContext) foreignInps inpAll outAll)
    $ Set.toList accountIds
    , concat metas)
  where
    (ios, metas) = unzip $ map (prefilterTxForWallets wKeys) (block ^. rbTxs)
    inps :: [Map HdAccountId (Set (TxIn, TxId))]
    outs :: [Map HdAccountId UtxoSummaryRaw]
    (inps, outs) = unzip ios
    inpAll :: Map HdAccountId (Set (TxIn, TxId))
    inpAll = Map.unionsWith Set.union inps
    outAll :: Map HdAccountId UtxoSummaryRaw
    outAll = Map.unionsWith Map.union outs

    accountIds = Map.keysSet inpAll `Set.union` Map.keysSet outAll


mkPrefBlock :: BlockContext
            -> Map HdAccountId Pending
            -> Map HdAccountId (Set (TxIn, TxId))
            -> Map HdAccountId (Map TxIn (TxOutAux, AddressSummary))
            -> HdAccountId
            -> (HdAccountId, PrefilteredBlock)
mkPrefBlock context foreignInps inps outs accId = (accId, PrefilteredBlock {
        pfbInputs         = inps'
      , pfbOutputs        = outs'
      , pfbAddrs          = addrs'
      , pfbMeta           = blockMeta'
      , pfbContext        = context
      })
    where
        fromAddrSummary :: AddressSummary -> HdAddress
        fromAddrSummary AddressSummary{..} = initHdAddress addrSummaryId addrSummaryAddr

        byAccountId k def = fromMaybe def . Map.lookup k

        -- this Set.map may reduce the number of elements. But this is okey, since we
        -- don't care about repetitions on txIds.
        inps' = Set.map fst (byAccountId accId Set.empty inps) <> (maybe mempty Pending.txIns $ Map.lookup accId foreignInps)
        inpsWithtxId = byAccountId accId Set.empty inps
        txIdsFromInputs = Set.map snd inpsWithtxId
        (outs' , addrsFromOutputs) = fromUtxoSummary (byAccountId accId Map.empty outs)
        addrs' = nub $ map fromAddrSummary addrsFromOutputs
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
mkAddressMeta _
    = AddressMeta isUsed
    where
        -- An address is considered "used" if
        -- (1) it is "our" address: we are only dealing with prefiltered transactions
        --     here and can at this stage assume that the address is indeed "ours".
        -- (2) the transaction is confirmed: we are dealing here with transactions that
        --     appear in a block and can assume that they are confirmed.
        isUsed = True

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
    % ", outputs: " % mapJson
    % "}"
    )
    (Set.toList pfbInputs)
    pfbOutputs
