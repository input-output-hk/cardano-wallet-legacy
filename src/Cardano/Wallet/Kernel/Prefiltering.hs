{-# LANGUAGE LambdaCase #-}

module Cardano.Wallet.Kernel.Prefiltering
    ( -- * Types
      PrefilteredBlock
    , PrefilteredTx

    -- * Constructors
    , prefilterBlock
    , prefilterUtxo

    -- * Smart Getters @PrefilteredTx@
    , pftInputs
    , pftMeta
    , pftOutputs

    -- * Smart Getters @PrefilteredBlock@
    , pfbAddrs
    , pfbByAccounts
    , pfbInputs
    , pfbMeta
    , pfbOutputs
    ) where

import           Universum

import qualified Data.List as L
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map
import           Data.SafeCopy (base, deriveSafeCopy)
import qualified Data.Set as Set

import           Cardano.Wallet.Kernel.DB.BlockContext (BlockContext, bcSlotId)
import           Cardano.Wallet.Kernel.DB.BlockMeta (AddressMeta,
                     BlockMeta (..), LocalBlockMeta (..), addressMetaIsUsed)
import           Cardano.Wallet.Kernel.DB.HdWallet (HdAccountId (..),
                     HdAddress (..), HdAddressId (..), IsOurs (..),
                     hdAddressId, hdAddressIdParent)
import           Cardano.Wallet.Kernel.DB.InDb (InDb (..), fromDb)
import           Cardano.Wallet.Kernel.DB.Resolved (ResolvedBlock, rbTxs,
                     rtxInputs, rtxMeta, rtxOutputs)
import           Pos.Chain.Txp (TxId, TxIn (..), TxOut (..), TxOutAux (..),
                     Utxo)
import           Pos.Core (Address, Coin, SlotId, Timestamp)


{-------------------------------------------------------------------------------
    Types
-------------------------------------------------------------------------------}

data PrefilteredTx = PrefilteredTx
    { pftInputs -- | Relevant inputs
        :: !(Map TxIn (HdAddress, Coin))

    , pftOutputs -- | Relevant outputs
        :: !(Map TxIn (HdAddress, Coin))

    , pftMeta -- | Transaction Meta
        :: InDb (TxId, Timestamp)
    } deriving (Eq)
deriveSafeCopy 0 'base ''PrefilteredTx

-- A prefiltered block is a block that contains only inputs and outputs from
-- the block that are relevant to the wallet.
data PrefilteredBlock = PrefilteredBlock
    { pfbTxs -- | All Prefiltered Transactions
        :: ![PrefilteredTx]

    , pfbForeigns -- | Foreign Inputs
        :: !(Map HdAccountId (Set TxIn))
    } deriving (Eq)
deriveSafeCopy 0 'base ''PrefilteredBlock

instance Semigroup PrefilteredBlock where
    (PrefilteredBlock t1 f1) <> (PrefilteredBlock t2 f2) =
        PrefilteredBlock (t1 <> t2) (f1 <> f2)

instance Monoid PrefilteredBlock where
    mempty = PrefilteredBlock mempty mempty


{-------------------------------------------------------------------------------
    Constructors
-------------------------------------------------------------------------------}

-- | Prefilter a resolved block, grouping by account ids. This discards
-- outputs that and inputs that are irrelevant for the block.
--
-- NOTE
-- Resolved inputs are treated like outputs. In essence, inputs are actually the
-- outputs of previous transactions. So, filtering is (almost) symmetric here.
-- We do consider (and retain) foreign inputs however.
prefilterBlock
    :: IsOurs s
    => Map HdAccountId (Set TxIn)
    -> ResolvedBlock
    -> s
    -> (PrefilteredBlock, s)
prefilterBlock foreigns rb = runState $ do
    PrefilteredBlock <$> prefilteredTxs <*> pure foreigns
  where
    prefilteredTxs = forM (rb ^. rbTxs) $ \tx -> do
        inputs  <- state $ prefilterUtxo $ Map.fromList $ NE.toList $ tx ^. rtxInputs . fromDb
        outputs <- state $ prefilterUtxo $ tx ^. rtxOutputs . fromDb
        return $ PrefilteredTx inputs outputs (tx ^. rtxMeta)

prefilterUtxo
    :: IsOurs s
    => Utxo
    -> s
    -> (Map TxIn (HdAddress, Coin), s)
prefilterUtxo utxo = runState $
    flip Map.traverseMaybeWithKey utxo $ \_ (TxOutAux (TxOut addr coin)) ->
        fmap (,coin) <$> state (isOurs addr)


{-------------------------------------------------------------------------------
    Getters / Transformations @PrefilteredBlock@
-------------------------------------------------------------------------------}

pfbAddrs :: PrefilteredBlock -> [HdAddress]
pfbAddrs =
    L.nub . mconcat . fmap (fmap fst . Map.elems . pftOutputs) . pfbTxs

-- | Re-index a 'PrefilteredBlock' per account id, where each corresponding
-- block is tailored to the account it (i.e only contains data relevant to this
-- account).
pfbByAccounts :: PrefilteredBlock -> Map HdAccountId PrefilteredBlock
pfbByAccounts block =
    Map.unionsWith (<>) $ map
        (\tx -> foldl' (byAccount tx) mempty (pftOutputs tx <> pftInputs tx))
        (pfbTxs block)
  where
    getAccId =
        view (_1 . hdAddressId . hdAddressIdParent)
    byAccount tx m (HdAddress (HdAddressId accId _) _, _) =
        let
            outs' = Map.filter ((== accId) . getAccId) (pftOutputs tx)
            inps' = Map.filter ((== accId) . getAccId) (pftInputs tx)
            txs   = [ tx { pftOutputs = outs', pftInputs = inps' } ]
            frgns = Map.restrictKeys (pfbForeigns block) (Set.singleton accId)
        in
            Map.insert accId (PrefilteredBlock txs frgns) m

pfbInputs :: PrefilteredBlock -> Set TxIn
pfbInputs block =
    let
        foreigns = mconcat $ Map.elems $ pfbForeigns block
        locals   = mconcat $ fmap (Map.keysSet . pftInputs) $ pfbTxs block
    in
        foreigns <> locals

pfbMeta :: (BlockContext, PrefilteredBlock) -> LocalBlockMeta
pfbMeta (ctx, block) =
    LocalBlockMeta $ BlockMeta (InDb slotMeta) addressMeta
  where
    slotMeta :: Map TxId SlotId
    slotMeta = Map.fromList $ flip map (pfbTxs block) $ \tx ->
        ( pftMeta tx ^. fromDb . _1
        , ctx ^. bcSlotId . fromDb
        )
    -- An address "addr" is considered "used" if
    --   (a) ours addr
    --   (b) There is at least one tx for which: ∃ c.(addr, c) ∈  outputs
    --
    -- Both (a) & (b) are guaranteed as a result of prefiltering.
    addressMeta :: Map (InDb Address) AddressMeta
    addressMeta = Map.fromList $ flip map (pfbAddrs block) $ \(HdAddress _ addr) ->
        ( addr
        , mempty & addressMetaIsUsed .~ True
        )

pfbOutputs :: PrefilteredBlock -> Utxo
pfbOutputs =
    fmap toTxOutAux . mconcat . fmap pftOutputs . pfbTxs
  where
    toTxOutAux (HdAddress _ (InDb addr), coin) = TxOutAux $ TxOut addr coin
