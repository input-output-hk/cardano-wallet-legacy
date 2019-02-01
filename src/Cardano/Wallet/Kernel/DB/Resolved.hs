{-# LANGUAGE LambdaCase #-}

-- | Resolved blocks and transactions
module Cardano.Wallet.Kernel.DB.Resolved
    ( -- * Resolved blocks and transactions
      ResolvedTx(..)
    , ResolvedBlock(..)

      -- * Lenses
    , rtxInputs
    , rtxOutputs
    , rtxMeta
    , rbTxs
    , rbContext
    , rbMeta

      -- * Metadata
    , resolvedToTxMetas
    ) where

import           Universum hiding (truncate)

import           Control.Lens.TH (makeLenses)
import qualified Data.List.NonEmpty as NE
import qualified Data.Map as Map
import           Formatting (bprint, (%))
import qualified Formatting.Buildable

import           Serokell.Util (listJson, mapJson, pairF)

import           Cardano.Wallet.Kernel.DB.BlockContext (BlockContext)
import           Cardano.Wallet.Kernel.DB.HdWallet (HdAccountId (..),
                     HdAccountIx (..), IsOurs (..), hdAccountIdIx,
                     hdAccountIdParent, hdAddressId, hdAddressIdParent)
import           Cardano.Wallet.Kernel.DB.InDb (InDb (..), fromDb)
import           Cardano.Wallet.Kernel.DB.TxMeta.Types (TxMeta (..))
import           Cardano.Wallet.Kernel.Util.Core (absCoin, sumCoinsUnsafe)
import           Pos.Chain.Txp (TxId, TxIn (..), TxOut (..), TxOutAux (..),
                     Utxo)
import           Pos.Core (Address, Coin, Timestamp)


{-------------------------------------------------------------------------------
  Resolved blocks and transactions
-------------------------------------------------------------------------------}

-- | (Unsigned) transaction with inputs resolved
--
-- NOTE: We cannot recover the original transaction from a 'ResolvedTx'.
-- Any information needed inside the wallet kernel must be explicitly
-- represented here.
data ResolvedTx = ResolvedTx {
      -- | Transaction inputs
      -- A transaction input @(h, i)@ points to the @i@th output of the transaction
      -- with hash @h@, which is not particularly informative. The corresponding
      -- 'ResolvedInput' is obtained by looking up what that output actually is.
      _rtxInputs  :: InDb (NonEmpty (TxIn, TxOutAux))

      -- | Transaction outputs
    , _rtxOutputs :: InDb Utxo

     -- | Transaction Meta
    , _rtxMeta    :: InDb (TxId, Timestamp)
    } deriving Show

-- | (Unsigned block) containing resolved transactions
--
-- NOTE: We cannot recover the original block from a 'ResolvedBlock'.
-- Any information needed inside the wallet kernel must be explicitly
-- represented here.
data ResolvedBlock = ResolvedBlock {
      -- | Transactions in the block
      _rbTxs     :: ![ResolvedTx]

      -- | Block context
    , _rbContext :: !BlockContext

      -- | Creation time of this block
    , _rbMeta    :: !Timestamp
    } deriving Show

makeLenses ''ResolvedTx
makeLenses ''ResolvedBlock


{-------------------------------------------------------------------------------
  Metadata
-------------------------------------------------------------------------------}

-- | Error returned whenever
data ErrMalformedResolvedBlock = ErrMalformedResolvedBlock Text
    deriving Show
instance Exception ErrMalformedResolvedBlock


resolvedToTxMetas
    :: (IsOurs s)
    => ResolvedBlock
    -> s
    -> (Either ErrMalformedResolvedBlock [TxMeta], s)
resolvedToTxMetas rb = runState $ runExceptT $ fmap mconcat $ do
    forM (rb ^. rbTxs) $ \tx -> do
        accounts <- rtxAccountInfo tx
        forM accounts $ \(accId, spent, gained, isLocal) -> ExceptT $ return $ do
            inps <- traverse mkTxMetaInps (tx ^. rtxInputs . fromDb)
            outs <- mkTxMetaOuts (tx ^. rtxOutputs . fromDb)
            return TxMeta
                { _txMetaId = tx ^. rtxMeta . fromDb . _1
                , _txMetaAmount = absCoin spent gained
                , _txMetaInputs = inps
                , _txMetaOutputs = outs
                , _txMetaCreationAt = tx ^. rtxMeta . fromDb . _2
                , _txMetaIsLocal = isLocal
                , _txMetaIsOutgoing = gained < spent
                , _txMetaWalletId = accId ^. hdAccountIdParent
                , _txMetaAccountIx = getHdAccountIx $ accId ^. hdAccountIdIx
                }
  where
      -- | Inspect a resolved transaction in a given context:
      --  - @accId@ for the underlying account id
      --  - @spent@ coins from input addrs; reduce the balance.
      --  - @gained@ coins from output addrs; increase the balance.
      --  - @isLocal@ true if all inputs and outputs addrs belong to the account.
      -- Note that those values aren't absolute for it depends on _who_ inspects
      -- the transaction.
      -- This is why we may obtain multiple meta from a single transaction but
      -- at most, one per account id we recognized as being ours.
    rtxAccountInfo
        :: (IsOurs s, MonadState s m)
        => ResolvedTx
        -> m [(HdAccountId, Coin, Coin, Bool)]
    rtxAccountInfo tx = do
        let inps = fmap snd $ NE.toList $ tx ^. rtxInputs . fromDb
        ourInps <- ours (\coins -> ([coins], [])) inps

        let outs = Map.elems $ tx ^. rtxOutputs . fromDb
        ourOuts <- ours (\coins -> ([], [coins])) outs

        return $ flip map (Map.toList (Map.unionWith (<>) ourInps ourOuts)) $ \(accId, (ourInps', ourOuts')) ->
            let isLocal =
                    (length inps + length outs)
                    ==
                    (length ourInps + length ourOuts)
            in (accId, sumCoinsUnsafe ourInps', sumCoinsUnsafe ourOuts', isLocal)

    ours
        :: (Traversable t, IsOurs s, MonadState s m, Semigroup b)
        => (Coin -> b)
        -> t TxOutAux
        -> m (Map HdAccountId b)
    ours fn = flip foldM mempty $ \m (TxOutAux (TxOut addr coin)) -> do
        state (isOurs addr) <&> \case
            Nothing -> m
            Just x  -> Map.unionWith (<>) m $ Map.singleton (x ^.  hdAddressId ^. hdAddressIdParent) (fn coin)

    mkTxMetaOuts
        :: Utxo
        -> Either ErrMalformedResolvedBlock (NonEmpty (Address, Coin))
    mkTxMetaOuts utxo = utxo
        & Map.elems
        & fmap (\(TxOutAux (TxOut addr coin)) -> (addr, coin))
        & NE.nonEmpty
        & maybe (Left $ ErrMalformedResolvedBlock $ "Empty UTxO : " <> show utxo) Right

    mkTxMetaInps
        :: (TxIn, TxOutAux)
        -> Either ErrMalformedResolvedBlock (TxId, Word32, Address, Coin)
    mkTxMetaInps (txin, TxOutAux (TxOut addr coin)) =
        uncurry (,,addr,coin) <$> case txin of
            TxInUtxo txId ix -> Right (txId, ix)
            TxInUnknown{} ->
                Left $ ErrMalformedResolvedBlock $ "Tx input is unknown: " <> show txin


{-------------------------------------------------------------------------------
  Pretty-printing
-------------------------------------------------------------------------------}

instance Buildable ResolvedTx where
  build ResolvedTx{..} = bprint
    ( "ResolvedTx "
    % "{ inputs:  " % mapJson
    % ", outputs: " % mapJson
    % ", meta:    " % pairF
    % "}"
    )
    (Map.fromList (toList (_rtxInputs  ^. fromDb)))
    (_rtxOutputs ^. fromDb)
    (_rtxMeta ^. fromDb)

instance Buildable ResolvedBlock where
  build ResolvedBlock{..} = bprint
    ( "ResolvedBlock "
    % "{ txs: " % listJson
    % "}"
    )
    _rbTxs
