{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE QuasiQuotes        #-}
{-# LANGUAGE RankNTypes         #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TupleSections      #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Sqlite database for the 'TxMeta' portion of the wallet kernel.
module Cardano.Wallet.Kernel.DB.Sqlite.Persistent (

    -- * Resource creation and acquisition
      newConnection
    , closeMetaDB

    -- * Clear all entries
    , clearMetaDB

    -- * Basic API
    , putTxMeta
    , getTxMeta
    , getTxMetas
    , deleteTxMetas

    -- * Unsafe functions
    , unsafeMigrateMetaDB

    -- * testing
    , mkInputs
    , fromInputs
    , mkOutputs
    , fromOutputs
    , putTxMetaT
    , getAllTxMetas
    , getTxMetasTable
    , getInputsTable
    , getOutputsTable
    ) where

import           Universum

import qualified Database.SQLite.Simple as Sqlite
import qualified Database.SQLite.SimpleErrors.Types as Sqlite

import           Cardano.Wallet.Kernel.DB.Sqlite.Persistent.Orphans ()
import           Database.Persist.Sqlite as Persist
import           Database.Persist.TH

import           Control.Exception (throwIO, toException)
import           Control.Monad (void)
import           Data.List.NonEmpty (NonEmpty (..), nonEmpty)
import qualified Data.List.NonEmpty as NonEmpty
import           GHC.Generics (Generic)

import           Cardano.Wallet.Kernel.DB.TxMeta.Types (AccountFops (..),
                     FilterOperation (..), Limit (..), Offset (..),
                    -- SortCriteria (..), SortDirection (..),
                     Sorting (..))
import qualified Cardano.Wallet.Kernel.DB.TxMeta.Types as Kernel

import qualified Pos.Chain.Txp as Txp
import qualified Pos.Core as Core

{-# ANN module ("HLint: ignore Reduce duplication" :: Text) #-}

share [ mkPersist sqlSettings { mpsPrefixFields = False } , mkMigrate "migrateAll" ] [persistLowerCase|

TxMeta
    txMetaTableId         Txp.TxId
    txMetaTableAmount     Core.Coin
    txMetaTableCreatedAt  Core.Timestamp
    txMetaTableIsLocal    Bool
    txMetaTableIsOutgoing Bool
    txMetaTableWalletId   Core.Address
    txMetaTableAccountIx  Word32

    Primary txMetaTableId txMetaTableWalletId txMetaTableAccountIx

    deriving Eq Show Generic

TxOutput
    outputTableTxId    Txp.TxId
    outputTableIndex   Word32
    outputTableAddress Core.Address
    outputTableCoin    Core.Coin

    Primary outputTableTxId outputTableIndex

    deriving Show Generic

TxInput
    inputTableTxId         Txp.TxId
    inputTableForeignTxid  Txp.TxId
    inputTableForeignIndex Word32
    inputTableAddress      Core.Address
    inputTableCoin         Core.Coin

    Primary inputTableTxId inputTableForeignTxid inputTableForeignIndex

    deriving Eq Show Generic
|]

{--

* Table 1: ~tx_metas~
** Primary key: ~(tx_meta_id, tx_meta_wallet, tx_meta_account)~

| tx_meta_id | tx_meta_amount | tx_meta_created_at | tx_meta_is_local | tx_meta_is_outgoing | tx_meta_account | tx_meta_wallet
|------------+----------------+--------------------+------------------+---------------------+-----------------+-----------------
| Txp.TxId   | Core.Coin      | Core.Timestamp     | Bool             | Bool                | Word32          | Core.Address

--}

-- | Creates a storage-specific 'TxMeta' out of a 'Kernel.TxMeta'.
mkTxMeta :: Kernel.TxMeta -> TxMeta
mkTxMeta txMeta = TxMeta
    { txMetaTableId         = txMeta ^. Kernel.txMetaId
    , txMetaTableAmount     = txMeta ^. Kernel.txMetaAmount
    , txMetaTableCreatedAt  = txMeta ^. Kernel.txMetaCreationAt
    , txMetaTableIsLocal    = txMeta ^. Kernel.txMetaIsLocal
    , txMetaTableIsOutgoing = txMeta ^. Kernel.txMetaIsOutgoing
    , txMetaTableWalletId   = txMeta ^. Kernel.txMetaWalletId
    , txMetaTableAccountIx  = txMeta ^. Kernel.txMetaAccountIx
    }

{--

* Table 2: ~tx_meta_inputs~
** Primary key: (~input_id~, ~input_foreign_txid~, ~input_foreign_index~)

input_id   | input_foreign_txid | input_foreign_index | input_address | input_coin
-----------+--------------------+---------------------+---------------+------------
Txp.TxId   | Txp.TxId           | Word32              | Core.Address  | Core.Coin


--}

-- | Convenient constructor of a list of 'TxInput' from a 'Kernel.TxMeta'.
mkInputs :: Kernel.TxMeta -> NonEmpty TxInput
mkInputs Kernel.TxMeta{..}  = toTxInput <$> _txMetaInputs
    where
        toTxInput :: (Txp.TxId, Word32, Core.Address, Core.Coin) -> TxInput
        toTxInput (fTxId, fIndex, addr, coin) =
            TxInput
            { inputTableTxId = _txMetaId
            , inputTableForeignTxid = fTxId
            , inputTableForeignIndex = fIndex
            , inputTableAddress = addr
            , inputTableCoin = coin
            }

fromInputs :: NonEmpty TxInput -> NonEmpty (Txp.TxId, Word32, Core.Address, Core.Coin)
fromInputs ls = go <$> ls
    where
        go TxInput{..} =
            ( inputTableForeignTxid
            , inputTableForeignIndex
            , inputTableAddress
            , inputTableCoin
            )

{--

** Table 3: ~tx_meta_outputs~
** Primary Key: ~output_id~, ~output_index~

output_id   | output_index | output_address | output_coin
------------|--------------+----------------+--------------
Txp.TxId    | Word32       | Core.Address   | Core.Coin

--}

-- | A workaround to define Ord for TxOutput.
forOrder :: TxOutput -> (Txp.TxId, Word32, Core.Address, Core.Coin)
forOrder TxOutput {..} =
    (outputTableTxId, outputTableIndex, outputTableAddress, outputTableCoin)

instance Eq TxOutput where
    a == b = (forOrder a) == (forOrder b)

instance Ord TxOutput where
    compare a b = compare (forOrder a) (forOrder b)

-- | Convenient constructor of a list of 'TxInput' from a 'Kernel.TxMeta'.
--   The list returned should ensure the uniqueness of Indexes.
--   The usage of unsafe constructor NonEmpty.fromList is justified
--   because [0..] is indeed Nonempty.
mkOutputs :: Kernel.TxMeta -> NonEmpty TxOutput
mkOutputs Kernel.TxMeta{..} = toTxOutput <$> NonEmpty.zip _txMetaOutputs (NonEmpty.fromList [0..])
    where
        toTxOutput :: ((Core.Address, Core.Coin), Word32) -> TxOutput
        toTxOutput ((addr, coin), index) =
            TxOutput
            { outputTableTxId = _txMetaId
            , outputTableIndex = index
            , outputTableAddress = addr
            , outputTableCoin = coin
            }

-- | The invariant here is that the list of TxOutput should have all the same
-- TxId and include all indexes starting from 0.
fromOutputs :: NonEmpty TxOutput -> NonEmpty (Core.Address, Core.Coin)
fromOutputs ls = go <$> NonEmpty.sort ls
  where
    go TxOutput {..} = (outputTableAddress, outputTableCoin)

-- Orphans & other boilerplate

runPersistConn
    :: Sqlite.Connection
    -> SqlPersistM a
    -> IO (Either Sqlite.SQLiteResponse a)
runPersistConn c a = try (unsafeRunPersistConn c a)

unsafeRunPersistConn
    :: Sqlite.Connection
    -> SqlPersistM a
    -> IO a
unsafeRunPersistConn  = error "TODO"


-- | Migrates the 'MetaDB', failing with an IO exception in case this is not
-- possible.
unsafeMigrateMetaDB :: Sqlite.Connection -> IO ()
unsafeMigrateMetaDB conn = do
    unsafeRunPersistConn conn (error "find runner" migrateAll)

-- | Simply a conveniency wrapper to avoid 'Kernel.TxMeta' to explicitly
-- import Sqlite modules.
newConnection :: FilePath -> IO Sqlite.Connection
newConnection = Sqlite.open

-- | Closes an open 'Connection' to the @Sqlite@ database stored in the
-- input 'MetaDBHandle'.
-- Even if open failed with error, this function should be called http://www.sqlite.org/c3ref/open.html
-- TODO: provide a bracket style interface to ensure this.
closeMetaDB :: Sqlite.Connection -> IO ()
closeMetaDB = Sqlite.close

-- | Delete everything out of the SQLite datbase.
clearMetaDB :: Sqlite.Connection -> IO ()
clearMetaDB _conn = do
    error "TODO"

-- | Save the given 'Kernel.TxMeta' to the database.
putTxMeta :: Sqlite.Connection -> Kernel.TxMeta -> IO ()
putTxMeta conn txMeta = void $ putTxMetaT conn txMeta

-- | Clear some metadata from the database
deleteTxMetas
    :: Sqlite.Connection
        -- ^ Database Handle
    -> Core.Address
        -- ^ Target wallet
    -> Maybe Word32
        -- ^  A target account index. If none, delete metas for all accounts
    -> IO ()
deleteTxMetas conn walletId mAccountIx = do
    unsafeRunPersistConn conn $ deleteWhere (conditionWalletId : conditionAccountIx)
  where
    conditionWalletId :: Filter TxMeta
    conditionWalletId =
        TxMetaTableWalletId ==. walletId

    conditionAccountIx :: [Filter TxMeta]
    conditionAccountIx = case mAccountIx of
        Nothing -> []
        Just ix -> [TxMetaTableAccountIx ==. ix]

-- | Inserts a new 'Kernel.TxMeta' in the database, given its opaque
-- 'MetaDBHandle'.
putTxMetaT :: Sqlite.Connection -> Kernel.TxMeta -> IO Kernel.PutReturn
putTxMetaT conn txMeta = do
    let tMeta   = mkTxMeta txMeta
        inputs  = mkInputs txMeta
        outputs = mkOutputs txMeta
        txId    = txMetaTableId tMeta
        accountIx = txMetaTableAccountIx tMeta
        walletId = txMetaTableWalletId tMeta
    res1 <- runPersistConn conn $ do
        -- The order here is important. If Outputs succeed everything else
        -- should also succeed.
        insertMany_ (NonEmpty.toList outputs)
        insertMany_ [tMeta]
        insertMany_ (NonEmpty.toList inputs)
    case res1 of
        -- All succeeded. We have a new Tx in db.
        Right _ -> return Kernel.Tx
        -- This is the only acceptable exception here. If anything else is
        -- thrown, that`s an error.
        Left (Sqlite.SQLConstraintError Sqlite.Unique "tx_metas_outputs.meta_id, tx_metas_outputs.output_index") -> do
            t <- getTxMetasById conn txId
            case (Kernel.txIdIsomorphic txMeta <$> t) of
                Just False ->
                    -- This violation means the Tx has same TxId but different
                    -- Inputs (as set) or Outputs (ordered).
                    throwIO
                        . Kernel.InvariantViolated
                        $ Kernel.TxIdInvariantViolated txId
                _  -> do
                    -- If there is not a  TxId violation, we can try to insert
                    -- TxMeta.  We handle Nothing and (Just True) the same here,
                    -- since it's possible that there is no Meta with this
                    -- Inputs/Outputs.  In the future we may consider doing
                    -- a better cleanup to avoid such cases.
                    res2 <- runPersistConn conn $ insert_ tMeta
                    case res2 of
                        -- all good. We managed to inset new TxMeta.
                        Right _ -> return Kernel.Meta
                        Left (Sqlite.SQLConstraintError Sqlite.Unique "tx_metas.meta_id, tx_metas.meta_wallet_id, tx_metas.meta_account_ix") -> do
                            res3 <- fmap (Kernel.txIdIsomorphic txMeta)
                                <$> getTxMeta conn txId walletId accountIx
                            case res3 of
                                -- We couldn`t insert, but there is nothing
                                -- here.  This should never happen.
                                Nothing ->
                                    throwIO $ Kernel.InvariantViolated (Kernel.UndisputableLookupFailed "TxMeta")
                                -- all good. TxMeta is also there. This is most
                                -- probably the reult of a rollback, where we
                                -- try to insert the same Tx two times.  Nothing
                                -- new is inserted n db.
                                Just True ->
                                    return Kernel.No
                                -- This violation means the Tx has same TxId but
                                -- different Inputs (as set) or Outputs
                                -- (ordered).
                                Just False ->
                                    throwIO
                                        . Kernel.InvariantViolated
                                        $ Kernel.TxIdInvariantViolated txId
                        Left e ->
                            throwIO $ Kernel.StorageFailure (toException e)
        Left e -> throwIO $ Kernel.StorageFailure (toException e)

-- | Converts a database-fetched 'TxMeta' into a domain-specific 'Kernel.TxMeta'.
toTxMeta :: TxMeta -> NonEmpty TxInput -> NonEmpty TxOutput -> Kernel.TxMeta
toTxMeta TxMeta{..} inputs outputs = Kernel.TxMeta {
      _txMetaId         = txMetaTableId
    , _txMetaAmount     = txMetaTableAmount
    , _txMetaInputs     = fromInputs inputs
    , _txMetaOutputs    = fromOutputs outputs
    , _txMetaCreationAt = txMetaTableCreatedAt
    , _txMetaIsLocal    = txMetaTableIsLocal
    , _txMetaIsOutgoing = txMetaTableIsOutgoing
    , _txMetaWalletId   = txMetaTableWalletId
    , _txMetaAccountIx  = txMetaTableAccountIx
    }

-- | Fetches a 'Kernel.TxMeta' from the database, given its 'Txp.TxId'.
getTxMeta
    :: Sqlite.Connection
    -> Txp.TxId
    -> Core.Address
    -> Word32
    -> IO (Maybe Kernel.TxMeta)
getTxMeta conn txid walletId accountIx = do
    res <- runPersistConn conn $ do
        mmeta <- Persist.get (TxMetaKey txid walletId accountIx)
        case mmeta of
            Just txMeta -> do
                inputs  <- nonEmpty <$> selectList [InputTableTxId ==. txid] []
                outputs <- nonEmpty <$> selectList [OutputTableTxId ==. txid] []
                pure $ toTxMeta txMeta <$> deentity inputs <*> deentity outputs
            _        -> pure Nothing
    case res of
         Left e  -> throwIO $ Kernel.StorageFailure (toException e)
         Right r -> return r

getTxMetasById :: Sqlite.Connection -> Txp.TxId -> IO (Maybe Kernel.TxMeta)
getTxMetasById conn txId = safeHead . fst <$> getTxMetas conn (Offset 0)
    (Limit 10) Everything Nothing (FilterByIndex txId) NoFilterOp Nothing

getAllTxMetas :: Sqlite.Connection -> IO [Kernel.TxMeta]
getAllTxMetas conn =  fst <$> getTxMetas conn (Offset 0)
    (Limit $ fromIntegral (maxBound :: Int)) Everything Nothing NoFilterOp NoFilterOp Nothing

getTxMetas
    :: Sqlite.Connection
    -> Offset
    -> Limit
    -> AccountFops
    -> Maybe Core.Address
    -> FilterOperation Txp.TxId
    -> FilterOperation Core.Timestamp
    -> Maybe Sorting
    -> IO ([Kernel.TxMeta], Maybe Int)
getTxMetas _conn (Offset _offset) (Limit _limit) _accountFops _mbAddress _fopTxId _fopTimestamp _mbSorting = do
    error "TODO"
--     res <- Sqlite.runDBAction $ runBeamSqlite conn $ do
--
--        -- The following 3 queries are disjointed and both fetches, respectively,
--        -- @TxMeta@ @TxInput@ and @TxOutput@.
--        -- The rationale behind doing three separate queries is that SQlite does
--        -- not support array types, neither array aggregations
--        -- (https://www.sqlite.org/lang_aggfunc.html).
--        -- This in particular means the list of Inputs and Outputs for each TxId
--        -- must be assembled in memory. One workarroun is to use group_concat
--        -- (see link above), but this would return the array in a string format,
--        -- separated by commas.
--
--        -- The length of meta list is bounded by 50 in each realistic senario.
--        -- So the marshalling between Haskell types - UTF8 - binary SQlite types
--        -- shouldn`t be costly.
--        meta <- SQL.runSelectReturningList $ SQL.select $ do
--            case mbAddress of
--                Nothing   -> SQL.limit_ limit $ SQL.offset_ offset $ metaQuery
--                Just addr -> SQL.limit_ limit $ SQL.offset_ offset $ metaQueryWithAddr addr
--        let txids = map (SQL.val_ . _txMetaTableId) meta
--        input <- SQL.runSelectReturningList $ SQL.select $ do
--                input <- SQL.all_ $ _mDbInputs metaDB
--                let txid = _inputTableTxId input
--                SQL.guard_ $ in_ txid txids
--                pure input
--        output <- SQL.runSelectReturningList $ SQL.select $ do
--                output <- SQL.all_ $ _mDbOutputs metaDB
--                let txid = _outputTableTxId output
--                SQL.guard_ $ in_ txid txids
--                pure output
--        return $ do
--             mt  <- nonEmpty meta
--             inp <- nonEmpty input
--             out <- nonEmpty output
--             return (mt, inp, out)
--
--    case res of
--        Left e -> throwIO $ Kernel.StorageFailure (toException e)
--        Right Nothing -> return ([], Just 0)
--        Right (Just (meta, inputs, outputs)) ->  do
--            eiCount <- Sqlite.runDBAction $ runBeamSqlite conn $
--                case mbAddress of
--                    Nothing   -> SQL.runSelectReturningOne $ SQL.select metaQueryC
--                    Just addr -> SQL.runSelectReturningOne $ SQL.select $ metaQueryWithAddrC addr
--            let mapWithInputs  = transform $ map (\inp -> (_inputTableTxId inp, inp)) inputs
--            let mapWithOutputs = transform $ map (\out -> (_outputTableTxId out, out)) outputs
--            let txMeta = toValidKernelTxMeta mapWithInputs mapWithOutputs $ NonEmpty.toList meta
--                count = case eiCount of
--                    Left _  -> Nothing
--                    Right c -> c
--            return (txMeta, count)
--
--    where
--        filters meta = do
--            SQL.guard_ $ filterAccs meta accountFops
--            SQL.guard_ $ applyFilter (_txMetaTableId meta) fopTxId
--            SQL.guard_ $ applyFilter (_txMetaTableCreatedAt meta) fopTimestamp
--            pure ()
--
--        filtersC meta = do
--            SQL.guard_ $ filterAccs meta accountFops
--            SQL.guard_ $ applyFilter (_txMetaTableId meta) fopTxId
--            SQL.guard_ $ applyFilter (_txMetaTableCreatedAt meta) fopTimestamp
--            pure ()
--
--        metaQuery = do
--            let query = SQL.all_ $ _mDbMeta metaDB
--            meta <- case mbSorting of
--                    Nothing ->
--                        SQL.orderBy_ (SQL.desc_ . _txMetaTableCreatedAt) query
--                    Just (Sorting SortByCreationAt dir) ->
--                        SQL.orderBy_ (toBeamSortDirection dir . _txMetaTableCreatedAt) query
--                    Just (Sorting SortByAmount     dir) ->
--                        SQL.orderBy_ (toBeamSortDirection dir . _txMetaTableAmount) query
--            filters meta
--            return meta
--
--        metaQueryC = SQL.aggregate_ (\_ -> SQL.as_ SQL.countAll_) $ do
--            meta <-  SQL.all_ $ _mDbMeta metaDB
--            filtersC meta
--            return meta
--
--        findAndUnion addr = do
--                let input = do
--                        inp <- SQL.all_ $ _mDbInputs metaDB
--                        SQL.guard_ $ ((_inputTableAddress inp) ==. (SQL.val_ addr))
--                        pure $ _inputTableTxId inp
--                let output = do
--                        out <- SQL.all_ $ _mDbOutputs metaDB
--                        SQL.guard_ $ ((_outputTableAddress out) ==. (SQL.val_ addr))
--                        pure $ _outputTableTxId out
--                -- union removes txId duplicates.
--                txid <- SQL.union_ input output
--                SQL.join_ (_mDbMeta metaDB) (\ mt -> ((_txMetaTableId mt) ==. txid))
--
--        metaQueryWithAddr addr = do
--            meta <- case mbSorting of
--                Nothing ->
--                    SQL.orderBy_ (SQL.desc_ . _txMetaTableCreatedAt) (findAndUnion addr)
--                Just (Sorting SortByCreationAt dir) ->
--                    SQL.orderBy_ (toBeamSortDirection dir . _txMetaTableCreatedAt) (findAndUnion addr)
--                Just (Sorting SortByAmount     dir) ->
--                    SQL.orderBy_ (toBeamSortDirection dir . _txMetaTableAmount) (findAndUnion addr)
--            filters meta
--            return meta
--
--        metaQueryWithAddrC addr = SQL.aggregate_ (\_ -> SQL.as_ SQL.countAll_) $ do
--            meta <- findAndUnion addr
--            filtersC meta
--            pure meta
--
--        filterAccs _ Everything = SQL.QExpr (pure (valueE (sqlValueSyntax True)))
--        filterAccs meta (AccountFops rootAddr (mbAccountIx)) =
--            (_txMetaTableWalletId meta ==. SQL.val_ rootAddr) &&.
--                case mbAccountIx of
--                    Nothing -> SQL.QExpr (pure (valueE (sqlValueSyntax True)))
--                    Just accountIx -> _txMetaTableAccountIx meta ==. SQL.val_ accountIx
--
--        applyFilter inputData fop =
--            let byPredicate o i = case o of
--                    Kernel.Equal            -> inputData ==. i
--                    Kernel.LesserThan       -> inputData <. i
--                    Kernel.GreaterThan      -> inputData >. i
--                    Kernel.LesserThanEqual  -> inputData <=. i
--                    Kernel.GreaterThanEqual -> inputData >=. i
--            in case fop of
--                NoFilterOp -> SQL.val_ True
--                FilterByIndex a -> byPredicate Kernel.Equal (SQL.val_ a)
--                FilterByPredicate ford a -> byPredicate ford (SQL.val_ a)
--                FilterByRange from to -> between_ inputData (SQL.val_ from) (SQL.val_ to)
--                FilterIn ls -> in_ inputData (map SQL.val_ ls)
--
--        transform :: NonEmpty (Txp.TxId, a) -> M.Map Txp.TxId (NonEmpty a)
--        transform = Foldable.foldl' updateFn M.empty
--
--        updateFn
--            :: M.Map Txp.TxId (NonEmpty a)
--            -> (Txp.TxId, a)
--            -> M.Map Txp.TxId (NonEmpty a)
--        updateFn acc (txid, new) =
--            M.insertWith (<>) txid (new :| []) acc
--
--        toValidKernelTxMeta
--            :: M.Map Txp.TxId (NonEmpty TxInput)
--            -> M.Map Txp.TxId (NonEmpty TxOutput)
--            -> [TxMeta]
--            -> [Kernel.TxMeta]
--        toValidKernelTexMeta im om =
--            mapMaybe $ \m -> do
--                let txid = _txMetaTableId m
--                toTxMeta m <$> M.lookup txid im <*> M.lookup txid om

-- Lower level api intended for testing

getTxMetasTable :: Sqlite.Connection -> IO [TxMeta]
getTxMetasTable conn = deentity . unsafeRunPersistConn conn $ selectList [] []

getInputsTable :: Sqlite.Connection -> IO [TxInput]
getInputsTable conn = deentity . unsafeRunPersistConn conn $ selectList [] []

getOutputsTable :: Sqlite.Connection -> IO [TxOutput]
getOutputsTable conn = deentity . unsafeRunPersistConn conn $ selectList [] []

deentity :: (Functor f, Functor g) => f (g (Entity rec)) -> f (g rec)
deentity = fmap (fmap entityVal)
