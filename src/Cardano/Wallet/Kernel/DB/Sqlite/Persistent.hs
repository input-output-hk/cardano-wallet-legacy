{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE NamedFieldPuns     #-}
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

import           Control.Exception (throwIO, toException)
import           Control.Monad (void)
import           Control.Monad.Logger (runNoLoggingT)
import           Control.Monad.Trans.Resource (runResourceT)
import qualified Data.Foldable as Foldable
import           Data.List.NonEmpty (NonEmpty (..), nonEmpty)
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map as M
import qualified Data.Text as Text
import           Data.Traversable (for)
import qualified Database.Esqueleto as E
import           Database.Persist.Sqlite as Persist
import           Database.Persist.TH
import qualified Database.Sqlite as Sqlite
import           Database.SQLite.SimpleErrors.Parser (receiveSQLError)
import qualified Database.SQLite.SimpleErrors.Types as Sqlite
import qualified Database.SQLite3 as SqliteDirect
import           GHC.Generics (Generic)

import           Cardano.Wallet.Kernel.DB.Sqlite.Persistent.Orphans ()
import           Cardano.Wallet.Kernel.DB.TxMeta.Types (AccountFops (..),
                     FilterOperation (..), Limit (..), Offset (..),
                     SortCriteria (..), SortDirection (..), Sorting (..))
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
    :: SqlBackend
    -> SqlPersistM a
    -> IO (Either Sqlite.SQLiteResponse a)
runPersistConn c a =
    bimap convertError id <$> try (unsafeRunPersistConn c a)

convertError :: Sqlite.SqliteException -> Sqlite.SQLiteResponse
convertError (Sqlite.SqliteException { seError, seFunctionName, seDetails }) =
    receiveSQLError (SqliteDirect.SQLError seError' seDetails seFunctionName)
  where
    seError' = case seError of
        Sqlite.ErrorOK                 -> SqliteDirect.ErrorOK
        Sqlite.ErrorError              -> SqliteDirect.ErrorError
        Sqlite.ErrorInternal           -> SqliteDirect.ErrorInternal
        Sqlite.ErrorPermission         -> SqliteDirect.ErrorPermission
        Sqlite.ErrorAbort              -> SqliteDirect.ErrorAbort
        Sqlite.ErrorBusy               -> SqliteDirect.ErrorBusy
        Sqlite.ErrorLocked             -> SqliteDirect.ErrorLocked
        Sqlite.ErrorNoMemory           -> SqliteDirect.ErrorNoMemory
        Sqlite.ErrorReadOnly           -> SqliteDirect.ErrorReadOnly
        Sqlite.ErrorInterrupt          -> SqliteDirect.ErrorInterrupt
        Sqlite.ErrorIO                 -> SqliteDirect.ErrorIO
        Sqlite.ErrorNotFound           -> SqliteDirect.ErrorNotFound
        Sqlite.ErrorCorrupt            -> SqliteDirect.ErrorCorrupt
        Sqlite.ErrorFull               -> SqliteDirect.ErrorFull
        Sqlite.ErrorCan'tOpen          -> SqliteDirect.ErrorCan'tOpen
        Sqlite.ErrorProtocol           -> SqliteDirect.ErrorProtocol
        Sqlite.ErrorEmpty              -> SqliteDirect.ErrorEmpty
        Sqlite.ErrorSchema             -> SqliteDirect.ErrorSchema
        Sqlite.ErrorTooBig             -> SqliteDirect.ErrorTooBig
        Sqlite.ErrorConstraint         -> SqliteDirect.ErrorConstraint
        Sqlite.ErrorMismatch           -> SqliteDirect.ErrorMismatch
        Sqlite.ErrorMisuse             -> SqliteDirect.ErrorMisuse
        Sqlite.ErrorNoLargeFileSupport -> SqliteDirect.ErrorNoLargeFileSupport
        Sqlite.ErrorAuthorization      -> SqliteDirect.ErrorAuthorization
        Sqlite.ErrorFormat             -> SqliteDirect.ErrorFormat
        Sqlite.ErrorRange              -> SqliteDirect.ErrorRange
        Sqlite.ErrorNotAConnection     -> SqliteDirect.ErrorNotADatabase
        Sqlite.ErrorRow                -> SqliteDirect.ErrorRow
        Sqlite.ErrorDone               -> SqliteDirect.ErrorDone


unsafeRunPersistConn
    :: SqlBackend
    -> SqlPersistM a
    -> IO a
unsafeRunPersistConn conn query = do
    runResourceT $ runNoLoggingT $ runSqlConn query conn


-- | Migrates the 'MetaDB', failing with an IO exception in case this is not
-- possible.
unsafeMigrateMetaDB :: SqlBackend -> IO ()
unsafeMigrateMetaDB conn = do
    unsafeRunPersistConn conn (void (runMigrationSilent migrateAll))

-- | Simply a conveniency wrapper to avoid 'Kernel.TxMeta' to explicitly
-- import Sqlite modules.
newConnection :: FilePath -> IO SqlBackend
newConnection fp = open' (mkSqliteConnectionInfo (Text.pack fp)) (\_ _ _ _ -> pure ())

-- | Closes an open 'Connection' to the @Sqlite@ database stored in the
-- input 'MetaDBHandle'.
-- Even if open failed with error, this function should be called http://www.sqlite.org/c3ref/open.html
-- TODO: provide a bracket style interface to ensure this.
closeMetaDB :: SqlBackend -> IO ()
closeMetaDB = mempty -- Sqlite.close

-- | Delete everything out of the SQLite datbase.
clearMetaDB :: SqlBackend -> IO ()
clearMetaDB conn = void $ runPersistConn conn $ do
    deleteWhere @_ @_ @TxOutput []
    deleteWhere @_ @_ @TxInput []
    deleteWhere @_ @_ @TxMeta []

-- | Save the given 'Kernel.TxMeta' to the database.
putTxMeta :: SqlBackend -> Kernel.TxMeta -> IO ()
putTxMeta conn txMeta = void $ putTxMetaT conn txMeta

-- | Clear some metadata from the database
deleteTxMetas
    :: SqlBackend
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
putTxMetaT :: SqlBackend -> Kernel.TxMeta -> IO Kernel.PutReturn
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
toTxMeta TxMeta{..} inputs outputs = Kernel.TxMeta
    { _txMetaId = txMetaTableId
    , _txMetaAmount = txMetaTableAmount
    , _txMetaInputs = fromInputs inputs
    , _txMetaOutputs = fromOutputs outputs
    , _txMetaCreationAt = txMetaTableCreatedAt
    , _txMetaIsLocal = txMetaTableIsLocal
    , _txMetaIsOutgoing = txMetaTableIsOutgoing
    , _txMetaWalletId = txMetaTableWalletId
    , _txMetaAccountIx  = txMetaTableAccountIx
    }

-- | Fetches a 'Kernel.TxMeta' from the database, given its 'Txp.TxId'.
getTxMeta
    :: SqlBackend
    -> Txp.TxId
    -> Core.Address
    -> Word32
    -> IO (Maybe Kernel.TxMeta)
getTxMeta conn txid walletId accountIx = do
    res <- runPersistConn conn $ do
        mmeta <- Persist.get (TxMetaKey txid walletId accountIx)
        fmap join . for mmeta $ \txMeta -> do
            inputs  <- nonEmpty <$> selectList [InputTableTxId ==. txid] []
            outputs <- nonEmpty <$> selectList [OutputTableTxId ==. txid] []
            pure $ toTxMeta txMeta <$> deentity inputs <*> deentity outputs
    case res of
         Left e  -> throwIO $ Kernel.StorageFailure (toException e)
         Right r -> return r

getTxMetasById :: SqlBackend -> Txp.TxId -> IO (Maybe Kernel.TxMeta)
getTxMetasById conn txId = safeHead . fst <$> getTxMetas conn (Offset 0)
    (Limit 10) Everything Nothing (FilterByIndex txId) NoFilterOp Nothing

getAllTxMetas :: SqlBackend -> IO [Kernel.TxMeta]
getAllTxMetas conn = fst <$> getTxMetas conn (Offset 0)
    (Limit $ fromIntegral (maxBound :: Int)) Everything Nothing NoFilterOp NoFilterOp Nothing

getTxMetas
    :: SqlBackend
    -> Offset
    -> Limit
    -> AccountFops
    -> Maybe Core.Address
    -> FilterOperation Txp.TxId
    -> FilterOperation Core.Timestamp
    -> Maybe Sorting
    -> IO ([Kernel.TxMeta], Maybe Int)
getTxMetas conn (Offset offset) (Limit limit) accountFops mbAddress fopTxId fopTimestamp mbSorting = do
    res <- runPersistConn conn $ do

        -- The following 3 queries are disjointed and both fetches, respectively,
        -- @TxMeta@ @TxInput@ and @TxOutput@.
        -- The rationale behind doing three separate queries is that SQlite does
        -- not support array types, neither array aggregations
        -- (https://www.sqlite.org/lang_aggfunc.html).
        -- This in particular means the list of Inputs and Outputs for each TxId
        -- must be assembled in memory. One workarroun is to use group_concat
        -- (see link above), but this would return the array in a string format,
        -- separated by commas.

        -- The length of meta list is bounded by 50 in each realistic senario.
        -- So the marshalling between Haskell types - UTF8 - binary SQlite types
        -- shouldn`t be costly.

        meta <- fmap entityVal <$> case mbAddress of
           Nothing   -> metaQuery
           Just addr -> metaQueryWithAddr addr

        let txids = E.valList (map txMetaTableId meta)

        input <-
            E.select $
            E.from $ \inp -> do
            E.where_ (inp E.^. InputTableTxId `E.in_` txids)
            pure inp

        output <-
            E.select $
            E.from $ \out -> do
            E.where_ (out E.^. OutputTableTxId `E.in_` txids)
            pure out

        return $ do
            mt  <- nonEmpty meta
            inp <- nonEmpty (entityVal <$> input)
            out <- nonEmpty (entityVal <$> output)
            return (mt, inp, out)

    case res of
        Left e ->
            throwIO $ Kernel.StorageFailure (toException e)
        Right Nothing ->
            return ([], Just 0)
        Right (Just (meta, inputs, outputs)) -> do
            eiCount <- fmap (fmap listToMaybe) . runPersistConn conn $ do
                case mbAddress of
                    Nothing   -> metaQueryC
                    Just addr -> metaQueryWithAddrC addr
            let mapWithInputs  = transform $ map (\inp -> (inputTableTxId inp, inp)) inputs
            let mapWithOutputs = transform $ map (\out -> (outputTableTxId out, out)) outputs
            let txMeta = toValidKernelTxMeta mapWithInputs mapWithOutputs $ NonEmpty.toList meta
            let count' = case eiCount of
                    Left _  -> Nothing
                    Right c -> E.unValue <$> c
            return (txMeta, count')
--
  where
    metaQuery =
        E.select $
        E.from $ \t -> do
        sorting t
        filters t
        E.limit (fromIntegral limit)
        E.offset (fromIntegral offset)
        return t

    metaQueryC =
        E.select $
        E.from $ \t -> do
        filters t
        return E.countRows

    filters t = do
        E.where_
            $ filterAccs t accountFops
            E.&&. applyFilter (t E.^. TxMetaTableId) fopTxId
            E.&&. applyFilter (t E.^. TxMetaTableCreatedAt) fopTimestamp

    filterAccs _ Everything =
        E.val True
    filterAccs meta (AccountFops rootAddr (mbAccountIx)) =
        (meta E.^. TxMetaTableWalletId E.==. E.val rootAddr) E.&&.
            case mbAccountIx of
                Nothing ->
                    E.val True
                Just accountIx ->
                    meta E.^. TxMetaTableAccountIx E.==. E.val accountIx

    applyFilter inputData fop =
        let byPredicate o i = case o of
                Kernel.Equal            -> inputData E.==. i
                Kernel.LesserThan       -> inputData E.<. i
                Kernel.GreaterThan      -> inputData E.>. i
                Kernel.LesserThanEqual  -> inputData E.<=. i
                Kernel.GreaterThanEqual -> inputData E.>=. i
        in case fop of
            NoFilterOp -> E.val True
            FilterByIndex a -> byPredicate Kernel.Equal (E.val a)
            FilterByPredicate ford a -> byPredicate ford (E.val a)
            FilterByRange from to ->
                inputData E.>=. (E.val from)
                E.&&. inputData E.<=. (E.val to)
            FilterIn ls -> E.in_ inputData (E.valList ls)

    metaQueryWithAddrC addr =
        E.select $
        E.from $ \(meta `E.InnerJoin` inp `E.InnerJoin` out) -> do
        E.on (meta E.^. TxMetaTableId E.==. inp E.^. InputTableTxId)
        E.on (inp E.^. InputTableAddress E.==. out E.^. OutputTableAddress)
        filters meta
        E.where_
            $ inp E.^. InputTableAddress E.==. E.val addr
            E.&&. out E.^. OutputTableAddress E.==. E.val addr
        pure E.countRows

    sorting meta =
        case mbSorting of
            Nothing ->
                E.orderBy [E.desc (meta E.^. TxMetaTableCreatedAt)]
            Just (Sorting SortByCreationAt dir) ->
                E.orderBy [toEsqSortDir dir (meta E.^. TxMetaTableCreatedAt)]
            Just (Sorting SortByAmount     dir) ->
                E.orderBy [toEsqSortDir dir (meta E.^. TxMetaTableAmount)]

    metaQueryWithAddr addr =
        E.select $
        E.distinct $
        E.from $ \(meta `E.InnerJoin` inp `E.InnerJoin` out) -> do
        E.on (meta E.^. TxMetaTableId E.==. inp E.^. InputTableTxId)
        E.on (inp E.^. InputTableAddress E.==. out E.^. OutputTableAddress)

        sorting meta

        filters meta

        E.where_
            $ inp E.^. InputTableAddress E.==. E.val addr
            E.&&. out E.^. OutputTableAddress E.==. E.val addr

        return meta

    transform :: NonEmpty (Txp.TxId, a) -> M.Map Txp.TxId (NonEmpty a)
    transform = Foldable.foldl' updateFn M.empty

    updateFn
        :: M.Map Txp.TxId (NonEmpty a)
        -> (Txp.TxId, a)
        -> M.Map Txp.TxId (NonEmpty a)
    updateFn acc (txid, new) =
        M.insertWith (<>) txid (new :| []) acc

    toValidKernelTxMeta
        :: M.Map Txp.TxId (NonEmpty TxInput)
        -> M.Map Txp.TxId (NonEmpty TxOutput)
        -> [TxMeta]
        -> [Kernel.TxMeta]
    toValidKernelTxMeta im om =
        mapMaybe $ \m -> do
            let txid = txMetaTableId m
            toTxMeta m <$> M.lookup txid im <*> M.lookup txid om

toEsqSortDir
    :: PersistField a
    => SortDirection
    -> E.SqlExpr (E.Value a)
    -> E.SqlExpr E.OrderBy
toEsqSortDir Ascending  = E.asc
toEsqSortDir Descending = E.desc
-- Lower level api intended for testing

getTxMetasTable :: SqlBackend -> IO [TxMeta]
getTxMetasTable conn = deentity . unsafeRunPersistConn conn $ selectList [] []

getInputsTable :: SqlBackend -> IO [TxInput]
getInputsTable conn = deentity . unsafeRunPersistConn conn $ selectList [] []

getOutputsTable :: SqlBackend -> IO [TxOutput]
getOutputsTable conn = deentity . unsafeRunPersistConn conn $ selectList [] []

deentity :: (Functor f, Functor g) => f (g (Entity rec)) -> f (g rec)
deentity = fmap (fmap entityVal)

-- vendored from persistent-sqlite
open' :: (IsSqlBackend backend) => SqliteConnectionInfo -> LogFunc -> IO backend
open' connInfo logFunc = do
    conn <- Sqlite.open $ view Persist.sqlConnectionStr connInfo
    wrapConnectionInfo connInfo conn logFunc `onException` Sqlite.close conn
