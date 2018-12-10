{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RankNTypes            #-}

module Test.Integration.Framework.DSL
    (
    -- * Scenario
      scenario
    , xscenario
    , Scenarios
    , Context(..)

    -- * Steps
    , setup
    , request
    , verify

    -- * Requests (Only API types)
    , NewAddress(..)
    , NewWallet (..)
    , Payment (..)
    , Redemption (..)
    , Setup(..)
    , WalletUpdate(..)
    , ShieldedRedemptionCode (..)
    , InputSelectionPolicy(..)
    , FilterOperations(..)
    , SortOperations(..)
    , WalletOperation(..)
    , AssuranceLevel(..)
    , DestinationChoice(..)
    , defaultAccountId
    , defaultAssuranceLevel
    , defaultDistribution
    , defaultGroupingPolicy
    , defaultPage
    , defaultPerPage
    , defaultSetup
    , defaultSource
    , defaultSpendingPassword
    , defaultWalletName
    , noRedemptionMnemonic
    , noSpendingPassword

    -- * Expectations
    , WalletError(..)
    , ErrNotEnoughMoney(..)
    , TransactionStatus(..)
    , expectEqual
    , expectError
    , expectFieldEqual
    , expectSuccess
    , expectTxInHistoryOf
    , expectAddressInIndexOf
    , expectTxStatusEventually
    , expectTxStatusNever
    , expectWalletError
    , expectWalletUTxO

    -- * Helpers
    , ($-)
    , amount
    , assuranceLevel
    , backupPhrase
    , initialCoins
    , mnemonicWords
    , wallet
    , walletId
    , walletName
    ) where

import           Universum hiding (getArgs, second)

import           Control.Concurrent (threadDelay)
import           Control.Concurrent.Async (race)
import           Data.Generics.Internal.VL.Lens (lens)
import           Data.Generics.Product.Typed (HasType, typed)
import qualified Data.Map.Strict as Map
import           Test.Hspec.Core.Spec (SpecM, it, xit)
import           Test.Hspec.Expectations.Lifted
import           Test.QuickCheck (arbitrary, generate)

import           Cardano.Mnemonic (mkMnemonic, mnemonicToSeed)
import           Cardano.Wallet.API.Request.Filter (FilterOperations (..))
import           Cardano.Wallet.API.Request.Pagination (Page, PerPage)
import           Cardano.Wallet.API.Request.Sort (SortOperations (..))
import           Cardano.Wallet.API.V1.Types
import           Cardano.Wallet.Client.Http (ClientError (..), WalletClient)
import qualified Cardano.Wallet.Client.Http as Client
import           Pos.Chain.Txp (TxIn (..), TxOut (..), TxOutAux (..))
import           Pos.Client.Txp (InputSelectionPolicy (..))
import           Pos.Core (Coin, IsBootstrapEraAddr (..), deriveLvl2KeyPair,
                     mkCoin, unsafeGetCoin)
import           Pos.Core.NetworkMagic (NetworkMagic (..))
import           Pos.Crypto (ShouldCheckPassphrase (..),
                     safeDeterministicKeyGen)
import           Test.Integration.Framework.Request (HasHttpClient, request,
                     successfulRequest, ($-))
import           Test.Integration.Framework.Scenario (Scenario)

--
-- SCENARIO
--

-- Prior to starting integration tests, we setup a global context
-- and "prepare" a few faucet wallets which all contains some funds.
-- This helps speed up testing and isolate them.
data Context = Context
    { _faucets
        :: [FilePath]
        -- Already funded faucet wallets
    , _client
        :: WalletClient IO
        -- A handle to the underlying wallet backend server
    } deriving (Generic)


-- | Just a type-alias to 'SpecM', like 'scenario'. Ultimately, everything is
-- made in such way that we can use normal (albeit lifted) HSpec functions and
-- utilities if needed (and rely on its CLI as well when needed).
type Scenarios ctx = SpecM (MVar ctx) ()

-- | Just a slightly-specialized alias for 'it' to help lil'GHC. Also, makes
-- sure the wallet has been cleared out completely before running the scenario.
scenario
    :: String
    -> Scenario Context IO ()
    -> Scenarios Context
scenario title spec = it title (successfulRequest Client.resetWalletState >> spec)

xscenario
    :: String
    -> Scenario Context IO ()
    -> Scenarios Context
xscenario = xit

--
-- TYPES
--

data DestinationChoice
    = RandomDestination
    | LockedDestination
    deriving (Show, Generic)


--
-- STEPS
--

data Setup = Setup
    { _initialCoins
        :: [Coin]
    , _walletName
        :: Text
    , _assuranceLevel
        :: AssuranceLevel
    , _mnemonicWords
        :: Maybe [Text]
    } deriving (Show, Generic)

data Fixture = Fixture
    { _wallet
        :: Wallet
    , _destinations
        :: NonEmpty Address
    , _backupPhrase
        :: BackupPhrase
    } deriving (Show, Generic)

-- | Setup a wallet with the given parameters.
setup
    :: Setup
    -> Scenario Context IO Fixture
setup args = withNextFaucet $ \faucet -> do
    phrase <- maybe
        (liftIO $ generate arbitrary)
        mkBackupPhrase
        (args ^. mnemonicWords)
    wal <- setupWallet args phrase faucet
    addrs  <- forM (RandomDestination :| []) setupDestination
    return $ Fixture wal addrs phrase

-- | Apply 'a' to all actions in sequence
verify :: (Monad m) => a -> [a -> m ()] -> m ()
verify a = mapM_ (a &)


--
-- DEFAULT VALUES
--

defaultAccountId :: AccountIndex
defaultAccountId = minBound

defaultAssuranceLevel :: AssuranceLevel
defaultAssuranceLevel = NormalAssurance

defaultDistribution
    :: HasType (NonEmpty Address) s
    => Word64
    -> s
    -> NonEmpty PaymentDistribution
defaultDistribution c s = pure $
    PaymentDistribution (V1 $ head $ s ^. typed) (V1 $ mkCoin c)

defaultGroupingPolicy :: Maybe (V1 InputSelectionPolicy)
defaultGroupingPolicy = Nothing

defaultPage :: Maybe Page
defaultPage = Nothing

defaultPerPage :: Maybe PerPage
defaultPerPage = Nothing

defaultSetup :: Setup
defaultSetup = Setup
    { _initialCoins   = []
    , _walletName     = defaultWalletName
    , _assuranceLevel = defaultAssuranceLevel
    , _mnemonicWords  = Nothing
    }

defaultSource
    :: HasType Wallet s
    => s
    -> PaymentSource
defaultSource s =
    PaymentSource (s ^. wallet . walletId) defaultAccountId

defaultSpendingPassword :: SpendingPassword
defaultSpendingPassword = mempty

defaultWalletName :: Text
defaultWalletName = "Fixture Wallet"

noRedemptionMnemonic :: Maybe RedemptionMnemonic
noRedemptionMnemonic = Nothing

noSpendingPassword :: Maybe SpendingPassword
noSpendingPassword = Nothing


--
-- HELPERS
--

amount
    :: HasType (V1 Coin) s
    => Lens' s Word64
amount =
    lens _get _set
  where
    _get :: HasType (V1 Coin) s => s -> Word64
    _get = unsafeGetCoin . unV1 . view typed
    _set :: HasType (V1 Coin) s => (s, Word64) -> s
    _set (s, v) = set typed (V1 $ mkCoin v) s

assuranceLevel :: HasType AssuranceLevel s => Lens' s AssuranceLevel
assuranceLevel = typed

backupPhrase :: HasType BackupPhrase s => Lens' s BackupPhrase
backupPhrase = typed

faucets :: HasType [FilePath] s => Lens' s [FilePath]
faucets = typed

initialCoins
    :: HasType [Coin] s
    => Lens' s [Word64]
initialCoins =
    lens _get _set
  where
    _get :: HasType [Coin] s => s -> [Word64]
    _get = map unsafeGetCoin . view typed
    _set :: HasType [Coin] s => (s, [Word64]) -> s
    _set (s, v) = set typed (map mkCoin v) s

mnemonicWords :: HasType (Maybe [Text]) s => Lens' s (Maybe [Text])
mnemonicWords = typed

wallet :: HasType Wallet s => Lens' s Wallet
wallet = typed

walletId :: HasType WalletId s => Lens' s WalletId
walletId = typed

walletName :: HasType Text s => Lens' s Text
walletName = typed


--
-- EXPECTATIONS
--

-- | The type signature is more scary than it seems. This drills into a given
--   `a` type through the provided lens and sees whether field matches.
--
--   e.g.
--     do
--       expectFieldEqual #walAssuranceLevel AssuranceStrict response
--       expectFieldEqual #walName "My Wallet" response
expectFieldEqual
    :: (MonadIO m, MonadFail m, Show a, Eq a)
    => Lens' s a
    -> a
    -> Either ClientError s
    -> m ()
expectFieldEqual getter a = \case
    Left e  -> wantedSuccessButError e
    Right s -> view getter s `shouldBe` a


-- | Expects entire equality of two types
expectEqual
    :: (MonadIO m, MonadFail m, Show a, Eq a)
    => a
    -> Either ClientError a
    -> m ()
expectEqual =
    expectFieldEqual id


-- | Expect an errored response, without any further assumptions
expectError
    :: (MonadIO m, MonadFail m, Show a)
    => Either ClientError a
    -> m ()
expectError = \case
    Left _  -> return ()
    Right a -> wantedErrorButSuccess a


-- | Expect a successful response, without any further assumptions
expectSuccess
    :: (MonadIO m, MonadFail m, Show a)
    => Either ClientError a
    -> m ()
expectSuccess = \case
    Left e  -> wantedSuccessButError e
    Right _ -> return ()


-- | Expect a transaction to be part of a wallet history.
expectTxInHistoryOf
    :: (MonadIO m, MonadFail m, MonadReader ctx m, HasHttpClient ctx)
    => Wallet
    -> Either ClientError Transaction
    -> m ()
expectTxInHistoryOf w = \case
    Left e    -> wantedSuccessButError e
    Right txn -> tryNextPage (on (==) txId txn) 1
  where
    tryNextPage predicate i = do
        txns <- successfulRequest $ Client.getTransactionIndexFilterSorts
            $- Just (walId w)
            $- Nothing
            $- Nothing
            $- Just (fromInteger i)
            $- Just 50
            $- NoFilters
            $- NoSorts
        when (null txns) $
            fail "expectTxInHistoryOf: couldn't find transaction in history"
        case find predicate txns of
            Nothing -> tryNextPage predicate (i + 1)
            Just _  -> return ()


-- | Expect an address to be part of the global index
expectAddressInIndexOf
    :: (MonadIO m, MonadFail m, MonadReader ctx m, HasHttpClient ctx)
    =>  Either ClientError WalletAddress
    -> m ()
expectAddressInIndexOf = \case
    Left e  -> wantedSuccessButError e
    Right addr -> tryNextPage ((==) addr) 1
  where
    tryNextPage predicate i = do
        addrs <- successfulRequest $ Client.getAddressIndexPaginated
            $- Just (fromInteger i)
            $- Just 50
        when (null addrs) $
            fail "expectAddressInIndexOf: couldn't find address in history"
        case find predicate addrs of
            Nothing -> tryNextPage predicate (i + 1)
            Just _  -> return ()


-- | Wait for a transaction to reach one of the given status. Fails after 60
-- seconds if not.
expectTxStatusEventually
    :: (MonadIO m, MonadFail m, MonadReader ctx m, HasHttpClient ctx)
    => Wallet
    -> [TransactionStatus]
    -> Either ClientError Transaction
    -> m ()
expectTxStatusEventually w statuses = \case
    Left e    -> wantedSuccessButError e
    Right txn -> do
        result <- ask >>= \ctx -> timeout (60 * second) (waitForTxStatus ctx w statuses txn)
        case result of
            Nothing -> fail "expectTxStatusEventually: waited too long for statuses."
            Just _  -> return ()


-- | Checks that a transacton "never" reaches one of the given status. Never
-- really means 60 seconds, you know...
expectTxStatusNever
    :: (MonadIO m, MonadFail m, MonadReader ctx m, HasHttpClient ctx)
    => Wallet
    -> [TransactionStatus]
    -> Either ClientError Transaction
    -> m ()
expectTxStatusNever w statuses = \case
    Left e    -> wantedSuccessButError e
    Right txn -> do
        result <- ask >>= \ctx -> timeout (60 * second) (waitForTxStatus ctx w statuses txn)
        case result of
            Nothing -> return ()
            Just _  -> fail "expectTxStatusNever: reached one of the provided statuses."


expectWalletError
    :: (MonadIO m, MonadFail m, Show a)
    => WalletError
    -> Either ClientError a
    -> m ()
expectWalletError e' = \case
    Right a -> wantedErrorButSuccess a
    Left e  -> (ClientWalletError e') `shouldBe` e


expectWalletUTxO
    :: (MonadIO m, MonadFail m)
    => [Word64]
    -> Either ClientError UtxoStatistics
    -> m ()
expectWalletUTxO coins = \case
    Left e  -> wantedSuccessButError e
    Right stats -> do
        addr <- liftIO $ generate arbitrary
        let constructUtxoEntry input coin =
                ( TxInUnknown input "arbitrary input"
                , TxOutAux (TxOut addr (mkCoin coin))
                )
        let utxo = Map.fromList $ zipWith constructUtxoEntry [0..] coins

        computeUtxoStatistics log10 [utxo] `shouldBe` stats

--
-- INTERNALS
--

wantedSuccessButError
    :: (MonadFail m, Show e)
    => e
    -> m void
wantedSuccessButError =
    fail . ("expected a successful response but got an error: " <>) . show

wantedErrorButSuccess
    :: (MonadFail m, Show a)
    => a
    -> m void
wantedErrorButSuccess =
    fail . ("expected an error but got a successful response: " <>) . show

timeout :: (MonadIO m) => Int -> IO a -> m (Maybe a)
timeout maxWaitingTime action = liftIO $ do
    race (threadDelay maxWaitingTime) action >>= \case
        Left _  -> return Nothing
        Right a -> return (Just a)

second :: Int
second = 1000000

-- | Wait until the given transaction reaches the given status. Potentially
-- loop ad infinitum; Caller is expected to cancel the thread at some point.
waitForTxStatus
    :: HasHttpClient ctx
    => ctx
    -> Wallet
    -> [TransactionStatus]
    -> Transaction
    -> IO ()
waitForTxStatus ctx w statuses txn = do
    -- NOTE
    -- A bit tricky here, we can't just fire async operation on anything else
    -- but plain `IO`. Hence the explicit context passing here.
    txns <- flip runReaderT ctx $ successfulRequest $ Client.getTransactionIndex
        $- Just (walId w)
        $- Nothing
        $- Nothing

    let tx = find (on (==) txId txn) txns
    if ((fmap txStatus tx) `elem` (fmap Just statuses)) then
        return ()
    else
        threadDelay (5 * second) >> waitForTxStatus ctx w statuses txn

-- | Make a backup phrase from a raw list of words.
mkBackupPhrase
    :: (MonadIO m, MonadFail m)
    => [Text]
    -> m BackupPhrase
mkBackupPhrase ws = either onError onSuccess (mkMnemonic ws)
  where
    onError err =
        fail $ "Invalid BackupPhrase provided: " <> show ws <> ". We expect 12\
            \ valid english mnemonic words but the following error has occured:"
            <> show err
    onSuccess =
        return . BackupPhrase

-- | Execute the given setup action with using the next faucet wallet. It fails
-- hard if there's no more faucet wallet available.
withNextFaucet
    :: (Wallet -> Scenario Context IO c)
    -> Scenario Context IO c
withNextFaucet actionWithFaucet = do
    ctx <- get

    when (null $ ctx ^. faucets) $ fail $
        "\nFailed to setup new scenario: there's no more available faucet wallets!\
        \\nWe import a faucet wallet for each scenario but only have a limited\
        \ number of them. This can be modified directly in the configuration file,\
        \ by default in: \n\n\ttest/integration/configuration.yaml\
        \\n\ntry increasing the number of available 'poors' wallets\
        \\n\nspec: &default_core_genesis_spec\
        \\n\tinitializer:\
        \\n\t\ttestBalance:\
        \\n\t\t\tpoors: ???\n"

    let acquireFaucet = do
            let (key:rest) = ctx ^. faucets
            put (ctx & faucets .~ rest)
            successfulRequest $ Client.importWallet $- WalletImport Nothing key

    let releaseFaucet faucet = do
            successfulRequest (Client.deleteWallet $- walId faucet)

    bracket acquireFaucet releaseFaucet actionWithFaucet


-- | Setup a given wallet and pre-fill it with given coins.
setupWallet
    :: Setup
    -> BackupPhrase
    -> Wallet
    -> Scenario Context IO Wallet
setupWallet args phrase faucet = do
    wal <- successfulRequest $ Client.postWallet $- NewWallet
        phrase
        Nothing
        (args ^. assuranceLevel)
        (args ^. walletName)
        CreateWallet

    let paymentSource = PaymentSource (walId faucet) minBound
    let paymentDist (addr, coin) = pure $ PaymentDistribution (addrId addr) (V1 coin)

    forM_ (args ^. initialCoins) $ \coin -> do
        -- NOTE
        -- Making payments to a different address each time to cope with
        -- grouping policy. That's actually a behavior we might want to
        -- test in the future. So, we'll need to do something smarter here.
        addr <- successfulRequest $ Client.postAddress $- NewAddress
            Nothing
            minBound
            (walId wal)

        txn <- request $ Client.postTransaction $- Payment
            paymentSource
            (paymentDist (addr, mkCoin coin))
            Nothing
            Nothing

        expectTxStatusEventually wal [InNewestBlocks, Persisted] txn
    return wal


-- | Generate some destinations for payments.
--
--   - RandomDestination generates fake addresses going nowhere (hopefully :) ...)
--   - LockedDestination generates addresses that points to an asset-locked wallet
setupDestination
    :: DestinationChoice
    -> Scenario Context IO Address
setupDestination = \case
    RandomDestination -> do
        (BackupPhrase mnemonic) <- liftIO (generate arbitrary)

        let (_, esk) = safeDeterministicKeyGen
                (mnemonicToSeed mnemonic)
                mempty

        let maddr = fst <$> deriveLvl2KeyPair
                NetworkMainOrStage
                (IsBootstrapEraAddr True)
                (ShouldCheckPassphrase False)
                mempty
                esk
                (getAccIndex minBound)
                1

        case maddr of
            Nothing ->
                fail "The impossible happened: failed to generate a \
                    \ random address. This is really unexpected since we\
                    \ aren't doing anything fancy here ... ?"

            Just addr ->
                return addr

    LockedDestination ->
        fail "Asset-locked destination aren't yet implemented. This\
            \ requires slightly more work than it seems and will be\
            \ implemented later."
