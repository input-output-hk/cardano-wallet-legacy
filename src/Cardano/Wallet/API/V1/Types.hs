{-# LANGUAGE CPP                        #-}
{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE DuplicateRecordFields      #-}
{-# LANGUAGE ExplicitNamespaces         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE PolyKinds                  #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE StrictData                 #-}
{-# LANGUAGE TemplateHaskell            #-}
-- The hlint parser fails on the `pattern` function, so we disable the
-- language extension here.
{-# LANGUAGE NoPatternSynonyms          #-}

-- Needed for the `Buildable`, `SubscriptionStatus` and `NodeId` orphans.
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Cardano.Wallet.API.V1.Types (
  -- * Swagger & REST-related types
    PasswordUpdate (..)
  , AccountUpdate (..)
  , NewAccount (..)
  , Update
  , New
  , ForceNtpCheck (..)
  -- * Domain-specific types
  -- * Wallets
  , Wallet (..)
  , AssuranceLevel (..)
  , NewWallet (..)
  , WalletUpdate (..)
  , WalletId (..)
  , exampleWalletId
  , WalletOperation (..)
  , SpendingPassword
  , mkSpendingPassword
  , WalletPassPhrase (..)
  , WalletTimestamp (..)
  , WalletInputSelectionPolicy (..)
  , WalletSoftwareVersion (..)
  , WalletTxId (..)
  , WalAddress (..)
  , WalletCoin (..)
  , EosWallet (..)
  , NewEosWallet (..)
  , UpdateEosWallet (..)
  -- * Addresses
  , AddressOwnership (..)
  , AddressIndex
  , AddressValidity (..)
  -- * Accounts
  , Account (..)
  , accountsHaveSameId
  , AccountIndex
  , AccountAddresses (..)
  , AccountBalance (..)
  , AccountPublicKeyWithIx (..)
  , getAccIndex
  , mkAccountIndex
  , mkAccountIndexM
  , unsafeMkAccountIndex
  , AccountIndexError(..)
  -- * Addresses
  , WalletAddress (..)
  , NewAddress (..)
  , BatchImportResult(..)
  , AddressLevel
  , addressLevelToWord32
  , word32ToAddressLevel
  -- * Payments
  , Payment (..)
  , PaymentSource (..)
  , PaymentDistribution (..)
  , Transaction (..)
  , TransactionType (..)
  , TransactionDirection (..)
  , TransactionStatus(..)
  , TransactionAsBase16
  , mkTransactionAsBase16
  , rawTransactionAsBase16
  , TransactionSignatureAsBase16
  , mkTransactionSignatureAsBase16
  , rawTransactionSignatureAsBase16
  , EstimatedFees (..)
  , AddressAndPath (..)
  , UnsignedTransaction (..)
  , AddressWithProof (..)
  , SignedTransaction (..)
  -- * Updates
  , WalletSoftwareUpdate (..)
  -- * Importing a wallet from a backup
  , WalletImport (..)
  -- * Settings
  , NodeSettings (..)
  , SlotDuration
  , mkSlotDuration
  , BlockchainHeight
  , mkBlockchainHeight
  , LocalTimeDifference
  , mkLocalTimeDifference
  , EstimatedCompletionTime
  , mkEstimatedCompletionTime
  , SyncThroughput
  , mkSyncThroughput
  , SyncState (..)
  , SyncProgress (..)
  , SyncPercentage
  , mkSyncPercentage
  , NodeInfo (..)
  , TimeInfo(..)
  , SubscriptionStatus(..)
  , Redemption(..)
  , RedemptionMnemonic(..)
  , BackupPhrase(..)
  , ShieldedRedemptionCode(..)
  , WAddressMeta (..)
  -- * Some types for the API
  , CaptureWalletId
  , CaptureAccountId
  -- * Core re-exports
  , Core.Address
  , Core.PublicKey
  , Core.PassPhrase
  -- * Wallet Errors
  , WalletError(..)
  , ErrNotEnoughMoney(..)
  , ErrUtxoNotEnoughFragmented(..)
  , msgUtxoNotEnoughFragmented
  , ErrZeroAmountCoin(..)
  , msgZeroAmountCoin
  , toServantError
  , toHttpErrorStatus
  , module Cardano.Wallet.Types.UtxoStatistics
  ) where

import qualified Prelude
import           Universum

import qualified Cardano.Crypto.Wallet as CC
import           Control.Lens (at, to, (?~))
import           Data.Aeson
import qualified Data.Aeson.Options as Aeson
import           Data.Aeson.TH as A
import           Data.Aeson.Types (Parser, Value (..), typeMismatch)
import           Data.Bifunctor (first)
import qualified Data.ByteArray as ByteArray
import qualified Data.ByteString as BS
import qualified Data.Char as C
import           Data.Default (Default (def))
import qualified Data.List.NonEmpty as NE
import           Data.Semigroup (Semigroup)
import           Data.Swagger hiding (Example, example)
import           Data.Text (Text, dropEnd, toLower)
import           Formatting (bprint, build, int, sformat, (%))
import qualified Formatting.Buildable
import           Generics.SOP.TH (deriveGeneric)
import qualified Serokell.Util.Base16 as Base16
import           Servant
import           Test.QuickCheck
import           Test.QuickCheck.Gen (Gen (..))
import qualified Test.QuickCheck.Gen as Gen
import qualified Test.QuickCheck.Modifiers as Gen

import           Pos.Node.API

import           Cardano.Wallet.API.Response.JSend (HasDiagnostic (..),
                     noDiagnosticKey)
import           Cardano.Wallet.API.Types.UnitOfMeasure (MeasuredIn (..),
                     UnitOfMeasure (..))
import           Cardano.Wallet.API.V1.Errors (ToHttpErrorStatus (..),
                     ToServantError (..))
import           Cardano.Wallet.API.V1.Generic (jsendErrorGenericParseJSON,
                     jsendErrorGenericToJSON)
import           Cardano.Wallet.API.V1.Swagger.Example (Example, example)
import           Cardano.Wallet.Kernel.AddressPoolGap (AddressPoolGap)
import           Cardano.Wallet.Types.UtxoStatistics
import           Cardano.Wallet.Util (buildIndent, buildList, mkJsonKey,
                     showApiUtcTime)

import           Cardano.Mnemonic (Mnemonic)
import qualified Pos.Binary.Class as Bi
import qualified Pos.Chain.Txp as Txp
import           Pos.Chain.Update (ApplicationName (..), SoftwareVersion (..))
import qualified Pos.Client.Txp.Util as Core
import qualified Pos.Core as Core
import           Pos.Crypto (PublicKey (..), decodeHash, hashHexF)
import qualified Pos.Crypto.Signing as Core
import           Pos.Infra.Communication.Types.Protocol ()
import           Pos.Infra.Diffusion.Subscription.Status
                     (SubscriptionStatus (..))
import           Pos.Infra.Util.LogSafe (BuildableSafeGen (..),
                     deriveSafeBuildable)
import           Test.Pos.Core.Arbitrary ()

-- | Declare generic schema, while documenting properties
--   For instance:
--
--    data MyData = MyData
--      { myDataField1 :: String
--      , myDataField2 :: String
--      } deriving (Generic)
--
--   instance ToSchema MyData where
--     declareNamedSchema =
--       genericSchemaDroppingPrefix "myData" (\(--^) props -> props
--         & ("field1" --^ "Description 1")
--         & ("field2" --^ "Description 2")
--       )
--
--   -- or, if no descriptions are added to the underlying properties
--
--   instance ToSchema MyData where
--     declareNamedSchema =
--       genericSchemaDroppingPrefix "myData" (\_ -> id)
--

optsADTCamelCase :: A.Options
optsADTCamelCase = defaultOptions
    { A.constructorTagModifier = mkJsonKey
    , A.sumEncoding            = A.ObjectWithSingleField
    }


--
-- Versioning
--

newtype WalletPassPhrase = WalletPassPhrase Core.PassPhrase
    deriving (Show, Eq, Ord, Generic)

-- | A 'SpendingPassword' represent a secret piece of information which can be
-- optionally supplied by the user to encrypt the private keys. As private keys
-- are needed to spend funds and this password secures spending, here the name
-- 'SpendingPassword'.
-- Practically speaking, it's just a type synonym for a PassPhrase, which is a
-- base16-encoded string.
type SpendingPassword = WalletPassPhrase

mkSpendingPassword :: Text -> Either Text SpendingPassword
mkSpendingPassword text =
    case Base16.decode text of
        Left e -> Left e
        Right bs -> do
            let bl = BS.length bs
            -- Currently passphrase may be either 32-byte long or empty (for
            -- unencrypted keys).
            if bl == 0 || bl == Core.passphraseLength
                then Right $ WalletPassPhrase $ ByteArray.convert bs
                else Left $ sformat
                     ("Expected spending password to be of either length 0 or "%int%", not "%int)
                     Core.passphraseLength bl

instance ToJSON WalletPassPhrase where
    toJSON (WalletPassPhrase pp) = String . Base16.encode . ByteArray.convert $ pp

instance FromJSON WalletPassPhrase where
    parseJSON (String pp) = case mkSpendingPassword pp of
        Left e    -> fail (toString e)
        Right pp' -> pure pp'
    parseJSON x           = typeMismatch "parseJSON failed for PassPhrase" x

instance Arbitrary WalletPassPhrase where
    arbitrary = fmap WalletPassPhrase arbitrary

instance ToSchema WalletPassPhrase where
    declareNamedSchema _ =
        pure $ NamedSchema (Just "WalletPassPhrase") $ mempty
            & type_ .~ SwaggerString
            & format ?~ "hex|base16"

newtype WalletCoin = WalletCoin { unWalletCoin :: Core.Coin }
    deriving (Eq, Generic, Ord, Show)

deriveSafeBuildable ''WalletCoin
instance BuildableSafeGen WalletCoin where
    buildSafeGen _ (WalletCoin c) = bprint build c

instance ToJSON WalletCoin where
    toJSON (WalletCoin c) = toJSON . Core.unsafeGetCoin $ c

instance FromJSON WalletCoin where
    parseJSON v = do
        i <- Core.Coin <$> parseJSON v
        either (fail . toString) (const (pure (WalletCoin i)))
            $ Core.checkCoin i

instance Arbitrary WalletCoin where
    arbitrary = fmap WalletCoin arbitrary

instance ToSchema WalletCoin where
    declareNamedSchema _ =
        pure $ NamedSchema (Just "WalletCoin") $ mempty
            & type_ .~ SwaggerNumber
            & maximum_ .~ Just (fromIntegral Core.maxCoinVal)

newtype WalAddress = WalAddress { unWalAddress :: Core.Address }
    deriving (Eq, Generic, Ord, Show)

deriveSafeBuildable ''WalAddress
instance BuildableSafeGen WalAddress where
    buildSafeGen _ (WalAddress addr) =
        bprint ("address: " % build) addr

instance Buildable [WalAddress] where
    build = bprint (buildList build)

instance ToJSON WalAddress where
    toJSON (WalAddress c) = String $ sformat Core.addressF c

instance FromJSON WalAddress where
    parseJSON (String a) = case Core.decodeTextAddress a of
        Left e     -> fail $ "Not a valid Cardano Address: " <> toString e
        Right addr -> pure (WalAddress addr)
    parseJSON x = typeMismatch "parseJSON failed for Address" x

instance Arbitrary WalAddress where
    arbitrary = fmap WalAddress arbitrary

instance ToSchema WalAddress where
    declareNamedSchema _ =
        pure $ NamedSchema (Just "Address") $ mempty
            & type_ .~ SwaggerString
            & format ?~ "base58"

instance FromHttpApiData WalAddress where
    parseQueryParam = fmap (fmap WalAddress) Core.decodeTextAddress

instance ToHttpApiData WalAddress where
    toQueryParam (WalAddress a) = sformat build a

deriving instance Hashable WalAddress
deriving instance NFData WalAddress

newtype WalletTimestamp = WalletTimestamp Core.Timestamp
    deriving (Eq, Generic, Ord, Show)

deriveSafeBuildable ''WalletTimestamp
instance BuildableSafeGen WalletTimestamp where
    buildSafeGen _ (WalletTimestamp ts) =
        bprint ("timestamp: " % build) ts

-- | Represents according to 'apiTimeFormat' format.
instance ToJSON WalletTimestamp where
    toJSON (WalletTimestamp timestamp) =
        let utcTime = timestamp ^. Core.timestampToUTCTimeL
        in  String $ showApiUtcTime utcTime

instance ToHttpApiData WalletTimestamp where
    toQueryParam (WalletTimestamp timestamp) = view (Core.timestampToUTCTimeL . to showApiUtcTime) $ timestamp

instance FromHttpApiData WalletTimestamp where
    parseQueryParam t =
        maybe
            (Left ("Couldn't parse timestamp or datetime out of: " <> t))
            (Right . WalletTimestamp)
            (Core.parseTimestamp t)

-- | Parses from both UTC time in 'apiTimeFormat' format and a fractional
-- timestamp format.
instance FromJSON WalletTimestamp where
    parseJSON = withText "Timestamp" $ \t ->
        maybe
            (fail ("Couldn't parse timestamp or datetime out of: " <> toString t))
            (pure . WalletTimestamp)
            (Core.parseTimestamp t)

instance Arbitrary WalletTimestamp where
    arbitrary = fmap WalletTimestamp arbitrary

instance ToSchema WalletTimestamp where
    declareNamedSchema _ =
        pure $ NamedSchema (Just "Timestamp") $ mempty
            & type_ .~ SwaggerString
            & description ?~ "Time in ISO 8601 format"

--
-- Domain-specific types, mostly placeholders.
--

instance Semigroup WalletPassPhrase where
    WalletPassPhrase a <> WalletPassPhrase b = WalletPassPhrase (a <> b)

instance Monoid WalletPassPhrase where
    mempty = WalletPassPhrase mempty
    mappend = (<>)

type WalletName = Text

-- | Wallet's Assurance Level
data AssuranceLevel =
    NormalAssurance
  | StrictAssurance
  deriving (Eq, Ord, Show, Enum, Bounded)

instance Arbitrary AssuranceLevel where
    arbitrary = elements [minBound .. maxBound]

deriveJSON
    Aeson.defaultOptions
        { A.constructorTagModifier = toString . toLower . dropEnd 9 . fromString
        }
    ''AssuranceLevel

instance ToSchema AssuranceLevel where
    declareNamedSchema _ =
        pure $ NamedSchema (Just "AssuranceLevel") $ mempty
            & type_ .~ SwaggerString
            & enum_ ?~ ["normal", "strict"]

deriveSafeBuildable ''AssuranceLevel
instance BuildableSafeGen AssuranceLevel where
    buildSafeGen _ NormalAssurance = "assurance level: normal"
    buildSafeGen _ StrictAssurance = "assurance level: strict"

-- | A Wallet ID.
newtype WalletId = WalletId Text deriving (Show, Eq, Ord, Generic)

exampleWalletId :: WalletId
exampleWalletId = WalletId "J7rQqaLLHBFPrgJXwpktaMB1B1kQBXAyc2uRSfRPzNVGiv6TdxBzkPNBUWysZZZdhFG9gRy3sQFfX5wfpLbi4XTFGFxTg"

deriveJSON Aeson.defaultOptions ''WalletId

instance ToSchema WalletId where
  declareNamedSchema = genericDeclareNamedSchema defaultSchemaOptions

instance ToJSONKey WalletId

instance Arbitrary WalletId where
    arbitrary = elements [exampleWalletId]

deriveSafeBuildable ''WalletId
instance BuildableSafeGen WalletId where
    buildSafeGen _ (WalletId wid) =
        bprint ("wallet id: " % build) wid

instance FromHttpApiData WalletId where
    parseQueryParam = Right . WalletId

instance ToHttpApiData WalletId where
    toQueryParam (WalletId wid) = wid

instance Hashable WalletId
instance NFData WalletId

-- | A Wallet Operation
data WalletOperation =
    CreateWallet
  | RestoreWallet
  deriving (Eq, Show, Enum, Bounded)

instance Arbitrary WalletOperation where
    arbitrary = elements [minBound .. maxBound]

-- Drops the @Wallet@ suffix.
deriveJSON Aeson.defaultOptions  { A.constructorTagModifier = reverse . drop 6 . reverse . map C.toLower
                                    } ''WalletOperation

instance ToSchema WalletOperation where
    declareNamedSchema _ =
        pure $ NamedSchema (Just "WalletOperation") $ mempty
            & type_ .~ SwaggerString
            & enum_ ?~ ["create", "restore"]

deriveSafeBuildable ''WalletOperation
instance BuildableSafeGen WalletOperation where
    buildSafeGen _ CreateWallet  = "operation: create"
    buildSafeGen _ RestoreWallet = "operation: restore"


newtype BackupPhrase = BackupPhrase
    { unBackupPhrase :: Mnemonic 12
    }
    deriving stock (Eq, Show)
    deriving newtype (ToJSON, FromJSON, Arbitrary)

instance ToSchema BackupPhrase where
    declareNamedSchema _ =
        pure
            . NamedSchema (Just "V1BackupPhrase")
            $ toSchema (Proxy @(Mnemonic 12))

-- | A type modelling the request for a new 'Wallet'.
data NewWallet = NewWallet {
      newwalBackupPhrase     :: !BackupPhrase
    , newwalSpendingPassword :: !(Maybe SpendingPassword)
    , newwalAssuranceLevel   :: !AssuranceLevel
    , newwalName             :: !WalletName
    , newwalOperation        :: !WalletOperation
    } deriving (Eq, Show, Generic)

deriveJSON Aeson.defaultOptions  ''NewWallet

instance Arbitrary NewWallet where
  arbitrary = NewWallet <$> arbitrary
                        <*> pure Nothing
                        <*> arbitrary
                        <*> pure "My Wallet"
                        <*> arbitrary

instance ToSchema NewWallet where
  declareNamedSchema =
    genericSchemaDroppingPrefix "newwal" (\(--^) props -> props
      & ("backupPhrase"     --^ "Backup phrase to restore the wallet.")
      & ("spendingPassword" --^ "Optional (but recommended) password to protect the wallet on sensitive operations.")
      & ("assuranceLevel"   --^ "Desired assurance level based on the number of confirmations counter of each transaction.")
      & ("name"             --^ "Wallet's name.")
      & ("operation"        --^ "Create a new wallet or Restore an existing one.")
    )

deriveSafeBuildable ''NewWallet
instance BuildableSafeGen NewWallet where
    buildSafeGen _ NewWallet{..} = bprint
        ( "New wallet"
        % "\n  " % build
        % "\n  name: " % build
        % "\n  " % build
        )
        newwalOperation
        newwalName
        newwalAssuranceLevel

-- | Type for representation of serialized transaction in Base16-format.
-- We use it for external wallets (to send/receive raw transaction during
-- external signing).
newtype TransactionAsBase16 = TransactionAsBase16Unsafe
    { eoswalTransactionAsBase16 :: Text
    } deriving (Eq, Generic, Ord, Show)

deriveJSON Aeson.defaultOptions ''TransactionAsBase16
instance Arbitrary TransactionAsBase16 where
    arbitrary = TransactionAsBase16Unsafe <$> pure
        "839f8200d8185826825820e981442c2be40475bb42193ca35907861d90715854de6fcba767b98f1789b51219439aff9f8282d818584a83581ce7fe8e468d2249f18cd7bf9aec0d4374b7d3e18609ede8589f82f7f0a20058208200581c240596b9b63fc010c06fbe92cf6f820587406534795958c411e662dc014443c0688e001a6768cc861b0037699e3ea6d064ffa0"

instance ToSchema TransactionAsBase16 where
    declareNamedSchema =
        genericSchemaDroppingPrefix "eoswal" (\(--^) props -> props
            & ("transactionAsBase16" --^ "Serialized transaction in Base16-format.")
        )

deriveSafeBuildable ''TransactionAsBase16
instance BuildableSafeGen TransactionAsBase16 where
    buildSafeGen _ TransactionAsBase16Unsafe{..} = bprint
        ("tx in hex-form: " % build)
        eoswalTransactionAsBase16

-- | Type for representation of transaction signature in Base16-format.
-- We use it for external wallet. Please note that technically there's no
-- signature of transaction, but signature of the hash of transaction.
newtype TransactionSignatureAsBase16 = TransactionSignatureAsBase16Unsafe
    { rawTransactionSignatureAsBase16 :: Text
    } deriving (Eq, Generic, Ord, Show)

deriveJSON Aeson.defaultOptions ''TransactionSignatureAsBase16
instance Arbitrary TransactionSignatureAsBase16 where
    arbitrary = TransactionSignatureAsBase16Unsafe <$> pure
        "5840709cc240ac9ad78cbf47c3eec76df917423943e34339277593e8e2b8c9f9f2e59583023bfbd8e26c40dff6a7fa424600f9b942819533d8afee37a5ac6d813207"

instance ToSchema TransactionSignatureAsBase16 where
    declareNamedSchema =
        genericSchemaDroppingPrefix "raw" (\(--^) props -> props
            & ("transactionSignatureAsBase16" --^ "Signature of the hash of transaction in Base16-format.")
        )

deriveSafeBuildable ''TransactionSignatureAsBase16
instance BuildableSafeGen TransactionSignatureAsBase16 where
    buildSafeGen _ TransactionSignatureAsBase16Unsafe{..} = bprint
        ("tx signature in hex-form: " % build)
        rawTransactionSignatureAsBase16

-- | Makes tx signature as Base16-text.
mkTransactionSignatureAsBase16 :: Core.Signature Txp.TxSigData -> TransactionSignatureAsBase16
mkTransactionSignatureAsBase16 (Core.Signature txSig) =
    TransactionSignatureAsBase16Unsafe . Base16.encode . CC.unXSignature $ txSig

instance Buildable [PublicKey] where
    build = bprint (buildList build)


data UpdateEosWallet = UpdateEosWallet
    { ueowalAssuranceLevel :: !AssuranceLevel
    , ueowalName           :: !Text
    , ueowalAddressPoolGap :: !AddressPoolGap
    } deriving (Eq, Show, Generic)

deriveJSON Aeson.defaultOptions  ''UpdateEosWallet

instance ToSchema UpdateEosWallet where
    declareNamedSchema =
        genericSchemaDroppingPrefix "ueowal" (\(--^) props -> props
            & ("assuranceLevel" --^ "New assurance level.")
            & ("name"           --^ "New wallet's name.")
            & ("addressPoolGap" --^ "New address pool gap.")
        )

instance Arbitrary UpdateEosWallet where
    arbitrary = UpdateEosWallet
        <$> arbitrary
        <*> pure "My EosWallet"
        <*> arbitrary

deriveSafeBuildable ''UpdateEosWallet
instance BuildableSafeGen UpdateEosWallet where
    buildSafeGen _ UpdateEosWallet{..} = bprint
        ( "Update for externally-owned sequential wallet"
        % "\n  name: " % build
        % "\n  " % build
        % "\n  " % build
        )
        ueowalName
        ueowalAssuranceLevel
        ueowalAddressPoolGap

-- | A type modelling the update of an existing wallet.
data WalletUpdate = WalletUpdate {
      uwalAssuranceLevel :: !AssuranceLevel
    , uwalName           :: !Text
    } deriving (Eq, Show, Generic)

deriveJSON Aeson.defaultOptions  ''WalletUpdate

instance ToSchema WalletUpdate where
  declareNamedSchema =
    genericSchemaDroppingPrefix "uwal" (\(--^) props -> props
      & ("assuranceLevel" --^ "New assurance level.")
      & ("name"           --^ "New wallet's name.")
    )

instance Arbitrary WalletUpdate where
  arbitrary = WalletUpdate <$> arbitrary
                           <*> pure "My Wallet"

deriveSafeBuildable ''WalletUpdate
instance BuildableSafeGen WalletUpdate where
    buildSafeGen _ WalletUpdate{..} = bprint
        ( "Update for wallet"
        % "\n  " % build
        % "\n  name: " % build
        )
        uwalAssuranceLevel
        uwalName

newtype EstimatedCompletionTime = EstimatedCompletionTime (MeasuredIn 'Milliseconds Word)
  deriving (Show, Eq)

mkEstimatedCompletionTime :: Word -> EstimatedCompletionTime
mkEstimatedCompletionTime = EstimatedCompletionTime . MeasuredIn

instance Ord EstimatedCompletionTime where
    compare (EstimatedCompletionTime (MeasuredIn w1))
            (EstimatedCompletionTime (MeasuredIn w2)) = compare w1 w2

instance Arbitrary EstimatedCompletionTime where
    arbitrary = EstimatedCompletionTime . MeasuredIn <$> arbitrary

deriveSafeBuildable ''EstimatedCompletionTime
instance BuildableSafeGen EstimatedCompletionTime where
    buildSafeGen _ (EstimatedCompletionTime (MeasuredIn w)) = bprint
        ("estimated completion time: " % build % " milliseconds")
        w

instance ToJSON EstimatedCompletionTime where
    toJSON (EstimatedCompletionTime (MeasuredIn w)) =
        object [ "quantity" .= toJSON w
               , "unit"     .= String "milliseconds"
               ]

instance FromJSON EstimatedCompletionTime where
    parseJSON = withObject "EstimatedCompletionTime" $ \sl -> mkEstimatedCompletionTime <$> sl .: "quantity"

instance ToSchema EstimatedCompletionTime where
    declareNamedSchema _ =
        pure $ NamedSchema (Just "EstimatedCompletionTime") $ mempty
            & type_ .~ SwaggerObject
            & required .~ ["quantity", "unit"]
            & properties .~ (mempty
                & at "quantity" ?~ (Inline $ mempty
                    & type_ .~ SwaggerNumber
                    & minimum_ .~ Just 0
                    )
                & at "unit" ?~ (Inline $ mempty
                    & type_ .~ SwaggerString
                    & enum_ ?~ ["milliseconds"]
                    )
                )

newtype SyncThroughput
    = SyncThroughput (MeasuredIn 'BlocksPerSecond Core.BlockCount)
  deriving (Show, Eq)

mkSyncThroughput :: Core.BlockCount -> SyncThroughput
mkSyncThroughput = SyncThroughput . MeasuredIn

instance Ord SyncThroughput where
    compare (SyncThroughput (MeasuredIn (Core.BlockCount b1)))
            (SyncThroughput (MeasuredIn (Core.BlockCount b2))) =
        compare b1 b2

instance Arbitrary SyncThroughput where
    arbitrary = SyncThroughput . MeasuredIn <$> arbitrary

deriveSafeBuildable ''SyncThroughput
instance BuildableSafeGen SyncThroughput where
    buildSafeGen _ (SyncThroughput (MeasuredIn (Core.BlockCount blocks))) = bprint
        ("sync throughput: " % build % " blocks per second")
        blocks

instance ToJSON SyncThroughput where
    toJSON (SyncThroughput (MeasuredIn (Core.BlockCount blocks))) =
      object [ "quantity" .= toJSON blocks
             , "unit"     .= String "blocksPerSecond"
             ]

instance FromJSON SyncThroughput where
    parseJSON = withObject "SyncThroughput" $ \sl -> mkSyncThroughput . Core.BlockCount <$> sl .: "quantity"

instance ToSchema SyncThroughput where
    declareNamedSchema _ =
        pure $ NamedSchema (Just "SyncThroughput") $ mempty
            & type_ .~ SwaggerObject
            & required .~ ["quantity", "unit"]
            & properties .~ (mempty
                & at "quantity" ?~ (Inline $ mempty
                    & type_ .~ SwaggerNumber
                    )
                & at "unit" ?~ (Inline $ mempty
                    & type_ .~ SwaggerString
                    & enum_ ?~ ["blocksPerSecond"]
                    )
                )

data SyncProgress = SyncProgress {
    spEstimatedCompletionTime :: !EstimatedCompletionTime
  , spThroughput              :: !SyncThroughput
  , spPercentage              :: !SyncPercentage
  } deriving (Show, Eq, Ord, Generic)

deriveJSON Aeson.defaultOptions ''SyncProgress

instance ToSchema SyncProgress where
    declareNamedSchema =
        genericSchemaDroppingPrefix "sp" (\(--^) props -> props
            & "estimatedCompletionTime"
            --^ "The estimated time the wallet is expected to be fully sync, based on the information available."
            & "throughput"
            --^ "The sync throughput, measured in blocks/s."
            & "percentage"
            --^ "The sync percentage, from 0% to 100%."
        )

deriveSafeBuildable ''SyncProgress
-- Nothing secret to redact for a SyncProgress.
instance BuildableSafeGen SyncProgress where
    buildSafeGen _ SyncProgress {..} = bprint
        ( "Sync progress"
        % "\n  " % build
        % "\n  " % build
        % "\n  percentage: " % build
        )
        spEstimatedCompletionTime
        spThroughput
        spPercentage

instance Example SyncProgress where
    example = do
        exPercentage <- example
        pure $ SyncProgress
            { spEstimatedCompletionTime = mkEstimatedCompletionTime 3000
            , spThroughput              = mkSyncThroughput (Core.BlockCount 400)
            , spPercentage              = exPercentage
            }

instance Arbitrary SyncProgress where
  arbitrary = SyncProgress <$> arbitrary
                           <*> arbitrary
                           <*> arbitrary

data SyncState =
      Restoring SyncProgress
    -- ^ Restoring from seed or from backup.
    | Synced
    -- ^ Following the blockchain.
    deriving (Eq, Show, Ord)

instance ToJSON SyncState where
    toJSON ss = object [ "tag"  .= toJSON (renderAsTag ss)
                       , "data" .= renderAsData ss
                       ]
      where
        renderAsTag :: SyncState -> Text
        renderAsTag (Restoring _) = "restoring"
        renderAsTag Synced        = "synced"

        renderAsData :: SyncState -> Value
        renderAsData (Restoring sp) = toJSON sp
        renderAsData Synced         = Null

instance FromJSON SyncState where
    parseJSON = withObject "SyncState" $ \ss -> do
        t <- ss .: "tag"
        case (t :: Text) of
            "synced"    -> pure Synced
            "restoring" -> Restoring <$> ss .: "data"
            _           -> typeMismatch "unrecognised tag" (Object ss)

instance ToSchema SyncState where
    declareNamedSchema _ = do
      syncProgress <- declareSchemaRef @SyncProgress Proxy
      pure $ NamedSchema (Just "SyncState") $ mempty
          & type_ .~ SwaggerObject
          & required .~ ["tag"]
          & properties .~ (mempty
              & at "tag" ?~ (Inline $ mempty
                  & type_ .~ SwaggerString
                  & enum_ ?~ ["restoring", "synced"]
                  )
              & at "data" ?~ syncProgress
              )

instance Arbitrary SyncState where
  arbitrary = oneof [ Restoring <$> arbitrary
                    , pure Synced
                    ]

-- | A 'Wallet'.
data Wallet = Wallet {
      walId                         :: !WalletId
    , walName                       :: !WalletName
    , walBalance                    :: !WalletCoin
    , walHasSpendingPassword        :: !Bool
    , walSpendingPasswordLastUpdate :: !WalletTimestamp
    , walCreatedAt                  :: !WalletTimestamp
    , walAssuranceLevel             :: !AssuranceLevel
    , walSyncState                  :: !SyncState
    } deriving (Eq, Ord, Show, Generic)

deriveJSON Aeson.defaultOptions ''Wallet

instance ToSchema Wallet where
    declareNamedSchema =
        genericSchemaDroppingPrefix "wal" (\(--^) props -> props
            & "id"
            --^ "Unique wallet identifier."
            & "name"
            --^ "Wallet's name."
            & "balance"
            --^ "Current balance, in Lovelace."
            & "hasSpendingPassword"
            --^ "Whether or not the wallet has a passphrase."
            & "spendingPasswordLastUpdate"
            --^ "The timestamp that the passphrase was last updated."
            & "createdAt"
            --^ "The timestamp that the wallet was created."
            & "assuranceLevel"
            --^ "The assurance level of the wallet."
            & "syncState"
            --^ "The sync state for this wallet."
        )

instance Arbitrary Wallet where
  arbitrary = Wallet <$> arbitrary
                     <*> pure "My wallet"
                     <*> arbitrary
                     <*> arbitrary
                     <*> arbitrary
                     <*> arbitrary
                     <*> arbitrary
                     <*> arbitrary

deriveSafeBuildable ''Wallet
instance BuildableSafeGen Wallet where
  buildSafeGen _ Wallet{..} = bprint
    ( "Wallet"
    % "\n  " % build
    % "\n  name: " % build
    % "\n  balance: " % build
    )
    walId
    walName
    walBalance

instance Buildable [Wallet] where
    build = bprint (buildList build)

instance ToSchema PublicKey where
    declareNamedSchema _ =
        pure $ NamedSchema (Just "PublicKey") $ mempty
            & type_ .~ SwaggerString
            & format ?~ "base58"

-- | Externally-owned sequential (EOS) wallet (mobile client or hardware wallet).
data EosWallet = EosWallet
    { eoswalId             :: !WalletId
    , eoswalName           :: !WalletName
    , eoswalAddressPoolGap :: !AddressPoolGap
    , eoswalBalance        :: !WalletCoin
    , eoswalAssuranceLevel :: !AssuranceLevel
    , eoswalCreatedAt      :: !WalletTimestamp
    } deriving (Eq, Ord, Show, Generic)

deriveJSON Aeson.defaultOptions ''EosWallet
instance ToSchema EosWallet where
    declareNamedSchema =
        genericSchemaDroppingPrefix "eoswal" (\(--^) props -> props
            & ("id"                 --^ "Unique wallet's identifier.")
            & ("name"               --^ "Wallet's name.")
            & ("addressPoolGap"     --^ "Address pool gap for this wallet.")
            & ("balance"            --^ "Current balance, in Lovelaces.")
            & ("assuranceLevel"     --^ "The assurance level of the wallet.")
            & ("createdAt"           --^ "The timestamp that the wallet was created.")
        )

instance Arbitrary EosWallet where
    arbitrary = EosWallet <$> arbitrary
                          <*> pure "My EOS-wallet"
                          <*> arbitrary
                          <*> arbitrary
                          <*> arbitrary
                          <*> arbitrary

deriveSafeBuildable ''EosWallet
instance BuildableSafeGen EosWallet where
  buildSafeGen _ EosWallet{..} = bprint
    ( "Externally-owned sequential wallet"
    % "\n  " % build
    % "\n  name: " % build
    % "\n  balance: " % build
    % "\n  " % build
    )
    eoswalId
    eoswalName
    eoswalBalance
    eoswalAddressPoolGap

instance Buildable [EosWallet] where
    build = bprint (buildList build)

--------------------------------------------------------------------------------
-- Addresses
--------------------------------------------------------------------------------

-- | Whether an address is valid or not.
newtype AddressValidity = AddressValidity { isValid :: Bool }
  deriving (Eq, Show, Generic)

deriveJSON Aeson.defaultOptions ''AddressValidity

instance ToSchema AddressValidity where
    declareNamedSchema = genericSchemaDroppingPrefix "is" (const identity)

instance Arbitrary AddressValidity where
  arbitrary = AddressValidity <$> arbitrary

deriveSafeBuildable ''AddressValidity
instance BuildableSafeGen AddressValidity where
    buildSafeGen _ AddressValidity{..} =
        bprint ("address is valid: " % build) isValid

-- | An address is either recognised as "ours" or not. An address that is not
--   recognised may still be ours e.g. an address generated by another wallet instance
--   will not be considered "ours" until the relevant transaction is confirmed.
--
--   In other words, `AddressAmbiguousOwnership` makes an inconclusive statement about
--   an address, whereas `AddressOwnership` is unambiguous.
data AddressOwnership
    = AddressIsOurs
    | AddressAmbiguousOwnership
    deriving (Show, Eq, Generic, Ord)

instance ToJSON AddressOwnership where
    toJSON = genericToJSON optsADTCamelCase

instance FromJSON AddressOwnership where
    parseJSON = genericParseJSON optsADTCamelCase

instance ToSchema AddressOwnership where
    declareNamedSchema _ =
        pure $ NamedSchema (Just "AddressOwnership") $ mempty
            & type_ .~ SwaggerString
            & enum_ ?~ ["isOurs", "ambiguousOwnership"]

instance Arbitrary AddressOwnership where
    arbitrary = oneof
        [ pure AddressIsOurs
        , pure AddressAmbiguousOwnership
        ]

-- | Address with associated metadata locating it in an account in a wallet.
data WAddressMeta = WAddressMeta
    { _wamWalletId     :: !WalletId
    , _wamAccountIndex :: !Word32
    , _wamAddressIndex :: !Word32
    , _wamAddress      :: !WalAddress
    } deriving (Eq, Ord, Show, Generic, Typeable)

instance Hashable WAddressMeta
instance NFData WAddressMeta

instance Buildable WAddressMeta where
    build WAddressMeta{..} = bprint
        ( "Address meta info:"
        % "\n  " % build
        % "\n  account ix: " % build
        % "\n  address ix: " % build
        % "\n  " % build
        )
        _wamWalletId
        _wamAccountIndex
        _wamAddressIndex
        _wamAddress

--------------------------------------------------------------------------------
-- Accounts
--------------------------------------------------------------------------------

data BatchImportResult a = BatchImportResult
    { aimTotalSuccess :: !Natural
    , aimFailures     :: ![a]
    } deriving (Show, Ord, Eq, Generic)

instance Buildable (BatchImportResult a) where
    build res = bprint
        ("BatchImportResult (success:"%int%", failures:"%int%")")
        (aimTotalSuccess res)
        (length $ aimFailures res)

instance ToJSON a => ToJSON (BatchImportResult a) where
    toJSON = genericToJSON Aeson.defaultOptions

instance FromJSON a => FromJSON (BatchImportResult a) where
    parseJSON = genericParseJSON Aeson.defaultOptions

instance (ToJSON a, ToSchema a, Arbitrary a) => ToSchema (BatchImportResult a) where
    declareNamedSchema =
        genericSchemaDroppingPrefix "aim" (\(--^) props -> props
            & ("totalSuccess" --^ "Total number of entities successfully imported")
            & ("failures" --^ "Entities failed to be imported, if any")
        )

instance Arbitrary a => Arbitrary (BatchImportResult a) where
    arbitrary = BatchImportResult
        <$> arbitrary
        <*> scale (`mod` 3) arbitrary -- NOTE Small list

instance Arbitrary a => Example (BatchImportResult a)

instance Semigroup (BatchImportResult a) where
    (BatchImportResult a0 b0) <> (BatchImportResult a1 b1) =
        BatchImportResult (a0 + a1) (b0 <> b1)

instance Monoid (BatchImportResult a) where
    mempty = BatchImportResult 0 mempty


-- | Summary about single address.
data WalletAddress = WalletAddress
    { addrId        :: !WalAddress
    , addrUsed      :: !Bool
    , addrOwnership :: !AddressOwnership
    } deriving (Show, Eq, Generic, Ord)

deriveJSON Aeson.defaultOptions ''WalletAddress

instance ToSchema WalletAddress where
    declareNamedSchema =
        genericSchemaDroppingPrefix "addr" (\(--^) props -> props
            & ("id"            --^ "Actual address.")
            & ("used"          --^ "True if this address has been used.")
            & ("ownership"     --^ "'isOurs' if this address is recognised as ours, 'ambiguousOwnership' if the node doesn't have information to make a unambiguous statement.")
        )

instance Arbitrary WalletAddress where
    arbitrary = WalletAddress <$> arbitrary
                              <*> arbitrary
                              <*> arbitrary

newtype AccountIndex = AccountIndex { getAccIndex :: Word32 }
    deriving (Show, Eq, Ord, Generic)

newtype AccountIndexError = AccountIndexError Word32
    deriving (Eq, Show)

instance Buildable AccountIndexError where
    build (AccountIndexError i) =
        bprint
            ("Account index should be in range ["%int%".."%int%"], but "%int%" was provided.")
            (getAccIndex minBound)
            (getAccIndex maxBound)
            i

mkAccountIndex :: Word32 -> Either AccountIndexError AccountIndex
mkAccountIndex index
    | index >= getAccIndex minBound = Right $ AccountIndex index
    | otherwise = Left $ AccountIndexError index

mkAccountIndexM :: MonadFail m => Word32 -> m AccountIndex
mkAccountIndexM =
    either (fail . toString . sformat build) pure . mkAccountIndex

unsafeMkAccountIndex :: Word32 -> AccountIndex
unsafeMkAccountIndex =
    either (error . sformat build) identity . mkAccountIndex

instance Bounded AccountIndex where
    -- NOTE: minimum for hardened key. See https://iohk.myjetbrains.com/youtrack/issue/CO-309
    minBound = AccountIndex 2147483648
    maxBound = AccountIndex maxBound

instance Enum AccountIndex where
    fromEnum (AccountIndex a) = fromEnum a
    toEnum a = case mkAccountIndex (toEnum a) of
        Left err -> error (sformat build err)
        Right ix -> ix

instance ToJSON AccountIndex where
    toJSON = toJSON . getAccIndex

instance FromJSON AccountIndex where
    parseJSON =
        mkAccountIndexM <=< parseJSON

instance Arbitrary AccountIndex where
    arbitrary =
        AccountIndex <$> choose (getAccIndex minBound, getAccIndex maxBound)

deriveSafeBuildable ''AccountIndex
-- Nothing secret to redact for a AccountIndex.
instance BuildableSafeGen AccountIndex where
    buildSafeGen _ (AccountIndex ix) =
        bprint ("account ix: " % build) ix

instance ToParamSchema AccountIndex where
    toParamSchema _ = mempty
        & type_ .~ SwaggerNumber
        & minimum_ .~ Just (fromIntegral $ getAccIndex minBound)
        & maximum_ .~ Just (fromIntegral $ getAccIndex maxBound)

instance ToSchema AccountIndex where
    declareNamedSchema =
        pure . paramSchemaToNamedSchema defaultSchemaOptions

instance FromHttpApiData AccountIndex where
    parseQueryParam =
        first (sformat build) . mkAccountIndex <=< parseQueryParam

instance ToHttpApiData AccountIndex where
    toQueryParam =
        fromString . show . getAccIndex

data AccountPublicKeyWithIx = AccountPublicKeyWithIx
    { accpubkeywithixPublicKey :: !PublicKey
    , accpubkeywithixIndex     :: !AccountIndex
    } deriving (Show, Ord, Eq, Generic)

deriveJSON Aeson.defaultOptions ''AccountPublicKeyWithIx

instance Arbitrary AccountPublicKeyWithIx where
    arbitrary = AccountPublicKeyWithIx <$> arbitrary
                                       <*> arbitrary

instance ToSchema AccountPublicKeyWithIx where
    declareNamedSchema =
        genericSchemaDroppingPrefix "accpubkeywithix" (\(--^) props -> props
            & ("publicKey" --^ "Public key of account in EOS-wallet.")
            & ("index"     --^ "Index of account in EOS-wallet.")
        )

deriveSafeBuildable ''AccountPublicKeyWithIx
instance BuildableSafeGen AccountPublicKeyWithIx where
    buildSafeGen _ AccountPublicKeyWithIx{..} = bprint
        ("account public key: " % build % ", " % build)
        accpubkeywithixPublicKey
        accpubkeywithixIndex

instance Buildable (NonEmpty AccountPublicKeyWithIx) where
    build = bprint (buildList build) . NE.toList

-- | A type modelling the request for a new 'EosWallet',
-- on the mobile client or hardware wallet.
data NewEosWallet = NewEosWallet
    { neweoswalAccounts       :: !(NonEmpty AccountPublicKeyWithIx)
    , neweoswalAddressPoolGap :: !(Maybe AddressPoolGap)
    , neweoswalAssuranceLevel :: !AssuranceLevel
    , neweoswalName           :: !WalletName
    } deriving (Eq, Show, Generic)

deriveJSON Aeson.defaultOptions ''NewEosWallet
instance Arbitrary NewEosWallet where
    arbitrary = NewEosWallet <$> arbitrary
                             <*> arbitrary
                             <*> arbitrary
                             <*> pure "My EOS-wallet"

instance ToSchema NewEosWallet where
    declareNamedSchema =
        genericSchemaDroppingPrefix "neweoswal" (\(--^) props -> props
            & ("accounts"       --^ "External wallet's accounts public keys with indexes.")
            & ("addressPoolGap" --^ "Address pool gap for this wallet.")
            & ("assuranceLevel" --^ "Desired assurance level based on the number of confirmations counter of each transaction.")
            & ("name"           --^ "External wallet's name.")
        )

deriveSafeBuildable ''NewEosWallet
instance BuildableSafeGen NewEosWallet where
    buildSafeGen _ NewEosWallet{..} = bprint
        ( "Request for new EOS-wallet"
        % "\n  name: " % build
        % "\n  " % build
        % "\n  " % build
        % "\n  accounts:\n" % buildIndent 2 (buildList build)
        )
        neweoswalName
        neweoswalAddressPoolGap
        neweoswalAssuranceLevel
        neweoswalAccounts

-- | A wallet 'Account'.
data Account = Account
    { accIndex     :: !AccountIndex
    , accAddresses :: ![WalletAddress]
    , accAmount    :: !WalletCoin
    , accName      :: !Text
    , accWalletId  :: !WalletId
    } deriving (Show, Ord, Eq, Generic)


--
-- IxSet indices
--





-- | Datatype wrapping addresses for per-field endpoint
newtype AccountAddresses = AccountAddresses
    { acaAddresses :: [WalletAddress]
    } deriving (Show, Ord, Eq, Generic)

-- | Datatype wrapping balance for per-field endpoint
newtype AccountBalance = AccountBalance
    { acbAmount    :: WalletCoin
    } deriving (Show, Ord, Eq, Generic)

accountsHaveSameId :: Account -> Account -> Bool
accountsHaveSameId a b =
    accWalletId a == accWalletId b
    &&
    accIndex a == accIndex b

deriveJSON Aeson.defaultOptions ''Account
deriveJSON Aeson.defaultOptions ''AccountAddresses
deriveJSON Aeson.defaultOptions ''AccountBalance

instance ToSchema Account where
    declareNamedSchema =
        genericSchemaDroppingPrefix "acc" (\(--^) props -> props
            & ("index"     --^ "Account's index in the wallet, starting at 0.")
            & ("addresses" --^ "Public addresses pointing to this account.")
            & ("amount"    --^ "Available funds, in Lovelace.")
            & ("name"      --^ "Account's name.")
            & ("walletId"  --^ "Id of the wallet this account belongs to.")
          )

instance ToSchema AccountAddresses where
    declareNamedSchema =
        genericSchemaDroppingPrefix "aca" (\(--^) props -> props
            & ("addresses" --^ "Public addresses pointing to this account.")
          )

instance ToSchema AccountBalance where
    declareNamedSchema =
        genericSchemaDroppingPrefix "acb" (\(--^) props -> props
            & ("amount"    --^ "Available funds, in Lovelace.")
          )

instance Arbitrary Account where
    arbitrary = Account <$> arbitrary
                        <*> arbitrary
                        <*> arbitrary
                        <*> pure "My account"
                        <*> arbitrary

instance Arbitrary AccountAddresses where
    arbitrary =
        AccountAddresses <$> arbitrary

instance Arbitrary AccountBalance where
    arbitrary =
        AccountBalance <$> arbitrary

deriveSafeBuildable ''Account
instance BuildableSafeGen Account where
    buildSafeGen _ Account{..} = bprint
        ( "Account"
        % "\n  " % build
        % "\n  name: " % build
        % "\n  amount: " % build
        % "\n  " % build
        % "\n  addresses:\n" % buildIndent 2 (buildList build)
        )
        accIndex
        accName
        accAmount
        accWalletId
        accAddresses

instance Buildable AccountAddresses where
    build =
        bprint (buildList build) . acaAddresses

instance Buildable AccountBalance where
    build =
        bprint build . acbAmount

instance Buildable [Account] where
    build =
        bprint (buildList build)

-- | Account Update
data AccountUpdate = AccountUpdate {
    uaccName      :: !Text
  } deriving (Show, Eq, Generic)

deriveJSON Aeson.defaultOptions ''AccountUpdate

instance ToSchema AccountUpdate where
  declareNamedSchema =
    genericSchemaDroppingPrefix "uacc" (\(--^) props -> props
      & ("name" --^ "New account's name.")
    )

instance Arbitrary AccountUpdate where
  arbitrary = AccountUpdate <$> pure "myAccount"

deriveSafeBuildable ''AccountUpdate
instance BuildableSafeGen AccountUpdate where
    buildSafeGen _ AccountUpdate{..} =
        bprint ("Account update, new name: " % build) uaccName


-- | New Account
data NewAccount = NewAccount
  { naccSpendingPassword :: !(Maybe SpendingPassword)
  , naccName             :: !Text
  } deriving (Show, Eq, Generic)

deriveJSON Aeson.defaultOptions ''NewAccount

instance Arbitrary NewAccount where
  arbitrary = NewAccount <$> arbitrary
                         <*> arbitrary

instance ToSchema NewAccount where
  declareNamedSchema =
    genericSchemaDroppingPrefix "nacc" (\(--^) props -> props
      & ("spendingPassword" --^ "Wallet's protection password, required if defined.")
      & ("name"             --^ "Account's name.")
    )

deriveSafeBuildable ''NewAccount
instance BuildableSafeGen NewAccount where
    buildSafeGen _ NewAccount{..} =
        bprint ("New account with name " % build) naccName

deriveSafeBuildable ''WalletAddress
instance BuildableSafeGen WalletAddress where
    buildSafeGen _ WalletAddress{..} = bprint
        (build % ", is it used: " % build)
        addrId
        addrUsed

instance Buildable [WalletAddress] where
    build = bprint (buildList build)

-- | Create a new Address
data NewAddress = NewAddress
  { newaddrSpendingPassword :: !(Maybe SpendingPassword)
  , newaddrAccountIndex     :: !AccountIndex
  , newaddrWalletId         :: !WalletId
  } deriving (Show, Eq, Generic)

deriveJSON Aeson.defaultOptions ''NewAddress

instance ToSchema NewAddress where
  declareNamedSchema =
    genericSchemaDroppingPrefix "newaddr" (\(--^) props -> props
      & ("spendingPassword" --^ "Wallet's protection password, required if defined.")
      & ("accountIndex"     --^ "Target account's index to store this address in.")
      & ("walletId"         --^ "Corresponding wallet identifier.")
    )

instance Arbitrary NewAddress where
  arbitrary = NewAddress <$> arbitrary
                         <*> arbitrary
                         <*> arbitrary

deriveSafeBuildable ''NewAddress
instance BuildableSafeGen NewAddress where
    buildSafeGen _ NewAddress{..} = bprint
        ( "Request for new address"
        % "\n  " % build
        % "\n  " % build
        )
        newaddrWalletId
        newaddrAccountIndex


type AddressIndex = Word32

-- | If we're talking about "hardened" indexes - we mean values above
-- 2^31 == maxBound `div` 2 + 1
hardenedValue :: Word32
hardenedValue = maxBound `div` 2 + 1

addressLevelToWord32 :: AddressLevel -> Word32
addressLevelToWord32 (AddressLevelNormal lvl)   = lvl
addressLevelToWord32 (AddressLevelHardened lvl) = lvl + hardenedValue

word32ToAddressLevel :: Word32 -> AddressLevel
word32ToAddressLevel lvl =
    if lvl <= hardenedValue then
        AddressLevelNormal lvl
    else
        AddressLevelHardened (lvl - hardenedValue)

data AddressLevel
    = AddressLevelHardened Word32
    | AddressLevelNormal Word32
    deriving (Eq, Generic, Ord, Show)

instance ToSchema AddressLevel where
    declareNamedSchema _ = do
        NamedSchema _ word32Schema <- declareNamedSchema (Proxy @Word32)
        pure $ NamedSchema (Just "AddressLevel") $ word32Schema
            & description ?~ mconcat
                [ "Address path level according to BIP-32 definition. "
                , "Levels in the (0..2^31-1) range are treated as normal, "
                , "those in the (2^31..2^32-1) range are treated as hardened."
                ]

instance Arbitrary AddressLevel where
    arbitrary = oneof
        [ AddressLevelHardened <$> arbitrary
        , AddressLevelNormal   <$> arbitrary
        ]

deriveSafeBuildable ''AddressLevel
instance BuildableSafeGen AddressLevel where
    buildSafeGen _ = \case
        AddressLevelNormal lvl   -> bprint build lvl
        AddressLevelHardened lvl -> bprint (build % "'") lvl

instance ToJSON AddressLevel where
    toJSON = toJSON . addressLevelToWord32

instance FromJSON AddressLevel where
    parseJSON = fmap word32ToAddressLevel . parseJSON

-- | A type incapsulating a password update request.
data PasswordUpdate = PasswordUpdate {
    pwdOld :: !SpendingPassword
  , pwdNew :: !SpendingPassword
  } deriving (Show, Eq, Generic)

deriveJSON Aeson.defaultOptions ''PasswordUpdate

instance ToSchema PasswordUpdate where
  declareNamedSchema =
    genericSchemaDroppingPrefix "pwd" (\(--^) props -> props
      & ("old" --^ "Old password.")
      & ("new" --^ "New passowrd.")
    )

instance Arbitrary PasswordUpdate where
  arbitrary = PasswordUpdate <$> arbitrary
                             <*> arbitrary

deriveSafeBuildable ''PasswordUpdate
instance BuildableSafeGen PasswordUpdate where
    buildSafeGen _ _ = bprint "Password update"

-- | 'EstimatedFees' represents the fees which would be generated
-- for a 'Payment' in case the latter would actually be performed.
data EstimatedFees = EstimatedFees {
    feeEstimatedAmount :: !WalletCoin
  } deriving (Show, Eq, Generic)

deriveJSON Aeson.defaultOptions ''EstimatedFees

instance ToSchema EstimatedFees where
  declareNamedSchema =
    genericSchemaDroppingPrefix "fee" (\(--^) props -> props
      & ("estimatedAmount" --^ "Estimated fees, in Lovelace.")
    )

instance Arbitrary EstimatedFees where
  arbitrary = EstimatedFees <$> arbitrary

deriveSafeBuildable ''EstimatedFees
instance BuildableSafeGen EstimatedFees where
    buildSafeGen _ EstimatedFees{..} = bprint
        ("Estimated fees: " % build) feeEstimatedAmount


-- | Maps an 'Address' to some 'Coin's, and it's
-- typically used to specify where to send money during a 'Payment'.
data PaymentDistribution = PaymentDistribution {
      pdAddress :: !WalAddress
    , pdAmount  :: !WalletCoin
    } deriving (Show, Ord, Eq, Generic)

deriveJSON Aeson.defaultOptions ''PaymentDistribution

instance ToSchema PaymentDistribution where
  declareNamedSchema =
    genericSchemaDroppingPrefix "pd" (\(--^) props -> props
      & ("address" --^ "Address to map coins to.")
      & ("amount"  --^ "Amount of coin to bind, in Lovelace.")
    )

instance Arbitrary PaymentDistribution where
  arbitrary = PaymentDistribution <$> arbitrary
                                  <*> arbitrary

deriveSafeBuildable ''PaymentDistribution
instance BuildableSafeGen PaymentDistribution where
    buildSafeGen _ PaymentDistribution{..} = bprint
        ( "payment distribution"
        % "\n  " % build
        % "\n  amount: " % build
        )
        pdAddress
        pdAmount


-- | A 'PaymentSource' encapsulate two essentially piece of data to reach for some funds:
-- a 'WalletId' and an 'AccountIndex' within it.
data PaymentSource = PaymentSource
  { psWalletId     :: !WalletId
  , psAccountIndex :: !AccountIndex
  } deriving (Show, Ord, Eq, Generic)

deriveJSON Aeson.defaultOptions ''PaymentSource

instance ToSchema PaymentSource where
  declareNamedSchema =
    genericSchemaDroppingPrefix "ps" (\(--^) props -> props
      & ("walletId"     --^ "Target wallet identifier to reach.")
      & ("accountIndex" --^ "Corresponding account's index on the wallet.")
    )

instance Arbitrary PaymentSource where
  arbitrary = PaymentSource <$> arbitrary
                            <*> arbitrary

deriveSafeBuildable ''PaymentSource
instance BuildableSafeGen PaymentSource where
    buildSafeGen _ PaymentSource{..} = bprint
        ( "source - " % build % ", " % build)
        psWalletId
        psAccountIndex


-- | A 'Payment' from one source account to one or more 'PaymentDistribution'(s).
data Payment = Payment
  { pmtSource           :: !PaymentSource
  , pmtDestinations     :: !(NonEmpty PaymentDistribution)
  , pmtGroupingPolicy   :: !(Maybe WalletInputSelectionPolicy)
  , pmtSpendingPassword :: !(Maybe SpendingPassword)
  } deriving (Show, Eq, Generic)

newtype WalletInputSelectionPolicy = WalletInputSelectionPolicy Core.InputSelectionPolicy
    deriving (Eq, Generic, Show)

deriveSafeBuildable ''WalletInputSelectionPolicy
instance BuildableSafeGen WalletInputSelectionPolicy where
    buildSafeGen _ (WalletInputSelectionPolicy policy) =
        bprint ("input selection policy: " % build) policy

instance ToJSON WalletInputSelectionPolicy where
    toJSON (WalletInputSelectionPolicy Core.OptimizeForSecurity)       = String "OptimizeForSecurity"
    toJSON (WalletInputSelectionPolicy Core.OptimizeForHighThroughput) = String "OptimizeForHighThroughput"

instance FromJSON WalletInputSelectionPolicy where
    parseJSON (String "OptimizeForSecurity")       = pure (WalletInputSelectionPolicy Core.OptimizeForSecurity)
    parseJSON (String "OptimizeForHighThroughput") = pure (WalletInputSelectionPolicy Core.OptimizeForHighThroughput)
    parseJSON x = typeMismatch "Not a valid InputSelectionPolicy" x

instance ToSchema WalletInputSelectionPolicy where
    declareNamedSchema _ =
        pure $ NamedSchema (Just "WalletInputSelectionPolicy") $ mempty
            & type_ .~ SwaggerString
            & enum_ ?~ ["OptimizeForSecurity", "OptimizeForHighThroughput"]

instance Arbitrary WalletInputSelectionPolicy where
    arbitrary = fmap WalletInputSelectionPolicy arbitrary


deriveJSON Aeson.defaultOptions ''Payment

instance Arbitrary Payment where
  arbitrary = Payment <$> arbitrary
                      <*> arbitrary
                      <*> arbitrary
                      <*> arbitrary

instance ToSchema Payment where
  declareNamedSchema =
    genericSchemaDroppingPrefix "pmt" (\(--^) props -> props
      & ("source"           --^ "Source for the payment.")
      & ("destinations"     --^ "One or more destinations for the payment.")
      & ("groupingPolicy"   --^ "Optional strategy to use for selecting the transaction inputs.")
      & ("spendingPassword" --^ "Wallet's protection password, required to spend funds if defined.")
    )

deriveSafeBuildable ''Payment
instance BuildableSafeGen Payment where
    buildSafeGen _ (Payment{..}) = bprint
        ( "Payment"
        % "\n  " % build
        % "\n  destinations:\n" % buildIndent 2 (buildList build)
        % "\n  " % build
        )
        pmtSource
        pmtDestinations
        pmtGroupingPolicy

----------------------------------------------------------------------------
-- TxId
----------------------------------------------------------------------------

newtype WalletTxId = WalletTxId Txp.TxId
    deriving (Eq, Generic, Ord, Show)

deriveSafeBuildable ''WalletTxId
instance BuildableSafeGen WalletTxId where
    buildSafeGen _ (WalletTxId txId) =
        bprint ("tx id: " % build) txId

instance Arbitrary WalletTxId where
  arbitrary = WalletTxId <$> arbitrary

instance ToJSON WalletTxId where
  toJSON (WalletTxId t) = String (sformat hashHexF t)

instance FromJSON WalletTxId where
    parseJSON = withText "TxId" $ \t -> do
       case decodeHash t of
           Left err -> fail $ "Failed to parse transaction ID: " <> toString err
           Right a  -> pure (WalletTxId a)

instance FromHttpApiData WalletTxId where
    parseQueryParam = fmap (fmap WalletTxId) decodeHash

instance ToHttpApiData WalletTxId where
    toQueryParam (WalletTxId txId) = sformat hashHexF txId

instance ToSchema WalletTxId where
    declareNamedSchema _ = declareNamedSchema (Proxy @Text)

----------------------------------------------------------------------------
  -- Transaction types
----------------------------------------------------------------------------

-- | The 'Transaction' type.
data TransactionType =
    LocalTransaction
  -- ^ This transaction is local, which means all the inputs
  -- and all the outputs belongs to the wallet from which the
  -- transaction was originated.
  | ForeignTransaction
  -- ^ This transaction is not local to this wallet.
  deriving (Show, Ord, Eq, Enum, Bounded)

instance Arbitrary TransactionType where
  arbitrary = elements [minBound .. maxBound]

-- Drops the @Transaction@ suffix.
deriveJSON defaultOptions { A.constructorTagModifier = reverse . drop 11 . reverse . map C.toLower
                          } ''TransactionType

instance ToSchema TransactionType where
    declareNamedSchema _ =
        pure $ NamedSchema (Just "TransactionType") $ mempty
            & type_ .~ SwaggerString
            & enum_ ?~ ["local", "foreign"]
            & description ?~ mconcat
                [ "A transaction is 'local' if all the inputs and outputs "
                , "belong to the current wallet. A transaction is foreign "
                , "if the transaction is not local to this wallet."
                ]

deriveSafeBuildable ''TransactionType
instance BuildableSafeGen TransactionType where
    buildSafeGen _ LocalTransaction   = "type: local"
    buildSafeGen _ ForeignTransaction = "type: foreign"


-- | The 'Transaction' @direction@
data TransactionDirection =
    IncomingTransaction
  -- ^ Represents a transaction that adds funds to the local wallet.
  | OutgoingTransaction
  -- ^ Represents a transaction that removes funds from the local wallet.
  deriving (Show, Ord, Eq, Enum, Bounded)

instance Arbitrary TransactionDirection where
  arbitrary = elements [minBound .. maxBound]

-- Drops the @Transaction@ suffix.
deriveJSON defaultOptions { A.constructorTagModifier = reverse . drop 11 . reverse . map C.toLower
                          } ''TransactionDirection

instance ToSchema TransactionDirection where
    declareNamedSchema _ =
        pure $ NamedSchema (Just "TransactionDirection") $ mempty
            & type_ .~ SwaggerString
            & enum_ ?~ ["outgoing", "incoming"]

-- | This is an information-less variant of 'PtxCondition'.
data TransactionStatus
    = Applying
    | InNewestBlocks
    | Persisted
    | WontApply
    | Creating
    deriving (Eq, Show, Ord)

allTransactionStatuses :: [TransactionStatus]
allTransactionStatuses =
    [Applying, InNewestBlocks, Persisted, WontApply, Creating]

transactionStatusToText :: TransactionStatus -> Text
transactionStatusToText x = case x of
    Applying {} ->
        "applying"
    InNewestBlocks {} ->
        "inNewestBlocks"
    Persisted {} ->
        "persisted"
    WontApply {} ->
        "wontApply"
    Creating {} ->
        "creating"

instance ToJSON TransactionStatus where
    toJSON x = object
        [ "tag" .= transactionStatusToText x
        , "data" .= Object mempty
        ]

instance ToSchema TransactionStatus where
    declareNamedSchema _ =
        pure $ NamedSchema (Just "TransactionStatus") $ mempty
            & type_ .~ SwaggerObject
            & required .~ ["tag", "data"]
            & properties .~ (mempty
                & at "tag" ?~ Inline (mempty
                    & type_ .~ SwaggerString
                    & enum_ ?~
                        map (String . transactionStatusToText)
                            allTransactionStatuses
                )
                & at "data" ?~ Inline (mempty
                    & type_ .~ SwaggerObject
                )
            )

instance FromJSON TransactionStatus where
    parseJSON = withObject "TransactionStatus" $ \o -> do
       tag <- o .: "tag"
       case tag of
           "applying" ->
                pure Applying
           "inNewestBlocks" ->
                pure InNewestBlocks
           "persisted" ->
                pure Persisted
           "wontApply" ->
                pure WontApply
           "creating" ->
                pure Creating
           _ ->
                fail $ "Couldn't parse out of " ++ toString (tag :: Text)

instance Arbitrary TransactionStatus where
    arbitrary = elements allTransactionStatuses

deriveSafeBuildable ''TransactionDirection
instance BuildableSafeGen TransactionDirection where
    buildSafeGen _ IncomingTransaction = "direction: incoming"
    buildSafeGen _ OutgoingTransaction = "direction: outgoing"

-- | A 'Wallet''s 'Transaction'.
data Transaction = Transaction
  { txId            :: !WalletTxId
  , txConfirmations :: !Word
  , txAmount        :: !WalletCoin
  , txInputs        :: !(NonEmpty PaymentDistribution)
  , txOutputs       :: !(NonEmpty PaymentDistribution)
    -- ^ The output money distribution.
  , txType          :: !TransactionType
    -- ^ The type for this transaction (e.g local, foreign, etc).
  , txDirection     :: !TransactionDirection
    -- ^ The direction for this transaction (e.g incoming, outgoing).
  , txCreationTime  :: !WalletTimestamp
    -- ^ The time when transaction was created.
  , txStatus        :: !TransactionStatus
  } deriving (Show, Ord, Eq, Generic)

deriveJSON Aeson.defaultOptions ''Transaction

instance ToSchema Transaction where
  declareNamedSchema =
    genericSchemaDroppingPrefix "tx" (\(--^) props -> props
      & ("id"            --^ "Transaction's id.")
      & ("confirmations" --^ "Number of confirmations.")
      & ("amount"        --^ "Coins moved as part of the transaction, in Lovelace.")
      & ("inputs"        --^ "One or more input money distributions.")
      & ("outputs"       --^ "One or more ouputs money distributions.")
      & ("type"          --^ "Whether the transaction is entirely local or foreign.")
      & ("direction"     --^ "Direction for this transaction.")
      & ("creationTime"  --^ "Timestamp indicating when the transaction was created.")
      & ("status"        --^ "Shows whether or not the transaction is accepted.")
    )

instance Arbitrary Transaction where
  arbitrary = Transaction <$> arbitrary
                          <*> arbitrary
                          <*> arbitrary
                          <*> arbitrary
                          <*> arbitrary
                          <*> arbitrary
                          <*> arbitrary
                          <*> arbitrary
                          <*> arbitrary

deriveSafeBuildable ''Transaction
instance BuildableSafeGen Transaction where
    buildSafeGen _ Transaction{..} = bprint
        ( "Transaction"
        % "\n  " % build
        % "\n  confirmations: " % build
        % "\n  " % build
        % "\n  " % build
        % "\n  amount: " % build
        % "\n  inputs:\n" % buildIndent 2 (buildList build)
        % "\n  outputs:\n" % buildIndent 2 (buildList build)
        )
        txId
        txConfirmations
        txType
        txDirection
        txAmount
        (toList txInputs)
        (toList txOutputs)

instance Buildable [Transaction] where
    build = bprint (buildList build)

-- | Technically we have serialized 'Tx' here, from the core.
mkTransactionAsBase16 :: Txp.Tx -> TransactionAsBase16
mkTransactionAsBase16 =
    TransactionAsBase16Unsafe . Base16.encode . Bi.serialize'

rawTransactionAsBase16 :: TransactionAsBase16 -> Text
rawTransactionAsBase16 (TransactionAsBase16Unsafe txtTx) = txtTx

instance Buildable [AddressLevel] where
    build = bprint (buildList build)

-- | Source address and corresponding derivation path, for external wallet.
data AddressAndPath = AddressAndPath
    { aapSrcAddress     :: !WalAddress -- AsBase58
    -- ^ Source address in Base58-format.
    , aapDerivationPath :: ![AddressLevel]
    -- ^ Derivation path used during generation of this address.
    } deriving (Show, Ord, Eq, Generic)

deriveJSON Aeson.defaultOptions ''AddressAndPath

instance ToSchema AddressAndPath where
    declareNamedSchema =
        genericSchemaDroppingPrefix "aap" (\(--^) props -> props
            & ("srcAddress"     --^ "Source address that corresponds to transaction input.")
            & ("derivationPath" --^ "Derivation path corresponding to this address.")
        )

instance Arbitrary AddressAndPath where
    arbitrary = AddressAndPath <$> arbitrary
                               <*> arbitrary

deriveSafeBuildable ''AddressAndPath
instance BuildableSafeGen AddressAndPath where
    buildSafeGen _ AddressAndPath{..} = bprint
        ("source " % build % "\nderivation path: " % build)
        aapSrcAddress
        aapDerivationPath

instance Buildable [AddressAndPath] where
    build = bprint (buildList build)

-- | A 'Wallet''s 'UnsignedTransaction'.
data UnsignedTransaction = UnsignedTransaction
    { rtxHex          :: !TransactionAsBase16
    -- ^ Serialized transaction in Base16-format.
    , rtxSrcAddresses :: ![AddressAndPath]
    -- ^ Addresses (with derivation paths) which will be used as a sources of money.
    } deriving (Show, Ord, Eq, Generic)

deriveJSON Aeson.defaultOptions ''UnsignedTransaction

instance ToSchema UnsignedTransaction where
    declareNamedSchema =
        genericSchemaDroppingPrefix "rtx" (\(--^) props -> props
            & ("hex"          --^ "New raw transaction in Base16-format.")
            & ("srcAddresses" --^ "Source addresses (with derivation paths), correspond to transaction inputs.")
        )

instance Arbitrary UnsignedTransaction where
    arbitrary = UnsignedTransaction <$> arbitrary
                                    <*> arbitrary

deriveSafeBuildable ''UnsignedTransaction
instance BuildableSafeGen UnsignedTransaction where
    buildSafeGen _ UnsignedTransaction{..} = bprint
        ( "Unsigned tx"
        % "\n  " % build
        % "\n  sources:\n" % buildIndent 2 (buildList build)
        )
        rtxHex
        rtxSrcAddresses

-- | To proof that the external wallet has the right to spend the input,
-- it returns the source address, the signature and the derived PK of
-- the transaction input.
data AddressWithProof = AddressWithProof
    { awpSrcAddress  :: !WalAddress
    -- ^ Source address.
    , awpTxSignature :: !TransactionSignatureAsBase16
    -- ^ Base16-encoded signature of transaction (made by derived SK).
    , awpDerivedPK   :: !PublicKey
    -- ^ Base58-encoded derived PK (corresponding to derived SK).
    } deriving (Show, Ord, Eq, Generic)

deriveJSON Aeson.defaultOptions ''AddressWithProof

instance ToSchema AddressWithProof where
    declareNamedSchema =
        genericSchemaDroppingPrefix "awp" (\(--^) props -> props
            & ("srcAddress"  --^ "Source address in Base58-format.")
            & ("txSignature" --^ "Transaction signature by derived SK.")
            & ("derivedPK"   --^ "Derived PK in Base58-format.")
        )

instance Arbitrary AddressWithProof where
    arbitrary = AddressWithProof <$> arbitrary
                                 <*> arbitrary
                                 <*> arbitrary

deriveSafeBuildable ''AddressWithProof
instance BuildableSafeGen AddressWithProof where
    buildSafeGen _ AddressWithProof{..} = bprint
        ( "address with proof"
        % "\n  source " % build
        % "\n  " % build
        % "\n  derived public key: "%build
        )
        awpSrcAddress
        awpTxSignature
        awpDerivedPK

instance Buildable [AddressWithProof] where
    build = bprint (buildList build)

-- | A 'Wallet''s 'SignedTransaction'. It is assumed
-- that this transaction was signed on the client-side
-- (mobile client or hardware wallet).
data SignedTransaction = SignedTransaction
    { stxTransaction     :: !TransactionAsBase16
    -- ^ Serialized transaction in base16-format.
    , stxAddrsWithProofs :: ![AddressWithProof]
    -- ^ Addresses with proofs for inputs of this transaction.
    } deriving (Show, Ord, Eq, Generic)

deriveJSON Aeson.defaultOptions ''SignedTransaction
instance ToSchema SignedTransaction where
    declareNamedSchema =
        genericSchemaDroppingPrefix "stx" (\(--^) props -> props
            & ("transaction"     --^ "New transaction that wasn't submitted in the blockchain yet.")
            & ("addrsWithProofs" --^ "Source addresses with proofs for inputs.")
        )

instance Arbitrary SignedTransaction where
    arbitrary = SignedTransaction <$> arbitrary
                                  <*> arbitrary

deriveSafeBuildable ''SignedTransaction
instance BuildableSafeGen SignedTransaction where
    buildSafeGen _ SignedTransaction{..} = bprint
        ( "Signed tx"
        % "\n  " % build
        % "\n proofs:\n" % buildIndent 2 (buildList build)
        )
        stxTransaction
        stxAddrsWithProofs

newtype WalletSoftwareVersion = WalletSoftwareVersion SoftwareVersion
    deriving (Eq, Generic, Show)

deriveSafeBuildable ''WalletSoftwareVersion
instance BuildableSafeGen WalletSoftwareVersion where
    buildSafeGen _ (WalletSoftwareVersion v) =
        bprint ("Wallet software version: " % build) v

-- | NOTE: There are 'ToJSON' and 'FromJSON' instances for 'V1 SoftwareVersion' type,
-- because we had it in internal API. Unfortunately, these instances are defined in
-- 'cardano-sl-chain' package and we cannot touch it for now.
-- So we have to define here the same instances for 'WalletSoftwareVersion' type,
-- it is required for golden JSON-test "V1 SoftwareVersion <-> WalletSoftwareVersion".
instance ToJSON WalletSoftwareVersion where
    toJSON (WalletSoftwareVersion (SoftwareVersion (ApplicationName appName) appVerNumber)) =
        object [ "applicationName" .= toJSON appName
               , "version" .= toJSON appVerNumber
               ]

instance FromJSON WalletSoftwareVersion where
    parseJSON = withObject "WalletSoftwareVersion" $ \o -> do
        appName <- o .: "applicationName"
        appVerNumber <- o .: "version"
        pure $ WalletSoftwareVersion $ SoftwareVersion (ApplicationName appName) appVerNumber

instance Arbitrary WalletSoftwareVersion where
    arbitrary = WalletSoftwareVersion <$> arbitrary

-- | NOTE: There is 'ToSchema' instance for 'V1 SoftwareVersion' type, but it's defined
-- in 'cardano-sl-chain' package and we cannot touch it for now. So we have to define
-- here some 'ToSchema'-instance for 'WalletSoftwareVersion'.
instance ToSchema WalletSoftwareVersion where
    declareNamedSchema _ =
        pure $ NamedSchema (Just "WalletSoftwareVersion") $ mempty
            & type_ .~ SwaggerObject

-- | A type representing an upcoming wallet update.
data WalletSoftwareUpdate = WalletSoftwareUpdate
  { updSoftwareVersion   :: !Text
  , updBlockchainVersion :: !Text
  , updScriptVersion     :: !Int
  -- Other types omitted for now.
  } deriving (Show, Eq, Generic)

deriveJSON Aeson.defaultOptions ''WalletSoftwareUpdate

instance ToSchema WalletSoftwareUpdate where
  declareNamedSchema =
    genericSchemaDroppingPrefix "upd" (\(--^) props -> props
      & ("softwareVersion"   --^ "Current software (wallet) version.")
      & ("blockchainVersion" --^ "Version of the underlying blockchain.")
      & ("scriptVersion"     --^ "Update script version.")
    )

instance Arbitrary WalletSoftwareUpdate where
  arbitrary = WalletSoftwareUpdate <$> arbitrary
                                   <*> arbitrary
                                   <*> fmap getPositive arbitrary

deriveSafeBuildable ''WalletSoftwareUpdate
instance BuildableSafeGen WalletSoftwareUpdate where
    buildSafeGen _ WalletSoftwareUpdate{..} = bprint
        ( "Wallet software update"
        % "\n  software version: " % build
        % "\n  blockchain version: " % build
        % "\n  script version: " % build
        )
        updSoftwareVersion
        updBlockchainVersion
        updScriptVersion

-- | A type encapsulating enough information to import a wallet from a
-- backup file.
data WalletImport = WalletImport
  { wiSpendingPassword :: !(Maybe SpendingPassword)
  , wiFilePath         :: !FilePath
  } deriving (Show, Eq, Generic)

deriveJSON Aeson.defaultOptions ''WalletImport

instance ToSchema WalletImport where
  declareNamedSchema =
    genericSchemaDroppingPrefix "wi" (\(--^) props -> props
      & ("spendingPassword"   --^ "An optional spending password to set for the imported wallet.")
      & ("filePath" --^ "The path to the .key file holding the backup.")
    )

instance Arbitrary WalletImport where
  arbitrary = WalletImport <$> arbitrary
                           <*> arbitrary

deriveSafeBuildable ''WalletImport
instance BuildableSafeGen WalletImport where
    buildSafeGen _ WalletImport{..} = bprint
        ("Import wallet from file: " % build)
        wiFilePath

-- | A redemption mnemonic.
newtype RedemptionMnemonic = RedemptionMnemonic
    { unRedemptionMnemonic :: Mnemonic 9
    }
    deriving stock (Eq, Show, Generic)
    deriving newtype (ToJSON, FromJSON, Arbitrary)

instance ToSchema RedemptionMnemonic where
    declareNamedSchema _ = pure $
        NamedSchema (Just "RedemptionMnemonic") (toSchema (Proxy @(Mnemonic 9)))

-- | A shielded redemption code.
newtype ShieldedRedemptionCode = ShieldedRedemptionCode
    { unShieldedRedemptionCode :: Text
    } deriving (Eq, Show, Generic)
      deriving newtype (ToJSON, FromJSON)

-- | This instance could probably be improved. A 'ShieldedRedemptionCode' is
-- a hash of the redemption key.
instance Arbitrary ShieldedRedemptionCode where
    arbitrary = ShieldedRedemptionCode <$> arbitrary

instance ToSchema ShieldedRedemptionCode where
    declareNamedSchema _ =
        pure
            $ NamedSchema (Just "ShieldedRedemptionCode") $ mempty
            & type_ .~ SwaggerString

-- | The request body for redeeming some Ada.
data Redemption = Redemption
    { redemptionRedemptionCode   :: ShieldedRedemptionCode
    -- ^ The redemption code associated with the Ada to redeem.
    , redemptionMnemonic         :: Maybe RedemptionMnemonic
    -- ^ An optional mnemonic. This mnemonic was included with paper
    -- certificates, and the presence of this field indicates that we're
    -- doing a paper vend.
    , redemptionSpendingPassword :: SpendingPassword
    -- ^ The user must provide a spending password that matches the wallet that
    -- will be receiving the redemption funds.
    , redemptionWalletId         :: WalletId
    -- ^ Redeem to this wallet
    , redemptionAccountIndex     :: AccountIndex
    -- ^ Redeem to this account index in the wallet
    } deriving (Eq, Show, Generic)

deriveSafeBuildable ''Redemption
instance BuildableSafeGen Redemption where
    buildSafeGen _ r = bprint
        ( "Redemption"
        % "\n  " % build
        % "\n  " % build
        )
        (redemptionWalletId r)
        (redemptionAccountIndex r)

deriveJSON Aeson.defaultOptions ''Redemption

instance ToSchema Redemption where
    declareNamedSchema =
        genericSchemaDroppingPrefix "redemption" (\(--^) props -> props
            & "redemptionCode"
            --^ "The redemption code associated with the Ada to redeem."
            & "mnemonic"
            --^ ( "An optional mnemonic. This must be provided for a paper "
                <> "certificate redemption."
                )
            & "spendingPassword"
            --^ ( "An optional spending password. This must match the password "
                <> "for the provided wallet ID and account index."
                )
        )

instance Arbitrary Redemption where
    arbitrary = Redemption <$> arbitrary
                           <*> arbitrary
                           <*> arbitrary
                           <*> arbitrary
                           <*> arbitrary

--
-- POST/PUT requests isomorphisms
--

type family Update (original :: *) :: * where
    Update Wallet =
        WalletUpdate
    Update Account =
        AccountUpdate
    Update WalletAddress =
        () -- read-only

type family New (original :: *) :: * where
    New Wallet =
        NewWallet
    New Account =
        NewAccount
    New WalletAddress =
        NewAddress
    New EosWallet =
        NewEosWallet

type CaptureWalletId = Capture "walletId" WalletId

type CaptureAccountId = Capture "accountId" AccountIndex

--
-- Example typeclass instances
--

instance Example Core.Address
instance Example AccountIndex
instance Example AccountBalance
instance Example AccountAddresses
instance Example AddressLevel
instance Example AddressPoolGap
instance Example WalletId
instance Example AssuranceLevel
instance Example LocalTimeDifference
instance Example PaymentDistribution
instance Example AccountUpdate
instance Example Wallet
instance Example EosWallet
instance Example UpdateEosWallet
instance Example WalletUpdate
instance Example WalletOperation
instance Example PasswordUpdate
instance Example EstimatedFees
instance Example Transaction
instance Example WalletSoftwareUpdate
instance Example WalletAddress
instance Example NewAccount
instance Example AddressValidity
instance Example NewAddress
instance Example ShieldedRedemptionCode
instance Example WalletPassPhrase
instance Example WalletCoin
instance Example PublicKey

-- | We have a specific 'Example' instance for @'V1' 'Address'@ because we want
-- to control the length of the examples. It is possible for the encoded length
-- to become huge, up to 1000+ bytes, if the 'UnsafeMultiKeyDistr' constructor
-- is used. We do not use this constructor, which keeps the address between
-- ~80-150 bytes long.
instance Example WalAddress where
    example = fmap WalAddress . Core.makeAddress
        <$> arbitrary
        <*> arbitraryAttributes
      where
        arbitraryAttributes =
            Core.AddrAttributes
                <$> arbitrary
                <*> oneof
                    [ pure Core.BootstrapEraDistr
                    , Core.SingleKeyDistr <$> arbitrary
                    ]
                <*> arbitrary

instance Example BackupPhrase where
    example = pure (BackupPhrase def)

instance Example Core.InputSelectionPolicy where
    example = pure Core.OptimizeForHighThroughput

instance Example WalletInputSelectionPolicy where
    example = pure (WalletInputSelectionPolicy Core.OptimizeForHighThroughput)

instance Example Account where
    example = Account <$> example
                      <*> example -- NOTE: this will produce non empty list
                      <*> example
                      <*> pure "My account"
                      <*> example

instance Example NewWallet where
    example = NewWallet <$> example
                        <*> example -- Note: will produce `Just a`
                        <*> example
                        <*> pure "My Wallet"
                        <*> example

instance Example TransactionAsBase16 where
    example = TransactionAsBase16Unsafe <$> pure
        "839f8200d8185826825820de3151a2d9cd8e2bbe292a6153d679d123892ddcfbee869c4732a5c504a7554d19386cff9f8282d818582183581caeb153a5809a084507854c9f3e5795bcca89781f9c386d957748cd42a0001a87236a1f1b00780aa6c7d62110ffa0"

instance Example TransactionSignatureAsBase16 where
    example = TransactionSignatureAsBase16Unsafe <$> pure
        "5840709cc240ac9ad78cbf47c3eec76df917423943e34339277593e8e2b8c9f9f2e59583023bfbd8e26c40dff6a7fa424600f9b942819533d8afee37a5ac6d813207"

instance Example AccountPublicKeyWithIx where
    example = AccountPublicKeyWithIx <$> example
                                     <*> example

instance Example NewEosWallet where
    example = NewEosWallet <$> example
                           <*> example
                           <*> example
                           <*> pure "My EOS-Wallet"

instance Example PaymentSource where
    example = PaymentSource <$> example
                            <*> example

instance Example Payment where
    example = Payment <$> example
                      <*> example
                      <*> example -- TODO: will produce `Just groupingPolicy`
                      <*> example

instance Example Redemption where
    example = Redemption <$> example
                         <*> pure Nothing
                         <*> example
                         <*> example
                         <*> example

instance Example AddressAndPath where
    example = AddressAndPath <$> example
                             <*> example

instance Example UnsignedTransaction where
    example = UnsignedTransaction <$> example
                                  <*> example

instance Example AddressWithProof where
    example = AddressWithProof <$> example
                               <*> example
                               <*> example

instance Example SignedTransaction where
    example = SignedTransaction <$> example
                                <*> example

instance Example WalletImport where
    example = WalletImport <$> example
                           <*> pure "/Users/foo/Documents/wallet_to_import.key"

--
-- Wallet Errors
--

-- | Details about what 'NotEnoughMoney' means
data ErrNotEnoughMoney
    -- | UTxO exhausted whilst trying to pick inputs to cover remaining fee
    = ErrCannotCoverFee

    -- | UTxO exhausted during input selection
    --
    -- We record the available balance of the UTxO
    | ErrAvailableBalanceIsInsufficient Int

    deriving (Eq, Show, Generic)

instance Buildable ErrNotEnoughMoney where
    build = \case
        ErrCannotCoverFee ->
             bprint "Not enough coins to cover fee."
        ErrAvailableBalanceIsInsufficient _ ->
             bprint "Not enough available coins to proceed."

instance ToJSON ErrNotEnoughMoney where
    toJSON = \case
        e@ErrCannotCoverFee -> object
            [ "msg" .= sformat build e
            ]
        e@(ErrAvailableBalanceIsInsufficient balance) -> object
            [ "msg"              .= sformat build e
            , "availableBalance" .= balance
            ]

instance FromJSON ErrNotEnoughMoney where
    parseJSON v =
            withObject "AvailableBalanceIsInsufficient" availableBalanceIsInsufficientParser v
        <|> withObject "CannotCoverFee" cannotCoverFeeParser v
      where
        cannotCoverFeeParser :: Object -> Parser ErrNotEnoughMoney
        cannotCoverFeeParser o = do
            msg <- o .: "msg"
            when (msg /= sformat build ErrCannotCoverFee) mempty
            pure ErrCannotCoverFee

        availableBalanceIsInsufficientParser :: Object -> Parser ErrNotEnoughMoney
        availableBalanceIsInsufficientParser o = do
            msg <- o .: "msg"
            when (msg /= sformat build (ErrAvailableBalanceIsInsufficient 0)) mempty
            ErrAvailableBalanceIsInsufficient <$> (o .: "availableBalance")


data ErrUtxoNotEnoughFragmented = ErrUtxoNotEnoughFragmented {
      theMissingUtxos :: !Int
    , theHelp         :: !Text
    } deriving (Eq, Generic, Show)


msgUtxoNotEnoughFragmented :: Text
msgUtxoNotEnoughFragmented = "Utxo is not enough fragmented to handle the number of outputs of this transaction. Query /api/v1/wallets/{walletId}/statistics/utxos endpoint for more information"

deriveJSON Aeson.defaultOptions ''ErrUtxoNotEnoughFragmented

instance Buildable ErrUtxoNotEnoughFragmented where
    build (ErrUtxoNotEnoughFragmented missingUtxos _ ) =
        bprint ("Missing "%build%" utxo(s) to accommodate all outputs of the transaction") missingUtxos


data ErrZeroAmountCoin = ErrZeroAmountCoin {
      theZeroOutputs :: !Int
    , theHelp        :: !Text
    } deriving (Eq, Generic, Show)


msgZeroAmountCoin :: Text
msgZeroAmountCoin = "Each payee must receive positive amount in the transaction - zero amount is not allowed"

deriveJSON Aeson.defaultOptions ''ErrZeroAmountCoin

instance Buildable ErrZeroAmountCoin where
    build (ErrZeroAmountCoin zeroOutputs _ ) =
        bprint ("There are "%build%" zero output(s) in the transaction") zeroOutputs



-- | Type representing any error which might be thrown by wallet.
--
-- Errors are represented in JSON in the JSend format (<https://labs.omniti.com/labs/jsend>):
-- ```
-- {
--     "status": "error"
--     "message" : <constr_name>,
--     "diagnostic" : <data>
-- }
-- ```
-- where `<constr_name>` is a string containing name of error's constructor (e. g. `NotEnoughMoney`),
-- and `<data>` is an object containing additional error data.
-- Additional data contains constructor fields, field names are record field names without
-- a `we` prefix, e. g. for `OutputIsRedeem` error "diagnostic" field will be the following:
-- ```
-- {
--     "address" : <address>
-- }
-- ```
--
-- Additional data in constructor should be represented as record fields.
-- Otherwise TemplateHaskell will raise an error.
--
-- If constructor does not have additional data (like in case of `WalletNotFound` error),
-- then "diagnostic" field will be empty object.
--
-- TODO: change fields' types to actual Cardano core types, like `Coin` and `Address`
data WalletError =
    -- | NotEnoughMoney weNeedMore
      NotEnoughMoney !ErrNotEnoughMoney
    -- | OutputIsRedeem weAddress
    | OutputIsRedeem !WalAddress
    -- | UnknownError weMsg
    | UnknownError !Text
    -- | InvalidAddressFormat weMsg
    | InvalidAddressFormat !Text
    | WalletNotFound
    | WalletAlreadyExists !WalletId
    | AddressNotFound
    | TxFailedToStabilize
    | InvalidPublicKey !Text
    | UnsignedTxCreationError
    | TooBigTransaction
    -- ^ Size of transaction (in bytes) is greater than maximum.
    | SignedTxSubmitError !Text
    | TxRedemptionDepleted
    -- | TxSafeSignerNotFound weAddress
    | TxSafeSignerNotFound !WalAddress
    -- | MissingRequiredParams requiredParams
    | MissingRequiredParams !(NonEmpty (Text, Text))
    -- | WalletIsNotReadyToProcessPayments weStillRestoring
    | CannotCreateAddress !Text
    -- ^ Cannot create derivation path for new address (for external wallet).
    | WalletIsNotReadyToProcessPayments !SyncProgress
    -- ^ The @Wallet@ where a @Payment@ is being originated is not fully
    -- synced (its 'WalletSyncState' indicates it's either syncing or
    -- restoring) and thus cannot accept new @Payment@ requests.
    -- | NodeIsStillSyncing wenssStillSyncing
    | NodeIsStillSyncing !SyncPercentage
    -- ^ The backend couldn't process the incoming request as the underlying
    -- node is still syncing with the blockchain.
    | RequestThrottled !Word64
    -- ^ The request has been throttled. The 'Word64' is a count of microseconds
    -- until the user should retry.
    | UtxoNotEnoughFragmented !ErrUtxoNotEnoughFragmented
    -- ^ available Utxo is not enough fragmented, ie., there is more outputs of transaction than
    -- utxos
    | ZeroAmountCoin !ErrZeroAmountCoin
    -- ^ there is at least one zero amount output in the transaction
    | EosWalletDoesNotHaveAccounts Text
    -- ^ EOS-wallet doesn't have any accounts.
    | EosWalletHasWrongAccounts Text
    -- ^ Some of accounts associated with EOS-wallets have FO-branch.
    | EosWalletGapsDiffer Text
    -- ^ Accounts associated with EOS-wallet contain different values of address pool gap.
    deriving (Generic, Show, Eq)

deriveGeneric ''WalletError

instance Exception WalletError

instance ToHttpErrorStatus WalletError

instance ToJSON WalletError where
    toJSON = jsendErrorGenericToJSON

instance FromJSON WalletError where
    parseJSON = jsendErrorGenericParseJSON

instance Arbitrary WalletError where
    arbitrary = Gen.oneof
        [ NotEnoughMoney <$> Gen.oneof
            [ pure ErrCannotCoverFee
            , ErrAvailableBalanceIsInsufficient <$> Gen.choose (1, 1000)
            ]
        , OutputIsRedeem . WalAddress <$> arbitrary
        , UnknownError <$> arbitraryText
        , InvalidAddressFormat <$> arbitraryText
        , pure WalletNotFound
        , WalletAlreadyExists <$> arbitrary
        , pure AddressNotFound
        , InvalidPublicKey <$> arbitraryText
        , pure UnsignedTxCreationError
        , SignedTxSubmitError <$> arbitraryText
        , pure TooBigTransaction
        , pure TxFailedToStabilize
        , pure TxRedemptionDepleted
        , TxSafeSignerNotFound . WalAddress <$> arbitrary
        , MissingRequiredParams <$> Gen.oneof
            [ unsafeMkNonEmpty <$> Gen.vectorOf 1 arbitraryParam
            , unsafeMkNonEmpty <$> Gen.vectorOf 2 arbitraryParam
            , unsafeMkNonEmpty <$> Gen.vectorOf 3 arbitraryParam
            ]
        , WalletIsNotReadyToProcessPayments <$> arbitrary
        , NodeIsStillSyncing <$> arbitrary
        , CannotCreateAddress <$> arbitraryText
        , RequestThrottled <$> arbitrary
        , UtxoNotEnoughFragmented <$> Gen.oneof
          [ ErrUtxoNotEnoughFragmented <$> Gen.choose (1, 10) <*> arbitrary
          ]
        , ZeroAmountCoin <$> Gen.oneof
          [ ErrZeroAmountCoin <$> Gen.choose (1, 10) <*> arbitrary
          ]
        ]
      where
        arbitraryText :: Gen Text
        arbitraryText =
            toText . Gen.getASCIIString <$> arbitrary

        arbitraryParam :: Gen (Text, Text)
        arbitraryParam =
            (,) <$> arbitrary <*> arbitrary

        unsafeMkNonEmpty :: [a] -> NonEmpty a
        unsafeMkNonEmpty (h:q) = h :| q
        unsafeMkNonEmpty _     = error "unsafeMkNonEmpty called with empty list"


-- | Give a short description of an error
instance Buildable WalletError where
    build = \case
        NotEnoughMoney x ->
             bprint build x
        OutputIsRedeem _ ->
             bprint "One of the TX outputs is a redemption address."
        UnknownError _ ->
             bprint "Unexpected internal error."
        InvalidAddressFormat _ ->
             bprint "Provided address format is not valid."
        WalletNotFound ->
             bprint "Reference to an unexisting wallet was given."
        WalletAlreadyExists _ ->
             bprint "Can't create or restore a wallet. The wallet already exists."
        AddressNotFound ->
             bprint "Reference to an unexisting address was given."
        InvalidPublicKey _ ->
            bprint "Extended public key (for external wallet) is invalid."
        UnsignedTxCreationError ->
            bprint "Unable to create unsigned transaction for an external wallet."
        TooBigTransaction ->
            bprint "Transaction size is greater than 4096 bytes."
        SignedTxSubmitError _ ->
            bprint "Unable to submit externally-signed transaction."
        MissingRequiredParams _ ->
            bprint "Missing required parameters in the request payload."
        WalletIsNotReadyToProcessPayments _ ->
            bprint "This wallet is restoring, and it cannot send new transactions until restoration completes."
        NodeIsStillSyncing _ ->
            bprint "The node is still syncing with the blockchain, and cannot process the request yet."
        TxRedemptionDepleted ->
            bprint "The redemption address was already used."
        TxSafeSignerNotFound _ ->
            bprint "The safe signer at the specified address was not found."
        TxFailedToStabilize ->
            bprint "We were unable to find a set of inputs to satisfy this transaction."
        CannotCreateAddress _ ->
            bprint "Cannot create derivation path for new address, for external wallet."
        RequestThrottled _ ->
            bprint "You've made too many requests too soon, and this one was throttled."
        UtxoNotEnoughFragmented x ->
            bprint build x
        ZeroAmountCoin x ->
            bprint build x
        EosWalletDoesNotHaveAccounts _ ->
            bprint "EOS-wallet doesn't have any accounts."
        EosWalletHasWrongAccounts _ ->
            bprint "Some of accounts associated with EOS-wallets have FO-branch."
        EosWalletGapsDiffer _ ->
            bprint "Accounts associated with EOS-wallet contain different values of address pool gap."

-- | Convert wallet errors to Servant errors
instance ToServantError WalletError where
    declareServantError = \case
        NotEnoughMoney{} ->
            err403
        OutputIsRedeem{} ->
            err403
        UnknownError{} ->
            err500
        WalletNotFound{} ->
            err404
        WalletAlreadyExists{} ->
            err403
        InvalidAddressFormat{} ->
            err401
        AddressNotFound{} ->
            err404
        InvalidPublicKey{} ->
            err400
        UnsignedTxCreationError{} ->
            err500
        TooBigTransaction{} ->
            err400
        SignedTxSubmitError{} ->
            err500
        MissingRequiredParams{} ->
            err400
        WalletIsNotReadyToProcessPayments{} ->
            err403
        NodeIsStillSyncing{} ->
            err412 -- Precondition failed
        TxFailedToStabilize{} ->
            err500
        TxRedemptionDepleted{} ->
            err400
        TxSafeSignerNotFound{} ->
            err400
        CannotCreateAddress{} ->
            err500
        RequestThrottled{} ->
            err400 { errHTTPCode = 429 }
        UtxoNotEnoughFragmented{} ->
            err403
        ZeroAmountCoin{} ->
            err400
        EosWalletDoesNotHaveAccounts{} ->
            err500
        EosWalletHasWrongAccounts{} ->
            err500
        EosWalletGapsDiffer{} ->
            err500

-- | Declare the key used to wrap the diagnostic payload, if any
instance HasDiagnostic WalletError where
    getDiagnosticKey = \case
        NotEnoughMoney{} ->
            "details"
        OutputIsRedeem{} ->
            "address"
        UnknownError{} ->
            "msg"
        WalletNotFound{} ->
            noDiagnosticKey
        WalletAlreadyExists{} ->
            "walletId"
        InvalidAddressFormat{} ->
            "msg"
        AddressNotFound{} ->
            noDiagnosticKey
        InvalidPublicKey{} ->
            "msg"
        UnsignedTxCreationError{} ->
            noDiagnosticKey
        TooBigTransaction{} ->
            noDiagnosticKey
        SignedTxSubmitError{} ->
            "msg"
        MissingRequiredParams{} ->
            "params"
        WalletIsNotReadyToProcessPayments{} ->
            "stillRestoring"
        NodeIsStillSyncing{} ->
            "stillSyncing"
        TxFailedToStabilize{} ->
            noDiagnosticKey
        TxRedemptionDepleted{} ->
            noDiagnosticKey
        TxSafeSignerNotFound{} ->
            "address"
        CannotCreateAddress{} ->
            "msg"
        RequestThrottled{} ->
            "microsecondsUntilRetry"
        UtxoNotEnoughFragmented{} ->
            "details"
        ZeroAmountCoin{} ->
            "details"
        EosWalletDoesNotHaveAccounts{} ->
            noDiagnosticKey
        EosWalletHasWrongAccounts{} ->
            noDiagnosticKey
        EosWalletGapsDiffer{} ->
            noDiagnosticKey
