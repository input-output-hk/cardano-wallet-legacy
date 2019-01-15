{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Cardano.Wallet.Kernel.DB.HdRootId (
      HdRootId
    , decodeHdRootId
    , mkHdRootIdForEOWallet
    , mkHdRootIdForFOWallet
    ) where

import           Universum

import           Data.ByteString.Base58 (bitcoinAlphabet, decodeBase58,
                     encodeBase58)
import           Data.SafeCopy (base, deriveSafeCopy)
import qualified Data.UUID as Uuid
import           Data.UUID.V4 (nextRandom)

import           Test.QuickCheck (Arbitrary (..))
import           Test.QuickCheck.Gen (oneof)

import           Formatting (bprint, build)
import qualified Formatting.Buildable

import qualified Pos.Binary.Class as Bi
import qualified Pos.Core as Core
import           Pos.Core.NetworkMagic (NetworkMagic (..))
import           Pos.Crypto (EncryptedSecretKey, encToPublic)

-- | Unified identifier for wallets (both FO and EO ones).
-- Technically it contains a text, but actually it contains one from two options:
--
-- 1. for FO-wallets - 'Core.Address' made from wallet's root public key, in Base58-format.
-- 2. for EO-wallets - 'UUID', in Base58-format.
--
-- Please note that using 'Core.Address' as wallet's identifier is a bad decision,
-- but since exchanges and users may have wallets' ids stored on their side we
-- have to keep backward-compatibility for existing ids.
newtype HdRootId = HdRootId { getHdRootId :: Text }
    deriving (Eq, Ord, Show, Generic)

instance NFData HdRootId

-- | TODO: It is not very arbitrary, maybe we should improve it.
instance Arbitrary HdRootId where
    arbitrary = oneof
        [ pure $ HdRootId "Ae2tdPwUPEZ18ZjTLnLVr9CEvUEUX4eW1LBHbxxxJgxdAYHrDeSCSbCxrvx"
        , pure $ HdRootId "J7rQqaLLHBFPrgJXwpktaMB1B1kQBXAyc2uRSfRPzNVGiv6TdxBzkPNBUWysZZZdhFG9gRy3sQFfX5wfpLbi4XTFGFxTg"
        , pure $ HdRootId "kKyjp8hUCYDK1UhU8Wg5sBu3Ud2TqFDCbTWCo9ySDGjaLLk73"
        , pure $ HdRootId "QxUZbMEK9Vg6V9UWQ1BRLwrBoQQuYt2RjnEW4vRmXBtsT9WKk"
        , pure $ HdRootId "maKVJgoL6c65NfUJDGeyQhcK6b9rynpEdwKe4T1wn3jfEZZRv"
        , pure $ HdRootId "SC6jKYB9gFnNhvZNuQW6e7MnWM59pwcyNp1ayp9n9iEiucYWr"
        , pure $ HdRootId "RqTmjdzemFRaZzu4TZH9qwS5NefKgTZ6fp2bELoX8hwJPSuBw"
        ]

instance Buildable HdRootId where
    build (HdRootId uniqueId) = bprint build uniqueId

deriveSafeCopy 1 'base ''HdRootId

-- | Decodes 'HdRootId' from the text.
decodeHdRootId :: Text -> Either Text HdRootId
decodeHdRootId rawId = decodeFromBS . encodeUtf8 $ rawId
  where
    decodeFromBS :: ByteString -> Either Text HdRootId
    decodeFromBS bs = do
        let base58Err = "Invalid Base58 representation of HdRootId"
        decodedBS <- maybeToRight base58Err $ decodeBase58 bitcoinAlphabet bs
        let probablyUUID = decodeUUID decodedBS
            probablyAddress = decodeAddress decodedBS
        if isRight probablyUUID
            then probablyUUID
            else probablyAddress

    decodeUUID bs = maybe
        (Left ("HdRootId is not a UUID" :: Text))
        (\_ -> Right $ HdRootId rawId)
        $ Uuid.fromASCIIBytes bs

    decodeAddress bs = either
        (\_ -> Left "Invalid HdRootId (it's not an Address nor UUID)")
        (\(_addr :: Core.Address) -> Right $ HdRootId rawId)
        $ Bi.decodeFull' bs

mkHdRootIdForEOWallet :: IO HdRootId
mkHdRootIdForEOWallet = HdRootId
    . decodeUtf8
    . encodeBase58 bitcoinAlphabet
    . Uuid.toASCIIBytes <$> nextRandom

mkHdRootIdForFOWallet
    :: NetworkMagic
    -> EncryptedSecretKey
    -> HdRootId
mkHdRootIdForFOWallet nm esk = HdRootId
    . decodeUtf8
    . Core.addrToBase58
    . (Core.makePubKeyAddressBoot nm)
    . encToPublic $ esk
