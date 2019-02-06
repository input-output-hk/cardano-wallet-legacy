{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Cardano.Wallet.Kernel.DB.HdRootId (
      HdRootId
    , decodeHdRootId
    , genHdRootId
    , eskToHdRootId
    ) where

import           Universum

import qualified Data.ByteString as BS
import           Data.ByteString.Base58 (bitcoinAlphabet, decodeBase58,
                     encodeBase58)
import           Data.SafeCopy (base, deriveSafeCopy)
import qualified Data.UUID as UUID
import qualified Data.UUID.V4 as UUID
import           Formatting (bprint, build)
import qualified Formatting.Buildable
import           Test.QuickCheck (Arbitrary (..))
import           Test.QuickCheck.Gen (chooseAny, oneof, vectorOf)

import qualified Pos.Binary.Class as Bi
import           Pos.Core (Address, addrToBase58, makePubKeyAddressBoot)
import           Pos.Core.NetworkMagic (NetworkMagic (..))
import           Pos.Crypto (EncryptedSecretKey, encToPublic,
                     safeDeterministicKeyGen)
import           Test.Pos.Core.Arbitrary ()


-- | Unified identifier for wallets (both FO and EO ones).
-- Technically it contains a text, but actually it contains one from two options:
--
-- 1. for FO-wallets - 'Address' made from wallet's root public key, in Base58-format.
-- 2. for EO-wallets - 'UUID', in Base58-format.
--
-- Using 'Address' for HdRootId was an arbitrary decision which turns out to
-- yield confusion and interrogations. There's no particular relationship
-- between an HdRootId and an actual Address. At that time, we needed an ID that
-- can be unique and determinically computed from a root encrypted secret key.
-- Addresses have that property, and therefore, were picked as root ids.
--
-- Since users of the API may have wallets' ids stored on their side, we can't
-- really change that for existing wallets in order to keep backward
-- compatibility.
newtype HdRootId = HdRootId { getHdRootId :: Text }
    deriving (Eq, Ord, Show, Generic)
deriveSafeCopy 1 'base ''HdRootId

instance NFData HdRootId

instance Arbitrary HdRootId where
    arbitrary = oneof [ arbitraryFO , arbitraryEO ]
      where
        arbitraryFO = do
            (_, esk) <- safeDeterministicKeyGen
                <$> (BS.pack <$> vectorOf 32 arbitrary)
                <*> pure mempty
            nm <- arbitrary
            pure (eskToHdRootId nm esk)

        arbitraryEO =
            HdRootId
            . decodeUtf8
            . encodeBase58 bitcoinAlphabet
            . UUID.toASCIIBytes
            <$> chooseAny

instance Buildable HdRootId where
    build (HdRootId uniqueId) = bprint build uniqueId


decodeHdRootId :: Text -> Maybe HdRootId
decodeHdRootId txt = do
    bs <- decodeBase58 bitcoinAlphabet (encodeUtf8 txt)
    decodeAddr bs <|> decodeUUID bs
  where
    decodeAddr :: ByteString -> Maybe HdRootId
    decodeAddr bs = do
        void $ either (\_ -> Nothing) Just $ Bi.decodeFull' @Address bs
        pure $ HdRootId txt
    decodeUUID :: ByteString -> Maybe HdRootId
    decodeUUID bs = do
        void $ UUID.fromASCIIBytes $ bs
        pure $ HdRootId txt

genHdRootId :: IO HdRootId
genHdRootId = HdRootId
    . decodeUtf8
    . encodeBase58 bitcoinAlphabet
    . UUID.toASCIIBytes
    <$> UUID.nextRandom

eskToHdRootId
    :: NetworkMagic
    -> EncryptedSecretKey
    -> HdRootId
eskToHdRootId nm esk = HdRootId
    . decodeUtf8
    . addrToBase58
    . (makePubKeyAddressBoot nm)
    . encToPublic $ esk
