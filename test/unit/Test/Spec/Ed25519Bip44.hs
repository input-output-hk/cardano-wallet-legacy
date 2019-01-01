{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE TypeApplications           #-}

module Test.Spec.Ed25519Bip44 (spec) where

import           Universum

import           Cardano.Crypto.Wallet (generate)
import           Pos.Crypto (EncryptedSecretKey, PassPhrase (..), PublicKey,
                     checkPassMatches, emptySalt, isHardened,
                     mkEncSecretWithSaltUnsafe)

import           Cardano.Wallet.Kernel.Ed25519Bip44 (ChangeChain,
                     deriveAddressPrivateKey, deriveAddressPublicKey,
                     derivePublicKey)

import qualified Data.ByteString as BS
import           Test.Hspec (Spec, describe, it)
import           Test.Pos.Core.Arbitrary ()
import           Test.QuickCheck (Arbitrary (..), InfiniteList (..), Property,
                     arbitraryBoundedIntegral, arbitrarySizedBoundedIntegral,
                     expectFailure, property, shrinkIntegral, (.&&.), (===),
                     (==>))

-- A wrapper type for hardened keys generator
newtype Hardened
    = Hardened Word32
    deriving (Show, Eq, Ord, Enum, Real, Integral, Num)

-- A wrapper type for non-hardened keys generator
newtype NonHardened
    = NonHardened Word32
    deriving (Show, Eq, Ord, Enum, Real, Integral, Num)

instance Bounded Hardened where
    minBound = Hardened 0x80000000 -- 2^31
    maxBound = Hardened $ maxBound @Word32

instance Bounded NonHardened where
    minBound = NonHardened $ minBound @Word32
    maxBound = NonHardened 0x7FFFFFFF -- 2^31 - 1

-- TODO (akegalj): seems like Large from quickcheck which is using
-- arbitrarySizedBoundedIntegral doesn't work correctly. That implementation
-- doesn't repect minBound and produces numbers which are bellow minBound!
instance Arbitrary Hardened where
    arbitrary = arbitraryBoundedIntegral
    shrink = filter (>= minBound) . shrinkIntegral

instance Arbitrary NonHardened where
    arbitrary = arbitrarySizedBoundedIntegral
    shrink = shrinkIntegral

-- | Deriving address public key should fail if address index
-- is hardened. We should be able to derive Address public key
-- only with non-hardened address index
--
-- FIXME: this property will fail because quickcheck will "Give up" on
-- trying to satisfy the precondition. By default quickcheck prefers smaller
-- values, so generator won't pick enough large values. This should be
-- fixed with custom generator (TODO)
--
-- quickcheck also defines Large wrapper but it fails as well :/
prop_cannotDeriveAddressPublicKeyForHardenedIx
    :: PublicKey
    -> ChangeChain
    -> Word32
    -> Property
prop_cannotDeriveAddressPublicKeyForHardenedIx accPubKey change addressIx =
    isHardened addressIx ==> isNothing addrPubKey
  where
    addrPubKey = deriveAddressPublicKey accPubKey change addressIx

-- | Deriving address public key should succeed if address index
-- is non-hardened.
prop_deriveAddressPublicKeyForNonHardenedIx
    :: PublicKey
    -> ChangeChain
    -> Word32
    -> Property
prop_deriveAddressPublicKeyForNonHardenedIx accPubKey change addressIx =
    not (isHardened addressIx) ==> isJust addrPubKey
  where
    addrPubKey = deriveAddressPublicKey accPubKey change addressIx

-- | Deriving address public key should be equal to deriving address
-- private key and extracting public key from it (works only for non-hardened child keys).
--
-- To compute the public child key of a parent private key:
--  * N(CKDpriv((kpar, cpar), i)) (works always).
--  * CKDpub(N(kpar, cpar), i) (works only for non-hardened child keys).
--
-- Thus:
--
-- N(CKDpriv((kpar, cpar), i)) === CKDpub(N(kpar, cpar), i)
--
-- if (kpar, cpar) is a non-hardened key.
--
-- For details see https://github.com/bitcoin/bips/blob/master/bip-0032.mediawiki#private-parent-key--public-child-key
prop_deriveAddressPublicFromAccountPrivateKey
    :: InfiniteList Word8
    -> PassPhrase
    -> ChangeChain
    -> Word32
    -> Property
prop_deriveAddressPublicFromAccountPrivateKey (InfiniteList seed _) passPhrase@(PassPhrase passBytes) changeChain addressIx =
    -- TODO (akegalj): check coverage with quickcheck @cover@
    -- FIXME (akegalj): instead of doing this create generator for non-hardened keys.
    -- This should in average discard 50% of examples and will thus be
    -- 50% slower.
    not (isHardened addressIx) ==> (isJust addrPubKey1 .&&. addrPubKey1 === addrPubKey2)
  where
    accEncPrvKey = mkEncSecretWithSaltUnsafe emptySalt passPhrase $ generate (BS.pack $ take 32 seed) passBytes
    -- N(CKDpriv((kpar, cpar), i))
    addrPubKey1 =
        derivePublicKey <$> deriveAddressPrivateKey
            passPhrase
            accEncPrvKey
            changeChain
            addressIx
    -- CKDpub(N(kpar, cpar), i)
    addrPubKey2 =
        deriveAddressPublicKey
            (derivePublicKey accEncPrvKey)
            changeChain
            addressIx

-- | Deriving address private key should always fail
-- if account index is hardened
prop_deriveAddressPrivateKeyHardened
    :: InfiniteList Word8
    -> PassPhrase
    -> ChangeChain
    -> Word32
    -> Property
prop_deriveAddressPrivateKeyHardened (InfiniteList seed _) passPhrase@(PassPhrase passBytes) changeChain addressIx =
    isHardened addressIx ==> isJust addrPrvKey
  where
    accEncPrvKey = mkEncSecretWithSaltUnsafe emptySalt passPhrase $ generate (BS.pack $ take 32 seed) passBytes
    addrPrvKey =
        deriveAddressPrivateKey
            passPhrase
            accEncPrvKey
            changeChain
            addressIx

-- | Deriving address private key should always fail
-- if password differs from account private key password
prop_deriveAddressPrivateKeyWrongPassword
    :: PassPhrase
    -> EncryptedSecretKey
    -> ChangeChain
    -> Word32
    -> Property
prop_deriveAddressPrivateKeyWrongPassword passPhrase accEncPrvKey changeChain addressIx =
    -- There is a really small possibility we will generate
    -- two equal passwords - so this precondition will rarely fail
    -- TODO (akegalj): check coverage with quickcheck @cover@
    isNothing (checkPassMatches passPhrase accEncPrvKey) ==> isJust addrPrvKey
  where
    addrPrvKey =
        deriveAddressPrivateKey
            passPhrase
            accEncPrvKey
            changeChain
            addressIx

spec :: Spec
spec = describe "Ed25519Bip44" $ do
    describe "Deriving address public key" $ do
        it "fails if address index is hardened" $
            property prop_cannotDeriveAddressPublicKeyForHardenedIx
        it "fails if address index is non-hardened" $
            property prop_deriveAddressPublicKeyForNonHardenedIx
        it "equals to deriving address private key and extracting public part from it: N(CKDpriv((kpar, cpar), i)) === CKDpub(N(kpar, cpar), i)" $
            property prop_deriveAddressPublicFromAccountPrivateKey
    describe "Deriving address private key" $ do
        it "fails if password differs" $
            expectFailure prop_deriveAddressPrivateKeyWrongPassword
        it "fails if address index is hardened" $
            expectFailure prop_deriveAddressPrivateKeyHardened
