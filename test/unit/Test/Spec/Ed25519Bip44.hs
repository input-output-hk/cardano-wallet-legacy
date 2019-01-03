{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE TypeApplications           #-}

module Test.Spec.Ed25519Bip44 (spec) where

import           Universum

import           Cardano.Crypto.Wallet (generate)
import           Pos.Crypto (EncryptedSecretKey, PassPhrase (..), PublicKey,
                     checkPassMatches, emptySalt, mkEncSecretWithSaltUnsafe)

import           Cardano.Wallet.Kernel.Ed25519Bip44 (ChangeChain,
                     deriveAccountPrivateKey, deriveAddressPrivateKey,
                     deriveAddressPublicKey, derivePublicKey)

import qualified Data.ByteString as BS
import           Test.Hspec (Spec, describe, it)
import           Test.Pos.Core.Arbitrary ()
import           Test.QuickCheck (Arbitrary (..), InfiniteList (..), Property,
                     arbitraryBoundedIntegral, arbitrarySizedBoundedIntegral,
                     property, shrinkIntegral, (.&&.), (===), (==>))

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
prop_deriveAddressPublicKeyHardened
    :: PublicKey
    -> ChangeChain
    -> Hardened
    -> Property
prop_deriveAddressPublicKeyHardened accPubKey change (Hardened addressIx) =
    property $ isNothing addrPubKey
  where
    addrPubKey = deriveAddressPublicKey accPubKey change addressIx

-- | Deriving address public key should succeed if address index
-- is non-hardened.
prop_deriveAddressPublicKeyNonHardened
    :: PublicKey
    -> ChangeChain
    -> NonHardened
    -> Property
prop_deriveAddressPublicKeyNonHardened accPubKey change (NonHardened addressIx) =
    property $ isJust addrPubKey
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
    -> NonHardened
    -> Property
prop_deriveAddressPublicFromAccountPrivateKey (InfiniteList seed _) passPhrase@(PassPhrase passBytes) changeChain (NonHardened addressIx) =
    -- TODO (akegalj): check coverage with quickcheck @cover@
    isJust addrPubKey1 .&&. addrPubKey1 === addrPubKey2
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
-- if address index is hardened
prop_deriveAddressPrivateKeyHardened
    :: InfiniteList Word8
    -> PassPhrase
    -> ChangeChain
    -> Hardened
    -> Property
prop_deriveAddressPrivateKeyHardened (InfiniteList seed _) passPhrase@(PassPhrase passBytes) changeChain (Hardened addressIx) =
    property $ isNothing addrPrvKey
  where
    accEncPrvKey = mkEncSecretWithSaltUnsafe emptySalt passPhrase $ generate (BS.pack $ take 32 seed) passBytes
    addrPrvKey =
        deriveAddressPrivateKey
            passPhrase
            accEncPrvKey
            changeChain
            addressIx

-- | Deriving address private key should always succeed
-- if address index is non-hardened
prop_deriveAddressPrivateKeyNonHardened
    :: InfiniteList Word8
    -> PassPhrase
    -> ChangeChain
    -> NonHardened
    -> Property
prop_deriveAddressPrivateKeyNonHardened (InfiniteList seed _) passPhrase@(PassPhrase passBytes) changeChain (NonHardened addressIx) =
    property $ isJust addrPrvKey
  where
    accEncPrvKey = mkEncSecretWithSaltUnsafe emptySalt passPhrase $ generate (BS.pack $ take 32 seed) passBytes
    addrPrvKey =
        deriveAddressPrivateKey
            passPhrase
            accEncPrvKey
            changeChain
            addressIx

-- | Deriving address private key should always fail for non-hardened key index
-- if password differs from account private key password
prop_deriveAddressPrivateKeyWrongPassword
    :: PassPhrase
    -> EncryptedSecretKey
    -> ChangeChain
    -> NonHardened
    -> Property
prop_deriveAddressPrivateKeyWrongPassword passPhrase accEncPrvKey changeChain (NonHardened addressIx) =
    isNothing (checkPassMatches passPhrase accEncPrvKey) ==> isNothing addrPrvKey
  where
    addrPrvKey =
        deriveAddressPrivateKey
            passPhrase
            accEncPrvKey
            changeChain
            addressIx

-- | Deriving address private key should always succeed for non-hardened key index
-- if password equals to account private key password
prop_deriveAddressPrivateKeyCorrectPassword
    :: PassPhrase
    -> EncryptedSecretKey
    -> ChangeChain
    -> NonHardened
    -> Property
prop_deriveAddressPrivateKeyCorrectPassword passPhrase accEncPrvKey changeChain (NonHardened addressIx) =
    isJust (checkPassMatches passPhrase accEncPrvKey) ==> isJust addrPrvKey
  where
    addrPrvKey =
        deriveAddressPrivateKey
            passPhrase
            accEncPrvKey
            changeChain
            addressIx

-- | Deriving account private key should always fail
-- if account index is non-hardened
prop_deriveAccountPrivateKeyNonHardened
    :: InfiniteList Word8
    -> PassPhrase
    -> NonHardened
    -> Property
prop_deriveAccountPrivateKeyNonHardened (InfiniteList seed _) passPhrase@(PassPhrase passBytes) (NonHardened accountIx) =
    property $ isNothing accPrvKey
  where
    masterEncPrvKey = mkEncSecretWithSaltUnsafe emptySalt passPhrase $ generate (BS.pack $ take 32 seed) passBytes
    accPrvKey =
        deriveAccountPrivateKey
            passPhrase
            masterEncPrvKey
            accountIx

-- | Deriving account private key should always succeed
-- if account index is hardened
prop_deriveAccountPrivateKeyHardened
    :: InfiniteList Word8
    -> PassPhrase
    -> Hardened
    -> Property
prop_deriveAccountPrivateKeyHardened (InfiniteList seed _) passPhrase@(PassPhrase passBytes) (Hardened accountIx) =
    property $ isJust accPrvKey
  where
    masterEncPrvKey = mkEncSecretWithSaltUnsafe emptySalt passPhrase $ generate (BS.pack $ take 32 seed) passBytes
    accPrvKey =
        deriveAccountPrivateKey
            passPhrase
            masterEncPrvKey
            accountIx

spec :: Spec
spec = describe "Ed25519Bip44" $ do
    describe "Deriving address public key" $ do
        it "fails if address index is hardened" $
            property prop_deriveAddressPublicKeyHardened
        it "succeeds if address index is non-hardened" $
            property prop_deriveAddressPublicKeyNonHardened
        it "equals to deriving address private key and extracting public part from it: N(CKDpriv((kpar, cpar), i)) === CKDpub(N(kpar, cpar), i)" $
            property prop_deriveAddressPublicFromAccountPrivateKey
    describe "Deriving address private key" $ do
        it "fails if address index is hardened" $
            property prop_deriveAddressPrivateKeyHardened
        it "succeeds if address index is non-hardened" $
            property prop_deriveAddressPrivateKeyNonHardened
        it "fails if passwords differ" $
            property prop_deriveAddressPrivateKeyWrongPassword
        it "succeeds if passwords are equal" $
            property prop_deriveAddressPrivateKeyCorrectPassword
    describe "Deriving account private key" $ do
        it "fails if account index is non-hardened" $
            property prop_deriveAccountPrivateKeyNonHardened
        it "succeeds if account index is hardened" $
            property prop_deriveAccountPrivateKeyHardened
