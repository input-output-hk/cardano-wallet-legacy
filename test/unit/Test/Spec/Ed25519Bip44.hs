{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE TypeApplications #-}

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
import           Test.QuickCheck (InfiniteList (..), Property, expectFailure,
                     property, (===), (==>))

-- | It proves that we cannot derive address public key
-- if address index is too big. We should be able to derive
-- Address public key only with non-hardened address index
prop_cannotDeriveAddressPublicKeyForBigIx
    :: PublicKey
    -> ChangeChain
    -> Word32
    -> Property
prop_cannotDeriveAddressPublicKeyForBigIx accPubKey change addressIx = property $
    if isHardened addressIx
        then isNothing addrPubKey
        else isJust addrPubKey
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
    not (isHardened addressIx) ==> (addrPubKey1 === addrPubKey2)
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
    isHardened addressIx ==> property (isJust addrPrvKey)
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
    isNothing (checkPassMatches passPhrase accEncPrvKey) ==> property (isJust addrPrvKey)
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
        it "fails if address index is too big (hardened)" $
            property prop_cannotDeriveAddressPublicKeyForBigIx
        it "equals to deriving address private key and extracting public part from it: N(CKDpriv((kpar, cpar), i)) === CKDpub(N(kpar, cpar), i)" $
            property prop_deriveAddressPublicFromAccountPrivateKey
    describe "Deriving address private key" $ do
        it "fails if password differs" $
            expectFailure prop_deriveAddressPrivateKeyWrongPassword
        it "fails if address index is hardened" $
            expectFailure prop_deriveAddressPrivateKeyHardened
