{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE TypeApplications #-}

module Test.Spec.Ed25519Bip44 (spec) where

import           Universum

import           Pos.Crypto (EncryptedSecretKey, PassPhrase, PublicKey,
                     ShouldCheckPassphrase (..), checkPassMatches)

import           Cardano.Wallet.Kernel.Ed25519Bip44 (ChangeChain,
                     deriveAddressPrivateKey, deriveAddressPublicKey,
                     derivePublicKey)

import           Test.Hspec (Spec, describe, it)
import           Test.Pos.Core.Arbitrary ()
import           Test.QuickCheck (Property, expectFailure, property, (===),
                     (==>))

-- | It proves that we cannot derive address public key
-- if address index is too big. We should be able to derive
-- Address public key only with non-hardened address index
prop_cannotDeriveAddressPublicKeyForBigIx
    :: PublicKey
    -> ChangeChain
    -> Word32
    -> Property
prop_cannotDeriveAddressPublicKeyForBigIx accountPublicKey change addressIx = property $
    if addressIx >= maxIx
        then isNothing addrPubKey
        else isJust addrPubKey
  where
    addrPubKey = deriveAddressPublicKey accountPublicKey change addressIx
    -- This is maximum value for soft derivation (only soft derivation
    -- is allowed to derive public key from public key).
    maxIx = 0x8000000

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
    :: PassPhrase
    -> EncryptedSecretKey
    -> ChangeChain
    -> Word32
    -> Property
prop_deriveAddressPublicFromAccountPrivateKey passPhrase accEncPrvKey changeChain addressIx =
    -- TODO (akegalj): check coverage with quickcheck @cover@
    -- FIXME (akegalj): instead of doing this create generator for non-hardened keys.
    -- This should in average discard 50% of examples and will thus be
    -- 50% slower.
    isNonHardened addressIx ==> (addrPubKey1 === addrPubKey2)
  where
    isNonHardened = (< 0x80000000)
    -- N(CKDpriv((kpar, cpar), i))
    addrPubKey1 =
        derivePublicKey <$> deriveAddressPrivateKey
            (ShouldCheckPassphrase False)
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

-- | Deriving address private key should always succeed
-- if password is not checked
prop_deriveAddressPrivateKeyNoPassword
    :: PassPhrase
    -> EncryptedSecretKey
    -> ChangeChain
    -> Word32
    -> Property
prop_deriveAddressPrivateKeyNoPassword passphrase accEncPrvKey changeChain =
    property . isJust .
        deriveAddressPrivateKey
            (ShouldCheckPassphrase False)
            passphrase
            accEncPrvKey
            changeChain

-- | Deriving address private key should always fail
-- if password is checked and differs from account private key password
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
    (isNothing $ checkPassMatches passPhrase accEncPrvKey) ==> property (isJust addrPrvKey)
  where
    addrPrvKey =
        deriveAddressPrivateKey
            (ShouldCheckPassphrase True)
            passPhrase
            accEncPrvKey
            changeChain
            addressIx

spec :: Spec
spec = describe "Ed25519Bip44" $ do
    describe "Deriving address public key" $ do
        it "fails if address index is too big" $
            property prop_cannotDeriveAddressPublicKeyForBigIx
        it "equals to deriving address private key and extracting public part from it: N(CKDpriv((kpar, cpar), i)) === CKDpub(N(kpar, cpar), i)" $
            property prop_deriveAddressPublicFromAccountPrivateKey
    describe "Deriving address private key" $ do
        it "succeeds if password is not checked" $
            property prop_deriveAddressPrivateKeyNoPassword
        it "fails if password is checked and differs" $
            expectFailure prop_deriveAddressPrivateKeyWrongPassword
