{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE TypeApplications #-}

module Test.Spec.Ed25519Bip44 (spec) where

import           Universum

import           Cardano.Crypto.Wallet (generate)
import           Pos.Crypto (PassPhrase (..), PublicKey, emptySalt, isHardened,
                     mkEncSecretWithSaltUnsafe)

import           Cardano.Wallet.Kernel.Ed25519Bip44 (ChangeChain,
                     deriveAccountPrivateKey, deriveAddressPublicKey)

import qualified Data.ByteString as BS
import           Test.Hspec (Spec, describe, it)
import           Test.Pos.Core.Arbitrary ()
import           Test.QuickCheck (InfiniteList (..), Property, expectFailure,
                     property, (==>))

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
        then isNothing result
        else isJust result
  where
    result = deriveAddressPublicKey accountPublicKey change addressIx
    -- This is maximum value for soft derivation (only soft derivation
    -- is allowed to derive public key from public key).
    maxIx = 0x8000000

-- | Deriving account private key should always fail
-- if account index is non-hardened
-- TODO (akegalj): this property is very weak
prop_deriveAccountPrivateKeyNotHardened
    :: InfiniteList Word8
    -> PassPhrase
    -> Word32
    -> Property
prop_deriveAccountPrivateKeyNotHardened (InfiniteList seed _) passPhrase@(PassPhrase passBytes) accountIx =
    not (isHardened accountIx) ==> property (isJust accPrvKey)
  where
    masterEncPrvKey = mkEncSecretWithSaltUnsafe emptySalt passPhrase $ generate (BS.pack $ take 32 seed) passBytes
    accPrvKey =
        deriveAccountPrivateKey
            masterEncPrvKey
            passPhrase
            accountIx

spec :: Spec
spec = describe "Ed25519Bip44" $ do
    describe "Deriving address public key" $ do
        it "fails if address index is too big" $
            property prop_cannotDeriveAddressPublicKeyForBigIx
    describe "Deriving account private key" $ do
        it "fails if account index is non-hardened" $
            expectFailure prop_deriveAccountPrivateKeyNotHardened
