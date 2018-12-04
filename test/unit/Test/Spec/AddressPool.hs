{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE TypeApplications #-}

module Test.Spec.AddressPool (spec) where

import           Universum

import           Cardano.Wallet.Kernel.AddressPool (getAddressPoolSize,
                     initAddressPool, lookupAddressPool)
import           Cardano.Wallet.Kernel.AddressPoolGap (AddressPoolGap,
                     mkAddressPoolGap)

import           Test.Hspec (Spec, describe, it)
import           Test.Pos.Core.Arbitrary ()
import           Test.QuickCheck (Property, property, (===))

-- | Since the pool is polymorphic by address type, we can use any type, even 'Word'.
mkRandomALaAddress :: Word -> Word
mkRandomALaAddress ix = ix

smallestGap, biggestGap :: AddressPoolGap
smallestGap = minBound
biggestGap  = maxBound

-- | It proves that we:
-- 1. can create 'AddressPoolGap' from any number within the min-max range,
-- 2. cannot create 'AddressPoolGap' from any other number.
prop_gapGeneratorIsValid
    :: Int
    -> Property
prop_gapGeneratorIsValid gap =
    case mkAddressPoolGap (fromIntegral gap) of
        Left _  -> isWithinBound gap === False
        Right _ -> isWithinBound gap === True
  where
    isWithinBound g =
        g >= fromIntegral smallestGap &&
        g <= fromIntegral biggestGap

-- | It proves that pool extension works as expected.
prop_poolExtensionIsValid
    :: AddressPoolGap
    -> Word
    -> Property
prop_poolExtensionIsValid validGap aLaAddress = do
    let pool = initAddressPool validGap mkRandomALaAddress
        (result, pool') = lookupAddressPool aLaAddress pool
    property $ case result of
        Nothing -> getAddressPoolSize pool == getAddressPoolSize pool'
        Just (foundAddr, foundAddrIx) -> do
            -- Since we found this address in the pool, it could be extended.
            if getAddressPoolSize pool == getAddressPoolSize pool'
                then aLaAddress == foundAddr
                else aLaAddress == foundAddr &&
                        -- Pool was extended, so found address was "on edge":
                        -- new addresses was added in it so number of "consecutive
                        -- undiscovered addresses" is equal to 'validGap'.
                        getAddressPoolSize pool' - (fromIntegral foundAddrIx + 1) == fromIntegral validGap

spec :: Spec
spec = describe "AddressPool" $ do
    describe "Gap" $
        it "gap generator works as expected" $
            property prop_gapGeneratorIsValid

    describe "Pool" $
        it "works as expected if we lookup existing address in the pool" $
            property prop_poolExtensionIsValid
