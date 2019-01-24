module Test.Spec.AddressPool (spec) where

import           Universum

import           Cardano.Wallet.Kernel.AddressPool (AddressPool,
                     ErrAddressPoolInvalid (..), emptyAddressPool,
                     getAddressPoolGap, getAddressPoolSize, initAddressPool,
                     lookupAddressPool, verifyPool)
import           Cardano.Wallet.Kernel.AddressPoolGap (AddressPoolGap)

import           Test.Hspec (Spec, describe, it)
import           Test.Pos.Core.Arbitrary ()
import           Test.QuickCheck (Arbitrary (..), Property, choose, conjoin,
                     label, property, (===))

spec :: Spec
spec = describe "AddressPool" $ do
    it "initAddressPool" $ property prop_initAddressPool
    it "emptyAddressPool" $ property prop_emptyAddressPool
    it "lookupAddressPool" $ property prop_lookupAddressPool

prop_initAddressPool
    :: (Parameters Word32)
    -> Maybe ErrAddressPoolInvalid
    -> Property
prop_initAddressPool (Parameters gap addrs0) = \case
    Nothing -> label "valid params" $
        void (initAddressPool gap identity addrs0) === Right ()
    Just e -> label ("invalid params: " <> show e) $
        let
            -- NOTE Manunally craft a faulty list of addresses
            g = fromIntegral gap
            addrs0' = case e of
                ErrIndexesAreNotSequential ->
                    replace (g - 1) (9999, 9999) addrs0
                ErrNotEnoughAddresses ->
                    take (g - 1) addrs0
        in
            void (initAddressPool gap identity addrs0') === Left e
  where
    replace i x xs =
        let front = take (i - 1) xs; back = drop i xs
        in  front ++ [x] ++ back

prop_emptyAddressPool
    :: AddressPoolGap
    -> Property
prop_emptyAddressPool gap =
    let pool = emptyAddressPool gap identity  in pool `seq` property True

-- | It proves that pool extension works as expected.
prop_lookupAddressPool
    :: (Parameters Word32, Int)
    -> Property
prop_lookupAddressPool (Parameters gap addrs0, addr) =
    let Right pool = initAddressPool gap identity addrs0
    in  prop_lookupAddressPool' (pool, fromIntegral addr)

prop_lookupAddressPool'
    :: (Show address, Ord address, Eq address)
    => (AddressPool address, address)
    -> Property
prop_lookupAddressPool' (pool, addr) =
    case lookupAddressPool addr pool of
        (Nothing, pool') -> label "hit outside pool" $ conjoin
            [ void (verifyPool pool') === Right ()
            , on (===) getAddressPoolSize pool pool'
            , on (===) getAddressPoolGap  pool pool'
            ]
        (Just (addr', _), pool') -> label "hit within pool" $
            let k = on (-) getAddressPoolSize pool' pool
            in conjoin
                [ void (verifyPool pool') === Right ()
                , addr' === addr
                , on (===) getAddressPoolGap pool pool'
                , property (k >= 0 && k <= fromIntegral (getAddressPoolGap pool))
                ]


{-------------------------------------------------------------------------------
  Utils
-------------------------------------------------------------------------------}

data Parameters address = Parameters
    { pGap              :: AddressPoolGap
    , pInitialAddresses :: [(address, Word32)]
    }
    deriving Show

instance Arbitrary (Parameters Word32) where
    arbitrary = do
        gap <- arbitrary
        n <- choose (fromIntegral gap, 5 * (fromIntegral gap))
        return $ Parameters gap (zip [0..n] [0..])
    shrink (Parameters gap xs) = Parameters gap
        <$> filter ((>= fromIntegral gap) . length)
                [ take (length xs `div` 2) xs
                , take (length xs - 1) xs
                ]
