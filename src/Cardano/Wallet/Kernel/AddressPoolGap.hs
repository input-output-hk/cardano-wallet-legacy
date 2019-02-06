{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Cardano.Wallet.Kernel.AddressPoolGap (
    -- ** Address pool gap (for EOS-wallets)
    AddressPoolGap
  , MkAddressPoolGapError (..)
  , mkAddressPoolGap
  ) where

import           Universum

import           Control.Lens ((?~))
import           Data.Aeson (FromJSON (..), ToJSON)
import           Data.Default (Default (..))
import           Data.SafeCopy (base, deriveSafeCopy)
import           Data.Swagger (NamedSchema (..), ToSchema (..), maximum_,
                     minimum_)
import           Data.Text.Read (decimal)

import           Formatting (bprint, build, int, sformat, (%))
import qualified Formatting.Buildable

import           Servant.API (FromHttpApiData (..), ToHttpApiData (..))
import           Test.QuickCheck
import qualified Test.QuickCheck.Gen as Gen

newtype AddressPoolGap = AddressPoolGap { getAddressPoolGap :: Word8 }
    deriving (Eq, Enum, Generic, Num, Ord, Real, Show)
    deriving newtype (Integral, ToJSON)

instance FromJSON AddressPoolGap where
    parseJSON = parseJSON >=> \gap -> case mkAddressPoolGap gap of
        Left err -> fail $ toString $ sformat build err
        Right x  -> pure x

instance Bounded AddressPoolGap where
    -- NOTE: these values may change in the future.
    minBound = AddressPoolGap 10
    maxBound = AddressPoolGap 100

newtype MkAddressPoolGapError = GapOutOfRange Word8
    deriving (Eq, Show)

instance Buildable MkAddressPoolGapError where
    build (GapOutOfRange invalidGap) = bprint
        ("Address pool gap should be in range ["%int%".."%int%"], but "%int%" was provided.")
        (getAddressPoolGap minBound)
        (getAddressPoolGap maxBound)
        invalidGap

-- | Default value of address pool gap is taken from BIP-44 specification.
instance Default AddressPoolGap where
    def = AddressPoolGap 20

instance Arbitrary AddressPoolGap where
    arbitrary = AddressPoolGap <$>
        Gen.choose (getAddressPoolGap minBound, getAddressPoolGap maxBound)

instance Buildable AddressPoolGap where
    build (AddressPoolGap gap) =
        bprint ("Address pool gap "%int) gap

instance ToSchema AddressPoolGap where
    declareNamedSchema _ = do
        NamedSchema _ s <- declareNamedSchema $ Proxy @Word8
        return $ NamedSchema (Just "AddressPoolGap") $ s
            & minimum_ ?~ fromIntegral (minBound :: AddressPoolGap)
            & maximum_ ?~ fromIntegral (maxBound :: AddressPoolGap)

instance FromHttpApiData AddressPoolGap where
    parseQueryParam rawGap = case decimal rawGap of
        Left _ -> Left "Unable to parse address pool gap (not an integer value)"
        Right (gap, _) -> case mkAddressPoolGap gap of
            Left problem   -> Left (sformat build problem)
            Right validGap -> Right validGap

instance ToHttpApiData AddressPoolGap where
    toQueryParam gap = sformat build gap

-- | Smart constructor for address pool gap.
mkAddressPoolGap :: Word8 -> Either MkAddressPoolGapError AddressPoolGap
mkAddressPoolGap gap
    | gap >= getAddressPoolGap minBound &&
      gap <= getAddressPoolGap maxBound = Right $ AddressPoolGap gap
    | otherwise = Left $ GapOutOfRange gap

deriveSafeCopy 1 'base ''AddressPoolGap
