module Cardano.Wallet.Kernel.DB.HdWallet.Derivation (
      deriveIndex
    , HardeningMode(..)
    , DerivationSchemeVersion (..)
    , derivationSchemeVersion
    ) where

import           Universum

import           Cardano.Crypto.Wallet.Types (DerivationIndex)
import qualified Pos.Core as Core
import           Pos.Core.Attributes (Attributes (attrData))
import           Pos.Crypto.HD (firstHardened)
import           Test.QuickCheck (Arbitrary (..), elements)

data HardeningMode = SoftDerivation
                   -- ^ Generates indexes in the range (0, maxBound @Word32)
                   | HardDerivation
                   -- ^ Generates indexes in the range (0x8000000, maxBound @Word32)

-- | Derives a new '
deriveIndex :: Monad m
            => ((DerivationIndex, DerivationIndex) -> m DerivationIndex)
            -- ^ A monadic computation which can pick a 'DerivationIndex' out
            -- of a range.
            -> (DerivationIndex -> a)
            -- How to build the final type out of the picked 'DerivationIndex'.
            -> HardeningMode
            -- ^ How we want to derive this index (@soft@ vs @hard@)
            -> m a
deriveIndex pickRange mkA hardeningMode =
    let range = case hardeningMode of
                     SoftDerivation -> (0, firstHardened - 1)
                     HardDerivation -> (firstHardened, maxBound)
    in mkA <$> pickRange range

-- Which derivation scheme is being used
data DerivationSchemeVersion
    -- Derivation scheme random is following bip32 scheme, ed25519v0 curve and addresses contain: account index and address index
    = DerivationSchemeRnd
    -- Derivation scheme sequential is bip44 scheme, ed25519v1 curve and addresses contain no address payload. Root key derivation from mnemonic keys also differs from root key derivation in random scheme
    | DerivationSchemeSeq
    deriving (Show)

derivationSchemeVersion :: Core.Address -> DerivationSchemeVersion
derivationSchemeVersion addr =
    if isJust mPayload
        then DerivationSchemeRnd
        else DerivationSchemeSeq
  where
    mPayload = Core.aaPkDerivationPath $ attrData $ Core.addrAttributes addr

instance Arbitrary DerivationSchemeVersion where
    arbitrary = elements [DerivationSchemeRnd, DerivationSchemeSeq]

