module Golden.APIV1Types
   ( tests
   ) where

import           Universum

import           Hedgehog (Property)
import qualified Hedgehog as H

import           Cardano.Wallet.API.V1.Types (WalletPassPhrase (..))
import           Pos.Crypto (PassPhrase (..))

import           Test.Pos.Util.Golden (discoverGolden, goldenTestJSON)

-----------------------------------------------------------------------
-- Main test export
-----------------------------------------------------------------------

tests :: IO Bool
tests =
    H.checkSequential $$discoverGolden

-------------------------------------------------------------------------------
-- API V1 ToJSON/FromJSON
-------------------------------------------------------------------------------

golden_WalletPassPhrase :: Property
golden_WalletPassPhrase =
    goldenTestJSON
        (WalletPassPhrase examplePassPhrase)
        "test/unit/Golden/golden/apiV1Types/json/PassPhrase"

-------------------------------------------------------------------------------
-- Examples
-------------------------------------------------------------------------------

-- | Currently PassPhrase should be 32 bytes long.
examplePassPhrase :: PassPhrase
examplePassPhrase = PassPhrase "hello_new_awesome_cardano_wallet"
