module Golden.APIV1Types
   ( tests
   ) where

import           Universum

import           Hedgehog (Property)
import qualified Hedgehog as H

import           Cardano.Wallet.API.V1.Types (WalletInputSelectionPolicy (..),
                     WalletPassPhrase (..), WalletTimestamp (..))
import           Pos.Client.Txp.Util (InputSelectionPolicy (..))
import           Pos.Core (Timestamp, parseTimestamp)

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

golden_WalletTimestamp :: Property
golden_WalletTimestamp =
    goldenTestJSON
        (WalletTimestamp exampleTimestamp)
        "test/unit/Golden/golden/apiV1Types/json/Timestamp"

golden_WalletInputSelectionPolicy1 :: Property
golden_WalletInputSelectionPolicy1 =
    goldenTestJSON
        (WalletInputSelectionPolicy OptimizeForSecurity)
        "test/unit/Golden/golden/apiV1Types/json/InputSelectionPolicy1"

golden_WalletInputSelectionPolicy2 :: Property
golden_WalletInputSelectionPolicy2 =
    goldenTestJSON
        (WalletInputSelectionPolicy OptimizeForHighThroughput)
        "test/unit/Golden/golden/apiV1Types/json/InputSelectionPolicy2"

-------------------------------------------------------------------------------
-- Examples
-------------------------------------------------------------------------------

-- | Currently PassPhrase should be 32 bytes long.
examplePassPhrase :: PassPhrase
examplePassPhrase = PassPhrase "hello_new_awesome_cardano_wallet"

exampleTimestamp :: Timestamp
exampleTimestamp = timestamp
  where
    Just timestamp = parseTimestamp "2018-12-11T08:09:10"
