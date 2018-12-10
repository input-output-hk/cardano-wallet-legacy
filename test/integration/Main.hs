module Main where

import           Universum hiding (keys)

import           Test.Hspec (beforeAll, describe, hspec)

import           Cardano.Cluster (MaxWaitingTime (..), NodeType (..))
import           Test.Integration.Framework.Cluster (startCluster, waitForNode)
import           Test.Integration.Framework.DSL (Context (..))
import qualified Test.Integration.Scenario.Accounts as Accounts
import qualified Test.Integration.Scenario.Addresses as Addresses
import qualified Test.Integration.Scenario.Transactions as Transactions
import qualified Test.Integration.Scenario.Wallets as Wallets

main :: IO ()
main = do
    -- Start cluster
    putTextLn "Starting cluster of nodes... ╰( ͡° ͜ʖ ͡° )つ──☆*:・ﾟ"
    putTextLn "Suggestion: tail -F ./state-integration/logs/edge.log.pub"
    ctx <- uncurry Context <$> startCluster
        [ ("core0", NodeCore)
        , ("core1", NodeCore)
        , ("core2", NodeCore)
        , ("relay", NodeRelay)
        , ("edge", NodeEdge)
        ]
    waitForNode (_client ctx) (MaxWaitingTime 90)

    -- Run tests
    hspec $ beforeAll (newMVar ctx) $ do
        describe "Accounts" Accounts.spec
        describe "Addresses" Addresses.spec
        describe "Transactions" Transactions.spec
        describe "Wallets" Wallets.spec