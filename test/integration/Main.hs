module Main where

import           Universum

import           Test.Hspec (describe, hspec)
import qualified Test.Integration.APISanityChecks as APISanityChecks
import           Test.Integration.Framework.Scenario (runWithContext)
import qualified Test.Integration.Scenario.Accounts as Accounts
import qualified Test.Integration.Scenario.Addresses as Addresses
import qualified Test.Integration.Scenario.Transactions as Transactions
import qualified Test.Integration.Scenario.Wallets as Wallets


main :: IO ()
main = do
    (ctx, manager) <- error "TODO: \
        \\n- Start Cluster\
        \\n- Start Wallet Server\
        \\n- Create HTTP(s) Manager & HTTP(s) Client\
        \\n- Bootstrap initial context (Create faucet wallets from genesis)"

    mvar <- newMVar ctx

    hspec $ do
        describe "API Sanity Checks" (APISanityChecks.spec manager)

        runWithContext mvar $ do
            describe "Accounts" Accounts.spec
            describe "Addresses" Addresses.spec
            describe "Transactions" Transactions.spec
            describe "Wallets" Wallets.spec
