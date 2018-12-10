module Test.Integration.Scenario.Addresses
  ( spec
  ) where

import           Universum

import qualified Cardano.Wallet.Client.Http as Client
import           Test.Integration.Framework.DSL

spec :: Scenarios Context
spec = do
    scenario "address is available after it's been created" $ do
        fixture <- setup defaultSetup

        response <- request $ Client.postAddress $- NewAddress
            noSpendingPassword
            defaultAccountId
            (fixture ^. wallet . walletId)

        verify response
            [ expectAddressInIndexOf
            ]
