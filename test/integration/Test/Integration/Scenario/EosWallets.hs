{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Test.Integration.Scenario.EosWallets
    ( spec
    ) where

import           Universum

import           Cardano.Wallet.Client.Http (ClientError, Wallet)
import qualified Cardano.Wallet.Client.Http as Client
import           Test.Integration.Framework.DSL

spec :: Scenarios Context
spec = do
    scenario "EOSWALLETS_CREATE_01 - one can use default address pool gap" $ do
        fixture  <- setup defaultSetup
        eowallet <- successfulRequest $ Client.postEosWallet $- NewEosWallet
            (take 3 $ fixture ^. externallyOwnedAccounts)
            noAddressPoolGap
            defaultAssuranceLevel
            defaultWalletName

        response <- request $ Client.getEosWallet $- eowallet ^. walletId

        verify response
            [ expectFieldEqual walletId (eowallet ^. walletId)
            , expectFieldEqual assuranceLevel defaultAssuranceLevel
            , expectFieldEqual walletName defaultWalletName
            , expectFieldEqual addressPoolGap defaultAddressPoolGap
            ]

    scenario "EOSWALLETS_CREATE_02 - one can configure the address pool gap" $ do
        fixture  <- setup $ defaultSetup
            & rawAddressPoolGap .~ 14
        eowallet <- successfulRequest $ Client.postEosWallet $- NewEosWallet
            (take 3 $ fixture ^. externallyOwnedAccounts)
            (Just $ fixture ^. addressPoolGap)
            defaultAssuranceLevel
            defaultWalletName

        response <- request $ Client.getEosWallet $- eowallet ^. walletId

        verify response
            [ expectFieldEqual walletId (eowallet ^. walletId)
            , expectFieldEqual assuranceLevel defaultAssuranceLevel
            , expectFieldEqual walletName defaultWalletName
            , expectFieldEqual addressPoolGap (fixture ^. addressPoolGap)
            ]

    scenario "EOSWALLETS_CREATE_03 - one can't provide an empty list of accounts" $ do
        pendingWith "TODO: will add feature / guard as part of #236"

        response <- request $ Client.postEosWallet $- NewEosWallet
            []
            noAddressPoolGap
            defaultAssuranceLevel
            defaultWalletName

        verify response
            [ expectError
            ]
