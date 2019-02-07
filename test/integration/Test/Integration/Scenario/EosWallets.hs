{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Test.Integration.Scenario.EosWallets
    ( spec
    ) where

import           Universum

import qualified Data.List.NonEmpty as NE

import           Cardano.Wallet.Client.Http (ClientError, Wallet)
import qualified Cardano.Wallet.Client.Http as Client
import           Test.Integration.Framework.DSL


spec :: Scenarios Context
spec = do
    scenario "EOSWALLETS_CREATE_01 - one can use default address pool gap" $ do
        fixture  <- setup defaultSetup
        eowallet <- successfulRequest $ Client.postEosWallet $- NewEosWallet
            (NE.fromList $ take 3 $ fixture ^. externallyOwnedAccounts)
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
            (NE.fromList $ take 3 $ fixture ^. externallyOwnedAccounts)
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
        let endpoint = "api/v1/wallets/externally-owned"
        response <- unsafeRequest ("POST", endpoint) $ Just $ [json|{
            "accounts": [],
            "addressPoolGap": "#{noAddressPoolGap}",
            "assuranceLevel": "#{defaultAssuranceLevel}",
            "name": "#{defaultWalletName}"
        }|]
        verify (response :: Either ClientError EosWallet)
            [ expectError
            ]
