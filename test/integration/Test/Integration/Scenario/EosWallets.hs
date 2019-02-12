{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Test.Integration.Scenario.EosWallets
    ( spec
    ) where

import           Universum

import qualified Data.List.NonEmpty as NE

import           Cardano.Wallet.Client.Http (ClientError, Wallet)
import qualified Cardano.Wallet.Client.Http as Client
import           Test.Hspec (describe)
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

    describe "EOSWALLETS_CREATE_02 - addressPoolGap cannot be outside [10..100]" $ do
        forM_ ([-1, 0, 9, 101]) $ \poolGap -> scenario ("addressPoolGap = " ++ show (poolGap :: Int)) $ do
            let endpoint = "api/v1/wallets/externally-owned"
            response <- unsafeRequest ("POST", endpoint) $ Just $ [json|{
                "accounts": [
                            {
                                "publicKey": "1OQQ6jrO8xzPyybgLEk5vuUcoCCH4fds3k5rqnxErglRb7EiGQKa74TP9jx0ATHCqUiD8uLO6pP8z31+c393lw==",
                                "index": 2710723942
                            }
                            ],
                "addressPoolGap": #{poolGap},
                "assuranceLevel": "normal",
                "name": "My EOS Wallet"
                }|]
            verify (response :: Either ClientError EosWallet)
                [ expectJSONError $ case poolGap of
                                        -1 -> "Error in $.addressPoolGap: Word8 is either floating or will cause over or underflow: -1.0"
                                        _  -> "Error in $.addressPoolGap: Address pool gap should be in range [10..100]"
                ]

    scenario "EOSWALLETS_CREATE_03 - one can't provide an empty list of accounts" $ do
        let endpoint = "api/v1/wallets/externally-owned"
        response <- unsafeRequest ("POST", endpoint) $ Just $ [json|{
            "accounts": [],
            "addressPoolGap": 10,
            "assuranceLevel": "normal",
            "name": "My EOS Wallet"
        }|]
        verify (response :: Either ClientError EosWallet)
            [ expectError
            ]

    describe "EOSWALLETS_CREATE_04 - one has to provide assuranceLevel to be either 'normal' or 'strict'" $ do
        let matrix =
                [ ( "normal"
                  , [json| "normal" |]
                  , [ expectSuccess ]
                  )
                , ( "strict"
                  , [json| "strict" |]
                  , [ expectSuccess ]
                  )
                , ( "empty string"
                  , [json| "" |]
                  , [ expectJSONError "Error in $.assuranceLevel: When parsing Cardano.Wallet.API.V1.Types.AssuranceLevel expected a String with the tag of a constructor but got ." ]
                  )
                , ( "555"
                  , [json| 555 |]
                  , [ expectJSONError "Error in $.assuranceLevel: When parsing Cardano.Wallet.API.V1.Types.AssuranceLevel expected String but got Number." ]
                  )
                , ( "亜哀愛źiemniak悪握圧扱安"
                  , [json| "亜哀愛źiemniak悪握圧扱安" |]
                  , [ expectJSONError "Error in $.assuranceLevel: When parsing Cardano.Wallet.API.V1.Types.AssuranceLevel expected a String with the tag of a constructor but got 亜哀愛źiemniak悪握圧扱安" ]
                  )
                ]

        forM_ matrix $ \(title, assurLevel, expectations) -> scenario ("assuranceLevel = " ++ title) $ do
            let endpoint = "api/v1/wallets/externally-owned"
            response <- unsafeRequest ("POST", endpoint) $ Just $ [json|{
                "accounts": [
                            {
                                "publicKey": "1OQQ6jrO8xzPyybgLEk5vuUcoCCH4fds3k5rqnxErglRb7EiGQKa74TP9jx0ATHCqUiD8uLO6pP8z31+c393lw==",
                                "index": 2710723942
                            }
                            ],
                "assuranceLevel": #{assurLevel},
                "addressPoolGap": 10,
                "name": "My EOS Wallet"
            }|]
            verify (response :: Either ClientError EosWallet) expectations

    scenario "EOSWALLETS_CREATE_05 - one has to provide wallet's name" $ do
        response <- unsafeRequest ("POST", "api/v1/wallets/externally-owned") $ Just $ [json|{
            "accounts": [
                        {
                            "publicKey": "1OQQ6jrO8xzPyybgLEk5vuUcoCCH4fds3k5rqnxErglRb7EiGQKa74TP9jx0ATHCqUiD8uLO6pP8z31+c393lw==",
                            "index": 2710723942
                        }
                        ],
            "addressPoolGap": 70,
            "assuranceLevel": "strict"
            }|]
        verify (response :: Either ClientError EosWallet)
            [ expectJSONError "Error in $: When parsing the record newEosWallet of type Cardano.Wallet.API.V1.Types.NewEosWallet the key name was not present."
            ]
