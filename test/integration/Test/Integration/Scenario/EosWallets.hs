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
import           Cardano.Wallet.API.V1.Types

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
                  , [ expectSuccess
                    , expectFieldEqual assuranceLevel NormalAssurance
                    ]
                  )
                , ( "strict"
                  , [json| "strict" |]
                  , [ expectSuccess
                    , expectFieldEqual assuranceLevel StrictAssurance
                    ]
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


    scenario "EOSWALLETS_CREATE_06 - create wallet reponse returns wallet details" $ do
        fixture  <- setup defaultSetup
        eowallet <- request $ Client.postEosWallet $- NewEosWallet
            (NE.fromList $ take 3 $ fixture ^. externallyOwnedAccounts)
            noAddressPoolGap
            defaultAssuranceLevel
            defaultWalletName
        verify eowallet
            [ expectFieldEqual assuranceLevel defaultAssuranceLevel
            , expectFieldEqual walletName defaultWalletName
            , expectFieldEqual addressPoolGap defaultAddressPoolGap
            , expectFieldEqual amount 0
            ]


    scenario "EOSWALLETS_DELETE_01 - deleted wallet is not available" $ do
        fixture  <- setup defaultSetup
        eowallet <- successfulRequest $ Client.postEosWallet $- NewEosWallet
            (NE.fromList $ take 3 $ fixture ^. externallyOwnedAccounts)
            noAddressPoolGap
            defaultAssuranceLevel
            defaultWalletName

        successfulRequest $ Client.deleteEosWallet $- eowallet ^. walletId

        response <- request $ Client.getEosWallet $- eowallet ^. walletId
        verify response
            [ expectWalletError (WalletNotFound)
            ]

    scenario "EOSWALLETS_DELETE_02 - cannot delete FO wallet with EOS endpoint" $ do
        fixture  <- setup defaultSetup

        delResp <- request $ Client.deleteEosWallet $- fixture ^. wallet . walletId
        verify delResp
            [ expectWalletError (WalletNotFound)
            ]

        getFo <- request $ Client.getWallet $- fixture ^. wallet . walletId
        verify getFo
            [ expectSuccess
            ]

    scenario "EOSWALLETS_DELETE_02 - cannot delete EOS wallet with FO endpoint" $ do
        fixture  <- setup defaultSetup
        eowallet <- successfulRequest $ Client.postEosWallet $- NewEosWallet
            (NE.fromList $ take 3 $ fixture ^. externallyOwnedAccounts)
            noAddressPoolGap
            defaultAssuranceLevel
            defaultWalletName

        delResp <- request  $ Client.deleteWallet $- eowallet ^. walletId
        verify delResp
            [ expectWalletError (WalletNotFound)
            ]

        getEos <- request $ Client.getEosWallet $- eowallet ^. walletId
        verify getEos
            [ expectSuccess
            ]


    scenario "EOSWALLETS_DETAILS_01 - cannot get FO wallet with EOS endpoint" $ do
        fixture  <- setup defaultSetup

        getFoWithEos <- request $ Client.getEosWallet $- fixture ^. wallet . walletId
        verify getFoWithEos
            [ expectWalletError (WalletNotFound)
            -- expectWalletError (UnknownError "ErrorCall")
            ]

    scenario "EOSWALLETS_DETAILS_01 - cannot get EOS wallet with FO endpoint" $ do
        fixture  <- setup defaultSetup
        eowallet <- successfulRequest $ Client.postEosWallet $- NewEosWallet
            (NE.fromList $ take 3 $ fixture ^. externallyOwnedAccounts)
            noAddressPoolGap
            defaultAssuranceLevel
            defaultWalletName

        getEosWithFo <- request $ Client.getWallet $- eowallet ^. walletId
        verify getEosWithFo
            [ expectWalletError (WalletNotFound)
            ]

    scenario "EOSWALLETS_LIST_01 - cannot get EOS wallets with FO endpoint" $ do
        fixture  <- setup defaultSetup
        _ <- successfulRequest $ Client.postEosWallet $- NewEosWallet
            (NE.fromList $ take 3 $ fixture ^. externallyOwnedAccounts)
            noAddressPoolGap
            defaultAssuranceLevel
            defaultWalletName

        getFO <- request $ Client.getWallets
        verify getFO
            [ expectSuccess
            , expectListSizeEqual 1
            , expectListItemFieldEqual 0 walletId (fixture ^. wallet . walletId)
            ]

    scenario "EOSWALLETS_LIST_01 - cannot get FO wallets with EOS endpoint" $ do
        fixture  <- setup defaultSetup
        eowallet <- successfulRequest $ Client.postEosWallet $- NewEosWallet
            (NE.fromList $ take 3 $ fixture ^. externallyOwnedAccounts)
            noAddressPoolGap
            defaultAssuranceLevel
            defaultWalletName

        getFO <- request $ Client.getEosWalletIndexFilterSorts
            $- Nothing
            $- Nothing
            $- NoFilters
            $- NoSorts
        verify getFO
            [ expectSuccess
            , expectListSizeEqual 1
            , expectListItemFieldEqual 0 walletId (eowallet ^. walletId)
            ]

    scenario "EOSWALLETS_UPDATE_01 - cannot update FO wallets with EOS endpoint" $ do
        fixture  <- setup $ defaultSetup
            & walletName .~ "Before update FO"
            & assuranceLevel .~ NormalAssurance

        let endpoint = "api/v1/wallets/externally-owned/" <> fromWalletId (fixture ^. wallet . walletId)
        updResp <- unsafeRequest ("PUT", endpoint) $ Just $ [json|{
                "assuranceLevel": "strict",
                "addressPoolGap": 12,
                "name": "My EosWallet2"
                }|]
        verify (updResp :: Either ClientError EosWallet)
            [ expectWalletError (UnknownError "UpdateEosWalletError")
            ]

        response <- request $ Client.getWallet $- fixture ^. wallet . walletId
        verify response
            [ expectFieldEqual walletId (fixture ^. wallet . walletId)
            , expectFieldEqual assuranceLevel NormalAssurance
            , expectFieldEqual walletName "Before update"
            ]

    scenario "EOSWALLETS_UPDATE_01 - cannot update EOS wallets with FO endpoint" $ do
        fixture  <- setup defaultSetup
        eowallet <- successfulRequest $ Client.postEosWallet $- NewEosWallet
            (NE.fromList $ take 3 $ fixture ^. externallyOwnedAccounts)
            noAddressPoolGap
            NormalAssurance
            "Before update EOS"

        let endpoint = "api/v1/wallets/" <> fromWalletId (eowallet ^. walletId)
        updResp <- unsafeRequest ("PUT", endpoint) $ Just $ [json|{
                "assuranceLevel": "strict",
                "name": "My EosWallet"
                }|]
        verify (updResp :: Either ClientError EosWallet)
            [ expectError
            ]

        response <- request $ Client.getEosWallet $- eowallet ^. walletId
        verify response
            [ expectFieldEqual walletId (eowallet ^. walletId)
            , expectFieldEqual assuranceLevel NormalAssurance
            , expectFieldEqual walletName "Before update EOS"
            , expectFieldEqual addressPoolGap defaultAddressPoolGap
            ]
    where
        fromWalletId :: Client.WalletId -> Text
        fromWalletId (Client.WalletId a) = a
