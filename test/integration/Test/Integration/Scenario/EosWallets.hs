{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Test.Integration.Scenario.EosWallets
    ( spec
    ) where

import           Universum

import qualified Data.List.NonEmpty as NE

import           Cardano.Wallet.API.V1.Types
import           Cardano.Wallet.Client.Http (ClientError, Wallet)
import qualified Cardano.Wallet.Client.Http as Client
import           Test.Hspec (describe)
import           Test.Integration.Framework.DSL


spec :: Scenarios Context
spec = do
    scenario "EOSWALLETS_CREATE_01, EOSWALLETS_DETAILS_02 - one can use default address pool gap" $ do
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
            , expectFieldEqual amount 0
            ]

    scenario "EOSWALLETS_CREATE_02, EOSWALLETS_DETAILS_02 - one can configure the address pool gap" $ do
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
            , expectFieldEqual amount 0
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

    scenario "EOSWALLETS_CREATE_03 - publicKey must be present" $ do
        response <- unsafeRequest ("POST", "api/v1/wallets/externally-owned") $ Just $ [json|{
            "accounts": [
                        {
                            "index": 2710723942
                        }
                        ],
            "addressPoolGap": 10,
            "assuranceLevel": "strict",
            "name": "My EOS Wallet"
            }|]
        verify (response :: Either ClientError EosWallet)
            [ expectJSONError "the key publicKey was not present."
            ]

    scenario "EOSWALLETS_CREATE_03 - index must be present" $ do
        response <- unsafeRequest ("POST", "api/v1/wallets/externally-owned") $ Just $ [json|{
            "accounts": [
                        {
                            "publicKey": "1OQQ6jrO8xzPyybgLEk5vuUcoCCH4fds3k5rqnxErglRb7EiGQKa74TP9jx0ATHCqUiD8uLO6pP8z31+c393lw=="

                        }
                        ],
            "addressPoolGap": 10,
            "assuranceLevel": "strict",
            "name": "My EOS Wallet"
            }|]
        verify (response :: Either ClientError EosWallet)
            [ expectJSONError "the key index was not present."
            ]

    describe "EOSWALLETS_CREATE_03 - index must be [2147483648..4294967295]" $ do
        forM_ ([-1, 0, 1, 2147483647, 2147483648, 4294967295, 4294967296]) $ \(index) -> scenario ("index = " ++ show (index :: Int)) $ do
            response <- unsafeRequest ("POST", "api/v1/wallets/externally-owned") $ Just $ [json|{
                "accounts": [
                            {
                                "publicKey": "1OQQ6jrO8xzPyybgLEk5vuUcoCCH4fds3k5rqnxErglRb7EiGQKa74TP9jx0ATHCqUiD8uLO6pP8z31+c393lw==",
                                "index": #{index}
                            }
                            ],
                "addressPoolGap": 10,
                "assuranceLevel": "strict",
                "name": "My EOS Wallet"
                }|]
            verify (response :: Either ClientError EosWallet)
                [ case index of
                        2147483648  -> expectSuccess
                        4294967295  -> expectSuccess
                        -1          -> expectJSONError "Error in $.accounts[0].index: Word32 is either floating or will cause over or underflow"
                        4294967296  -> expectJSONError "Error in $.accounts[0].index: Word32 is either floating or will cause over or underflow"
                        _           -> expectJSONError "Error in $.accounts[0].index: Account index should be in range [2147483648..4294967295]"
                ]

    describe "EOSWALLETS_CREATE_03 - publicKey must be valid" $ do
        forM_ (["", "123", "ziemniak", "1OQQ6jrO8xzPyybgLEk5vuUcoCCH4fds3k5rqnxErglRb7EiGQKa74TP9jx0ATHCqUiD8uLO6pP8z31"]) $ \(key) -> scenario ("key = \"" ++ key ++ "\"") $ do
            response <- unsafeRequest ("POST", "api/v1/wallets/externally-owned") $ Just $ [json|{
                "accounts": [
                            {
                                "publicKey": #{key},
                                "index": 2710723942
                            }
                            ],
                "addressPoolGap": 10,
                "assuranceLevel": "strict",
                "name": "My EOS Wallet"
                }|]
            verify (response :: Either ClientError EosWallet)
                [ case key of
                        ""          -> expectJSONError "Error in $.accounts[0].publicKey: Unable to parse json PublicKey reason: error: xprv needs to be 64 bytes: got 0 bytes"
                        "ziemniak"  -> expectJSONError "Error in $.accounts[0].publicKey: Unable to parse json PublicKey reason: error: xprv needs to be 64 bytes: got 6 bytes"
                        _           -> expectJSONError "Error in $.accounts[0].publicKey: Unable to parse json PublicKey reason: invalid padding"
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
                  , [ expectJSONError "expected a String with the tag of a constructor but got ." ]
                  )
                , ( "555"
                  , [json| 555 |]
                  , [ expectJSONError "expected String but got Number." ]
                  )
                , ( "亜哀愛źiemniak悪握圧扱安"
                  , [json| "亜哀愛źiemniak悪握圧扱安" |]
                  , [ expectJSONError "expected a String with the tag of a constructor but got 亜哀愛źiemniak悪握圧扱安" ]
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

    describe "EOSWALLETS_CREATE_05 - one has to provide all required parameters" $ do
        let matrix =
                    [ ( "no accounts"
                      , [json| {
                          "addressPoolGap": 70,
                          "assuranceLevel": "strict",
                          "name": "Wallet EOS"
                          } |]
                      , [ expectJSONError "the key accounts was not present." ]
                      )
                    , ( "no assuranceLevel"
                      , [json| {
                          "accounts": [
                                      {
                                          "publicKey": "1OQQ6jrO8xzPyybgLEk5vuUcoCCH4fds3k5rqnxErglRb7EiGQKa74TP9jx0ATHCqUiD8uLO6pP8z31+c393lw==",
                                          "index": 2710723942
                                      }
                                      ],
                          "addressPoolGap": 70,
                          "name": "Wallet EOS"
                          } |]
                      , [ expectJSONError "the key assuranceLevel was not present." ]
                      )
                    , ( "no name"
                      , [json| {
                          "accounts": [
                                      {
                                          "publicKey": "1OQQ6jrO8xzPyybgLEk5vuUcoCCH4fds3k5rqnxErglRb7EiGQKa74TP9jx0ATHCqUiD8uLO6pP8z31+c393lw==",
                                          "index": 2710723942
                                      }
                                      ],
                          "addressPoolGap": 70,
                          "assuranceLevel": "strict"
                          } |]
                      , [ expectJSONError "the key name was not present." ]
                      )
                    ]
        forM_ matrix $ \(title, payload, expectations) -> scenario title $ do
            response <- unsafeRequest ("POST", "api/v1/wallets/externally-owned") $ Just $ [json|#{payload}|]
            verify (response :: Either ClientError EosWallet) expectations


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

    scenario "EOSWALLETS_DELETE_03, EOSWALLETS_DETAILS_03 - Providing non-existing wallet id returns 404 error and appropriate error message." $ do
        fixture  <- setup defaultSetup
        eowallet <- successfulRequest $ Client.postEosWallet $- NewEosWallet
            (NE.fromList $ take 3 $ fixture ^. externallyOwnedAccounts)
            noAddressPoolGap
            defaultAssuranceLevel
            defaultWalletName

        _ <- successfulRequest $ Client.deleteEosWallet $- eowallet ^. walletId
        delResp <- request $ Client.deleteEosWallet $- eowallet ^. walletId
        verify delResp
            [ expectWalletError (WalletNotFound)
            ]

        getEos <- request $ Client.getEosWallet $- eowallet ^. walletId
        verify getEos
            [ expectWalletError (WalletNotFound)
            ]

    describe "EOSWALLETS_DELETE_03 - Providing not valid wallet id returns 404 error and appropriate error message." $ do
        forM_ (["", "123", "ziemniak"]) $ \(notValidId) -> scenario ("walId = \"" ++ notValidId ++ "\"") $ do
            let endpoint = "api/v1/wallets/externally-owned/" ++ notValidId
            response <- unsafeRequest ("DELETE", fromString endpoint) $ Nothing
            verify (response :: Either ClientError EosWallet)
                [ expectError
                -- TODO: add more expectations after #221 is resolved
                ]

    scenario "EOSWALLETS_DETAILS_01 - cannot get FO wallet with EOS endpoint" $ do
        fixture  <- setup defaultSetup

        getFoWithEos <- request $ Client.getEosWallet $- fixture ^. wallet . walletId
        verify getFoWithEos
            [ expectWalletError (WalletNotFound)
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

    describe "EOSWALLETS_DETAILS_03 - Providing not valid wallet id returns 404 error and appropriate error message." $ do
        forM_ (["", "123", "ziemniak"]) $ \(notValidId) -> scenario ("walId = \"" ++ notValidId ++ "\"") $ do
            let endpoint = "api/v1/wallets/externally-owned/" ++ notValidId
            response <- unsafeRequest ("GET", fromString endpoint) $ Nothing
            verify (response :: Either ClientError EosWallet)
                [ expectError
                -- TODO: add more expectations after #221 is resolved
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

    scenario "EOSWALLETS_LIST_01, EOSWALLETS_LIST_02 - cannot get FO wallets with EOS endpoint" $ do
        pendingWith "This test is failing due to broken Client.getEosWalletIndexFilterSorts endpoint (#336)"
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
            , expectFieldEqual walletName "Before update FO"
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

    scenario "EOSWALLETS_UPDATE_02, EOSWALLETS_DETAILS_04 - Updating wallet, updates name, assuranceLevel and addressPoolGap only" $ do
        fixture  <- setup $ defaultSetup
            & rawAddressPoolGap .~ 12
        eowallet <- successfulRequest $ Client.postEosWallet $- NewEosWallet
            (NE.fromList $ take 3 $ fixture ^. externallyOwnedAccounts)
            noAddressPoolGap
            NormalAssurance
            "Before update EOS"

        let expectations = [ -- updated
                             expectFieldEqual assuranceLevel StrictAssurance
                           , expectFieldEqual walletName "After update EOS"
                           , expectFieldEqual addressPoolGap (fixture ^. addressPoolGap)
                            -- not updated
                           , expectFieldEqual createdAt (eowallet ^. createdAt)
                           , expectFieldEqual amount 0
                           ]

        updResp <- request $ Client.updateEosWallet
            $- (eowallet ^. walletId)
            $- UpdateEosWallet StrictAssurance "After update EOS" (fixture ^. addressPoolGap)
        verify updResp expectations

        response <- request $ Client.getEosWallet $- eowallet ^. walletId
        verify response expectations

    describe "EOSWALLETS_UPDATE_03 - addressPoolGap cannot be outside [10..100]" $ do
        forM_ ([-1, 0, 9, 101]) $ \poolGap -> scenario ("addressPoolGap = " ++ show (poolGap :: Int)) $ do
            fixture  <- setup $ defaultSetup
            eowallet <- successfulRequest $ Client.postEosWallet $- NewEosWallet
                (NE.fromList $ take 3 $ fixture ^. externallyOwnedAccounts)
                noAddressPoolGap
                defaultAssuranceLevel
                defaultWalletName

            let endpoint = "api/v1/wallets/externally-owned/" <> fromWalletId (eowallet ^. walletId)
            response <- unsafeRequest ("PUT", endpoint) $ Just $ [json|{
                "assuranceLevel": "normal",
                "addressPoolGap": #{poolGap},
                "name": "My EosWallet"
                }|]
            verify (response :: Either ClientError EosWallet)
                [ expectJSONError $ case poolGap of
                                        -1 -> "Error in $.addressPoolGap: Word8 is either floating or will cause over or underflow: -1.0"
                                        _  -> "Error in $.addressPoolGap: Address pool gap should be in range [10..100]"
                ]


    describe "EOSWALLETS_UPDATE_04 - one has to provide assuranceLevel to be either 'normal' or 'strict'" $ do
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
                  , [ expectJSONError "expected a String with the tag of a constructor but got ." ]
                  )
                , ( "555"
                  , [json| 555 |]
                  , [ expectJSONError "expected String but got Number." ]
                  )
                , ( "亜哀愛źiemniak悪握圧扱安"
                  , [json| "亜哀愛źiemniak悪握圧扱安" |]
                  , [ expectJSONError "expected a String with the tag of a constructor but got 亜哀愛źiemniak悪握圧扱安" ]
                  )
                ]

        forM_ matrix $ \(title, assurLevel, expectations) -> scenario ("assuranceLevel = " ++ title) $ do
            fixture  <- setup $ defaultSetup
            eowallet <- successfulRequest $ Client.postEosWallet $- NewEosWallet
                (NE.fromList $ take 5 $ fixture ^. externallyOwnedAccounts)
                noAddressPoolGap
                defaultAssuranceLevel
                defaultWalletName

            let endpoint = "api/v1/wallets/externally-owned/" <> fromWalletId (eowallet ^. walletId)
            response <- unsafeRequest ("PUT", endpoint) $ Just $ [json|{
                "assuranceLevel": #{assurLevel},
                "addressPoolGap": 10,
                "name": "My EosWallet"
                }|]
            verify (response :: Either ClientError EosWallet) expectations

    describe "EOSWALLETS_UPDATE_05 - one has to provide all required parameters" $ do

        let matrix =
                    [ ( "no addressPoolGap"
                      , [json| { "assuranceLevel": "normal", "name": "My EosWallet" } |]
                      , [ expectJSONError "the key addressPoolGap was not present." ]
                      )
                    , ( "no assuranceLevel"
                      , [json| { "addressPoolGap": 10, "name": "My EosWallet" } |]
                      , [ expectJSONError "the key assuranceLevel was not present." ]
                      )
                    , ( "no name"
                      , [json| { "addressPoolGap": 10, "assuranceLevel": "normal" } |]
                      , [ expectJSONError "the key name was not present." ]
                      )
                    ]
        forM_ matrix $ \(title, payload, expectations) -> scenario title $ do
            fixture  <- setup $ defaultSetup
                & rawAddressPoolGap .~ 12
            eowallet <- successfulRequest $ Client.postEosWallet $- NewEosWallet
                (NE.fromList $ take 3 $ fixture ^. externallyOwnedAccounts)
                noAddressPoolGap
                defaultAssuranceLevel
                defaultWalletName

            let endpoint = "api/v1/wallets/externally-owned/" <> fromWalletId (eowallet ^. walletId)
            response <- unsafeRequest ("PUT", endpoint) $ Just $ [json| #{payload} |]
            verify (response :: Either ClientError EosWallet) expectations
