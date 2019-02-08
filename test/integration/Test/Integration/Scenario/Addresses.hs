module Test.Integration.Scenario.Addresses
    ( spec
    ) where

import           Universum

import           Cardano.Wallet.API.V1.Types (Account (accAddresses),
                     WalletAddress (..))
import           Cardano.Wallet.Client.Http (BatchImportResult, ClientError)
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

    scenario "ADDRESSES_IMPORT_01 - Unused addresses previously created on a wallet's account, can be imported" $ do
        fixture <- setup defaultSetup
        addr <- successfulRequest $ Client.postAddress $- NewAddress
            Nothing
            defaultAccountId
            (fixture ^. wallet . walletId)
        void $ successfulRequest $ Client.deleteWallet $- (fixture ^. wallet . walletId)
        void $ successfulRequest $ Client.postWallet $- NewWallet
            (fixture ^. backupPhrase)
            noSpendingPassword
            NormalAssurance
            defaultWalletName
            RestoreWallet

        response <- request $ Client.importAddresses
            $- fixture ^. wallet . walletId
            $- [view address addr]

        verify response
            [ expectFieldEqual totalSuccess 1
            , expectFieldEqual failures []
            , \_ -> expectAddressInIndexOf (Right addr)
            ]

    scenario "ADDRESSES_IMPORT_02 - Can't import addresses that aren't ours" $ do
        (ourFixture, theirFixture) <- (,) <$> setup defaultSetup <*> setup defaultSetup
        addrs <- sequence $
            [ mkAddress (ourFixture   ^. backupPhrase) defaultAccountId 14
            , mkAddress (theirFixture ^. backupPhrase) defaultAccountId 1
            , mkAddress (theirFixture ^. backupPhrase) defaultAccountId 2
            , mkAddress (theirFixture ^. backupPhrase) defaultAccountId 3
            ]

        response <- request $ Client.importAddresses
            $- ourFixture ^. wallet . walletId
            $- addrs
        index <- fmap (fmap accAddresses) $ request $ Client.getAccount
            $- ourFixture ^. wallet . walletId
            $- defaultAccountId
        verify response
            [ expectFieldEqual totalSuccess 1
            , expectFieldEqual failures (drop 1 addrs)
            ]
        verify index
            [ expectListSizeEqual 2 -- NOTE 2 because there's also a default address
            ]

    scenario "ADDRESSES_IMPORT_03 - Can't import addresses that are already present (used or unused)" $ do
        -- NOTE
        -- The fixture looks a bit complex here but in the end, we should end
        -- up with two addresses:
        --
        -- - 1 unused, default address of the account
        -- - 1 used, created by the fixture to receive the initial payment
        --
        -- We make sure of that by adding an extra 'verify'
        fixture <- setup $ defaultSetup
            & initialCoins .~ [1000000]
        addrs <- fmap accAddresses $ successfulRequest $ Client.getAccount
            $- fixture ^. wallet . walletId
            $- defaultAccountId
        verify addrs
            [ expectListSizeEqual 1 . Right . filter addrUsed
            , expectListSizeEqual 1 . Right . filter (not . addrUsed)
            ]

        response <- request $ Client.importAddresses
            $- fixture ^. wallet . walletId
            $- map (view address) addrs
        verify response
            [ expectFieldEqual totalSuccess 0
            , expectFieldEqual failures (map (view address) addrs)
            ]

    scenario "ADDRESSES_IMPORT_04 - Can import addresses from different account into default account of the wallet" $ do
        fixture <- setup $ defaultSetup

        accountResp <- successfulRequest $ Client.postAccount
            $- (fixture ^. wallet . walletId)
            $- NewAccount
                noSpendingPassword
                "New Account"

        addrs <- sequence $ [mkAddress (fixture ^. backupPhrase) (Client.accIndex accountResp) 1]

        response <- request $ Client.importAddresses
            $- fixture ^. wallet . walletId
            $- addrs
        verify response
            [ expectFieldEqual totalSuccess 1
            ]

    scenario "ADDRESSES_IMPORT_05 - Returns error when wallet id is invalid" $ do
        response <- unsafeRequest ("POST", "api/v1/wallets/aaa/addresses") $ Just $ [json|[
            "DdzFFzCqrhssoca9zmsbhqHxJRjrDyzR1wh4Rs9ffbFTiYkcnDsYU416MYe2A29BFigVPBQgnkQH64et6pAqSjAqPPFbHcG1zR7G6kGr"
        ]|]
        verify (response :: Either ClientError (BatchImportResult Text))
            [ expectWalletError (UnknownError "ImportAddressError")
            ]

    scenario "ADDRESSES_IMPORT_05 - Returns error when wallet id is valid but missing" $ do
        fixture <- setup $ defaultSetup
        successfulRequest $ Client.deleteWallet
            $- (fixture ^. wallet . walletId)

        addrs <- sequence $ [mkAddress (fixture ^. backupPhrase) defaultAccountId 1]

        response <- request $ Client.importAddresses
            $- fixture ^. wallet . walletId
            $- addrs
        verify response
            [ expectWalletError (UnknownError "ImportAddressError")
            ]

    scenario "ADDRESSES_IMPORT_06 - Returns error when address is invalid" $ do
        fixture <- setup $ defaultSetup

        let endpoint = "api/v1/wallets/" <> fromWalletId (fixture ^. wallet . walletId) <> ("/addresses" :: Text)
        response <- unsafeRequest ("POST", endpoint) $ Just $ [json|[
          "dasd",
          "1"
          ]|]
        verify (response :: Either ClientError (BatchImportResult Text))
            [ expectJSONError "Not a valid Cardano Address"
            ]

    scenario "ADDRESSES_IMPORT_06 - Returns error when body is invalid" $ do
        fixture <- setup $ defaultSetup

        let endpoint = "api/v1/wallets/" <> fromWalletId (fixture ^. wallet . walletId) <> ("/addresses" :: Text)
        response <- unsafeRequest ("POST", endpoint) $ Nothing
        verify (response :: Either ClientError (BatchImportResult Text))
            [ expectJSONError "Error in $: not enough input"
            ]
