module Test.Integration.Scenario.Addresses
    ( spec
    ) where

import           Universum

import           Cardano.Wallet.API.V1.Types (Account (accAddresses),
                     WalletAddress (..))
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
        print accountResp
        addrs <- sequence $ [mkAddress (fixture ^. backupPhrase) (Client.accIndex accountResp) 1]

        response <- request $ Client.importAddresses
            $- fixture ^. wallet . walletId
            $- defaultAccountId
            $- addrs
        verify response
            [ expectFieldEqual totalSuccess 1
            ]
