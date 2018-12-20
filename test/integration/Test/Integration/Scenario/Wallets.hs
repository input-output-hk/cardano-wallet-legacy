module Test.Integration.Scenario.Wallets
    ( spec
    ) where

import           Universum

import           Cardano.Wallet.Client.Http (ClientError, Wallet)
import qualified Cardano.Wallet.Client.Http as Client
import           Test.Integration.Framework.DSL

spec :: Scenarios Context
spec = do
    scenario "demo unsafeRequest" $ do
        fixture <- setup $ defaultSetup

        let endpoint = "api" </> "v1" </> "wallets" </> fixture ^. wallet . walletId
        response <- unsafeRequest ("PUT", endpoint) $ Just $ [json|{
            "invalidField": #{fixture ^. wallet}
        }|]

        verify (response :: Either ClientError Wallet)
            [ expectJSONError "key assuranceLevel was not present"
            ]

    scenario "wallet is available after it's been created" $ do
        fixture <- setup $ defaultSetup
            & walletName .~ "漢patate字"
            & assuranceLevel .~ StrictAssurance

        response <- request $ Client.getWallet
            $- (fixture ^. wallet . walletId)

        verify response
            [ expectFieldEqual walletName "漢patate字"
            , expectFieldEqual assuranceLevel StrictAssurance
            ]

    scenario "updating a wallet persists the update" $ do
        fixture <- setup defaultSetup

        [_, response] <- sequence
            [ request $ Client.updateWallet
                $- (fixture ^. wallet . walletId)
                $- WalletUpdate StrictAssurance "漢patate字"
            , request $ Client.getWallet
                $- (fixture ^. wallet . walletId)
            ]

        verify response
            [ expectFieldEqual walletName "漢patate字"
            , expectFieldEqual assuranceLevel StrictAssurance
            ]

    scenario "UTxO statistics reflect wallet's inactivity" $ do
        fixture <- setup defaultSetup

        response <- request $ Client.getUtxoStatistics
            $- (fixture ^. wallet . walletId)

        verify response
            [ expectWalletUTxO []
            ]

    scenario "UTxO statistics reflect wallet's activity" $ do
        fixture <- setup $ defaultSetup
            & initialCoins .~ [14, 42, 1337]

        response <- request $ Client.getUtxoStatistics
            $- (fixture ^. wallet . walletId)

        verify response
            [ expectWalletUTxO [14, 42, 1337]
            ]

    scenario "can't create wallet if backupPhrase is already known" $ do
        fixture <- setup $ defaultSetup
            & mnemonicWords .~ testBackupPhrase

        response <- request $ Client.postWallet $- NewWallet
            (fixture ^. backupPhrase)
            noSpendingPassword
            defaultAssuranceLevel
            defaultWalletName
            CreateWallet

        verify response
            [ expectWalletError (WalletAlreadyExists (fixture ^. wallet . walletId))
            ]

    scenario "can't restore wallet if backupPhrase is already known" $ do
        fixture <- setup $ defaultSetup
            & mnemonicWords .~ testBackupPhrase

        response <- request $ Client.postWallet $- NewWallet
            (fixture ^. backupPhrase)
            noSpendingPassword
            defaultAssuranceLevel
            defaultWalletName
            RestoreWallet

        verify response
            [ expectWalletError (WalletAlreadyExists (fixture ^. wallet . walletId))
            ]
  where
    testBackupPhrase :: [Text]
    testBackupPhrase =
        ["clap", "panda", "slim", "laundry", "more", "vintage", "cash", "shaft"
        , "token", "history", "misery", "problem"]
