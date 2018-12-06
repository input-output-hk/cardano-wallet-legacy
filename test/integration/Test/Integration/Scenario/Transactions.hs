module Test.Integration.Scenario.Transactions
    ( spec
    ) where

import           Universum

import qualified Cardano.Wallet.Client.Http as Client
import           Test.Integration.Framework.DSL


spec :: Scenarios Context
spec = do
    scenario "successful payment appears in the history" $ do
        fixture <- setup $ defaultSetup
            & initialCoins .~ [100]

        response <- request $ Client.postTransaction $- Payment
            (defaultSource fixture)
            (defaultDistribution 14 fixture)
            defaultGroupingPolicy
            noSpendingPassword

        verify response
            [ expectTxInHistoryOf (fixture ^. wallet)
            , expectTxStatusEventually [Creating, Applying, InNewestBlocks, Persisted]
            ]

    scenario "estimate fees of well-formed transaction returns" $ do
        fixture <- setup $ defaultSetup
            & initialCoins .~ [100]

        response <- request $ Client.getTransactionFee $- Payment
            (defaultSource fixture)
            (defaultDistribution 14 fixture)
            defaultGroupingPolicy
            noSpendingPassword

        verify response
            [ expectSuccess
            ]

    scenario "payment fails when wallet has no funds" $ do
        fixture <- setup $ defaultSetup

        response <- request $ Client.postTransaction $- Payment
            (defaultSource fixture)
            (defaultDistribution 14 fixture)
            defaultGroupingPolicy
            noSpendingPassword

        verify response
            [ expectWalletError (NotEnoughMoney (ErrAvailableBalanceIsInsufficient 0))
            ]

    scenario "payment fails when wallet has insufficient funds" $ do
        fixture <- setup $ defaultSetup
            & initialCoins .~ [14]

        response <- request $ Client.postTransaction $- Payment
            (defaultSource fixture)
            (defaultDistribution 42 fixture)
            defaultGroupingPolicy
            noSpendingPassword

        verify response
            [ expectWalletError (NotEnoughMoney (ErrAvailableBalanceIsInsufficient 42))
            ]

    scenario "payment fails when wallet cannot cover for fee" $ do
        fixture <- setup $ defaultSetup
            & initialCoins .~ [42]

        response <- request $ Client.postTransaction $- Payment
            (defaultSource fixture)
            (defaultDistribution 42 fixture)
            defaultGroupingPolicy
            noSpendingPassword

        verify response
            [ expectWalletError (NotEnoughMoney ErrCannotCoverFee)
            ]

    scenario "balance is increased when valid key is redemeed" $ do
        fixture <- setup defaultSetup

        sequence_
            [ do -- // 1 Redeem a certificate
                response <- request $ Client.redeemAda $- Redemption
                    (ShieldedRedemptionCode "QBYOctbb6fJT/dBDLwg4je+SAvEzEhRxA7wpLdEFhnY=")
                    noRedemptionMnemonic
                    defaultSpendingPassword
                    (fixture ^. walletId)
                    defaultAccountId

                verify response
                    [ expectTxStatusEventually [InNewestBlocks, Persisted]
                    ]

            , do -- // 2 Control The Balance
                response <- request $ Client.getAccountBalance
                    $- (fixture ^. walletId)
                    $- defaultAccountId

                verify response
                    [ expectFieldEqual amount 100000
                    ]

            , do -- // 3 Try Redeeming again
                response <- request $ Client.redeemAda $- Redemption
                    (ShieldedRedemptionCode "QBYOctbb6fJT/dBDLwg4je+SAvEzEhRxA7wpLdEFhnY=")
                    noRedemptionMnemonic
                    defaultSpendingPassword
                    (fixture ^. walletId)
                    defaultAccountId

                verify response
                    [ expectWalletError TxRedemptionDepleted
                    ]
            ]

    -- NOTE:
    -- Cases where we have to increase the number of change outputs are hard
    -- to test in practice. We either need:
    --
    -- - A BIG change to cause an overflow (but even with all the genesis
    --   wallets, we don't have enough funds)
    --
    -- - A selection that will have no change such that a new one will be
    --   created for the change. However, the coin selection tends to always
    --   generate a change output.

    -- Initial Selection:      Final Selection:
    --   inputs : [200000]       inputs : [200000]
    --   outputs: [1]            outputs: [1]
    --   changes: [199999]       changes: [28094]
    --   fee+   : 171905         fee+   : 171817
    --
    --           Actual fee: 171905 (+88)
    coinSelectionScenario "no extra inputs, no extra change" [200000] 1

    -- Initial Selection:      Final Selection:
    --   inputs : [171906]       inputs : [171906]
    --   outputs: [1]            outputs: [1]
    --   changes: [171905]       changes: []
    --   fee+   : 171905         fee+   : 167862
    --
    --           Actual fee: 167862 (+4043)
    coinSelectionScenario "empties a wallet" [171906] 1

    -- Initial Selection:      Final Selection:
    --   inputs : [100000]       inputs : [100000, 100000]
    --   outputs: [1]            outputs: [1]
    --   changes: [99999]        changes: [19964]
    --   fee+   : 171905         fee+   : 179947
    --
    --           Actual fee: 180035 (+88)
    coinSelectionScenario "needs one extra input" [10000, 10000] 1

    -- Initial Selection:      Final Selection:
    --   inputs : [30000]        inputs : [30000, 30000, 30000, 30000,
    --                                     30000, 30000, 30000, 30000]
    --   outputs: [42]           outputs: [42]
    --   changes: [29958]        changes: [11055]
    --   fee+   : 171905         fee+   : 228815
    --
    --           Actual fee: 228903 (+88)
    coinSelectionScenario "needs many extra inputs" (replicate 8 30000) 42
  where
    coinSelectionScenario :: String -> [Word64] -> Word64 -> Scenarios Context
    coinSelectionScenario title coins amt =
        scenario ("coin selection: " <> title) $ do
            fixture <- setup $ defaultSetup
                & initialCoins .~ coins

            response <- request $ Client.postTransaction $- Payment
                (defaultSource fixture)
                (defaultDistribution amt fixture)
                defaultGroupingPolicy
                noSpendingPassword

            verify response
                [ expectTxStatusEventually [InNewestBlocks, Persisted]
                ]
