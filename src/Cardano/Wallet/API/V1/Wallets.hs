module Cardano.Wallet.API.V1.Wallets where

import           Cardano.Wallet.API.Request
import           Cardano.Wallet.API.Response
import           Cardano.Wallet.API.Types
import           Cardano.Wallet.API.V1.Parameters
import           Cardano.Wallet.API.V1.Types
import           Pos.Core as Core

import           Servant

type FullyOwnedAPI = Tags '["Wallets"] :>
    (    "wallets" :> Summary "Create a new Wallet or restore an existing one."
                   :> ReqBody '[ValidJSON] (New Wallet)
                   :> PostCreated '[ValidJSON] (APIResponse Wallet)
    :<|> "wallets" :> Summary "Return a list of the available wallets."
                   :> WalletRequestParams
                   :> FilterBy '[ WalletId
                                , Core.Coin
                                ] Wallet
                   :> SortBy   '[ Core.Coin
                                , WalletTimestamp
                                ] Wallet
                   :> Get '[ValidJSON] (APIResponse [Wallet])
    :<|> "wallets" :> CaptureWalletId
                   :> "password"
                   :> Summary "Update the password for the given Wallet."
                   :> ReqBody '[ValidJSON] PasswordUpdate
                   :> Put '[ValidJSON] (APIResponse Wallet)
    :<|> "wallets" :> CaptureWalletId
                   :> Summary "Delete the given Wallet and all its accounts."
                   :> DeleteNoContent '[ValidJSON] NoContent
    :<|> "wallets" :> CaptureWalletId
                   :> Summary "Return the Wallet identified by the given walletId."
                   :> Get '[ValidJSON] (APIResponse Wallet)
    :<|> "wallets" :> CaptureWalletId
                   :> Summary "Update the Wallet identified by the given walletId."
                   :> ReqBody '[ValidJSON] (Update Wallet)
                   :> Put '[ValidJSON] (APIResponse Wallet)
    :<|> "wallets" :> CaptureWalletId :> "statistics" :> "utxos"
                   :> Summary "Return UTxO statistics for the Wallet identified by the given walletId."
                   :> Get '[ValidJSON] (APIResponse UtxoStatistics)
    )

type ExternallyOwnedAPI = Tags '["Externally Owned Wallets"]
    :> ( "wallets"
        :> "externally-owned"
        :> Summary "Create a new Wallet or restore an existing one."
        :> ReqBody '[ValidJSON] (NewEosWallet)
        :> PostCreated '[ValidJSON] (APIResponse EosWallet)

    :<|> "wallets"
        :> "externally-owned"
        :> Summary "Return the Wallet identified by the given walletId."
        :> CaptureWalletId
        :> Get '[ValidJSON] (APIResponse EosWallet)

    :<|> "wallets"
        :> "externally-owned"
        :> Summary "Update the Wallet identified by the given walletId."
        :> CaptureWalletId
        :> ReqBody '[ValidJSON] (UpdateEosWallet)
        :> Put '[ValidJSON] (APIResponse EosWallet)

    :<|> "wallets"
        :> "externally-owned"
        :> Summary "Delete the given Wallet and all its accounts."
        :> CaptureWalletId
        :> DeleteNoContent '[ValidJSON] NoContent

    :<|> "wallets"
        :> "externally-owned"
        :> Summary "Return a list of the available wallets."
        :> WalletRequestParams
        :> FilterBy
           '[ WalletId
            , Core.Coin
            ] EosWallet
        :> SortBy
           '[ Core.Coin
            , WalletTimestamp
            ] EosWallet
        :> Get '[ValidJSON] (APIResponse [EosWallet])

    )
