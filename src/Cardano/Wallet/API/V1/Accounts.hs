module Cardano.Wallet.API.V1.Accounts where

import           Servant

import           Cardano.Wallet.API.Request
import           Cardano.Wallet.API.Response
import           Cardano.Wallet.API.Types
import           Cardano.Wallet.API.V1.Parameters
import           Cardano.Wallet.API.V1.Types


type API
    = Tag "Accounts" 'NoTagDescription :>
    (    "wallets" :> CaptureWalletId :> "accounts"
          :> CaptureAccountId
          :> Summary "Delete an Account."
          :> DeleteNoContent '[ValidJSON] NoContent
    :<|> "wallets" :> CaptureWalletId :> "accounts"
          :> CaptureAccountId
          :> Summary "Retrieve a specific Account."
          :> Get '[ValidJSON] (APIResponse Account)
    :<|> "wallets" :> CaptureWalletId :> "accounts"
          :> WalletRequestParams
          :> Summary "Retrieve the full list of Accounts."
          :> Get '[ValidJSON] (APIResponse [Account])
    :<|> "wallets" :> CaptureWalletId :> "accounts"
          :> Summary "Create a new Account for the given Wallet."
          :> ReqBody '[ValidJSON] (New Account)
          :> Post '[ValidJSON] (APIResponse Account)
    :<|> "wallets" :> CaptureWalletId :> "accounts"
          :> CaptureAccountId
          :> Summary "Update an Account for the given Wallet."
          :> ReqBody '[ValidJSON] (Update Account)
          :> Put '[ValidJSON] (APIResponse Account)
    :<|> "wallets" :> CaptureWalletId :> "accounts"
          :> CaptureAccountId :> "addresses"
          :> Summary "Retrieve only account's addresses."
          :> WalletRequestParams
          :> FilterBy '[WalAddress] WalletAddress
          :> Get '[ValidJSON] (APIResponse AccountAddresses)
    :<|> "wallets" :> CaptureWalletId :> "accounts"
          :> CaptureAccountId :> "amount"
          :> Summary "Retrieve only account's balance."
          :> Get '[ValidJSON] (APIResponse AccountBalance)
    )
