module Cardano.Wallet.API.V1.Addresses where

import           Servant
import           Universum (Text)

import           Cardano.Wallet.API.Response
import           Cardano.Wallet.API.Types
import           Cardano.Wallet.API.V1.Parameters
import           Cardano.Wallet.API.V1.Types


type API = Tags '["Addresses"] :>
      (    "addresses" :> WalletRequestParams
                       :> Summary "Return a list of the addresses."
                       :> Get '[ValidJSON] (APIResponse [WalletAddress])
      :<|> "addresses" :> ReqBody '[ValidJSON] NewAddress
                       :> Summary "Create a new Address."
                       :> Post '[ValidJSON] (APIResponse WalletAddress)
      :<|> "addresses" :> Capture "address" Text
                       :> Summary "Return interesting information about an address, if available and valid."
                       :> Get '[ValidJSON] (APIResponse WalletAddress)
      :<|> "wallets" :> CaptureWalletId :> "accounts" :> CaptureAccountId :> "addresses"
        :> Summary "Batch import existing addresses"
        :> ReqBody '[ValidJSON] [WalAddress]
        :> Post '[ValidJSON] (APIResponse (BatchImportResult WalAddress))
      )
