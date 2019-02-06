module Cardano.Wallet.API.V1.Handlers (handlers) where

import           Servant
import           Universum

import qualified Cardano.Wallet.API.V1 as V1
import qualified Cardano.Wallet.API.V1.Handlers.Accounts as Accounts
import qualified Cardano.Wallet.API.V1.Handlers.Addresses as Addresses
import qualified Cardano.Wallet.API.V1.Handlers.Info as Info
import qualified Cardano.Wallet.API.V1.Handlers.Settings as Settings
import qualified Cardano.Wallet.API.V1.Handlers.Transactions as Transactions
import qualified Cardano.Wallet.API.V1.Handlers.Wallets as Wallets

import           Cardano.Wallet.NodeProxy (NodeHttpClient)
import           Cardano.Wallet.WalletLayer (ActiveWalletLayer,
                     walletPassiveLayer)


handlers :: NodeHttpClient -> ActiveWalletLayer IO -> Server V1.API
handlers nc aw =
    Addresses.handlers pw
    :<|> Wallets.fullyOwnedHandlers pw
    :<|> Wallets.externallyOwnedHandlers pw
    :<|> Accounts.handlers pw
    :<|> Transactions.handlers aw
    :<|> Settings.handlers nc
    :<|> Info.handlers nc
  where
    pw = walletPassiveLayer aw
