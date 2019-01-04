## Easy client functions and CLI for Cardano Wallet API

This provides more util functions for setting up a `WalletClient` so
that you can quickly make API calls from `ghci`.

There is also a CLI for use in test scripts.

### How to use

Start a wallet using `stack exec cardano-node -- ....`

In another terminal, fire up `ghci`.

    stack ghci cardano-wallet-tool:lib
    λ> import Cardano.Wallet.Client.Easy
    λ> cfg = normalConnectConfig "./state-wallet-testnet" 8090
    λ> wc <- walletClientFromConfig cfg
    λ> getWallets wc

### How to use CLI

For a list of options, run:

    cardano-wallet-tool --help

An example usage would be:

    cardano-wallet-tool --state-dir ./state-wallet-testnet localhost:8090 wait-for-sync


### Future work

 - implement commands for the entire wallet API
 - reduce boilerplate
 - add a `--curl` option which doesn't do anything except print an
   equivalent `curl` command-line.
 - provide option for human-readable output instead of formatted JSON
 - progress bar for sync/restore tasks if run in a terminal
 - use pagination features of the API
 
