# Decoupling cardano wallet

**Executive summary**
*The minimum steps needed to run separate process downloading blocks from Byron era cardano node*


In the current prototype it is shown that bare networking node instance is needed in order to request current tip of the block and download corresponding block.
All Byron specific configuration required to connect to the node are located in **ByronSpecifics.hs**.
In order to build the process run :

``` sh
$ cd cardano-wallet
$ stack build cardano-wallet:decoupled-wallet
```

Then run cluster in another console via

``` sh
$ cd cardano-sl
$ stack test cardano-wallet:test:integration
```

The process instantiate at *127.0.0.1:3005* and tries to talk to *127.0.0.1:3001:0* (which is one of the cluster's node)
In order to instantiate the process run :
``` sh
$ cd cardano-wallet
$ stack exec -- decoupled-wallet
```
