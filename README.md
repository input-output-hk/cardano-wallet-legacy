# Wallet Decoupling

<p align="left">
  <br/>
  <blockquote>
    <p>
      <strong>Author</strong>: Matthias Benkort <br/>
      <strong>Date</strong>: 19 Oct. 2018 <br/>
      <strong>Version</strong>: 1 <br/>
    </p>
  </blockquote>
</p>

<p align="right" style="position: relative; top: -70px; margin-bottom: -70px;">
  <img src="IOHK_small.png" />
<p>

# Current State

## Cardano-SL Wallet

#### BListener

The node implements the `BListener` interface which boils down to two 
actions: apply a block and switch to a fork.


#### Node State Adaptor

Various pieces required by the node were historically not derived from blocks and from 
what it receives via the BListener. Most of them are gathered under the `NodeStateAdaptor`.

#### Recovery

During recovery, the wallet currently peeks into the node's database to resolve the current UTxO,
the initial one and the historical data by applying blocks since the genesis one. 

#### Blund 

The wallet currently receives `Blund`s from the `BListener` instead of mere `Block`s. 
However, `Undo`s from `Blund`s are only used to retrieve some transaction Metadata which
are otherwise available in the UTxO maintained by the wallet.

#### Update

Being the only communication channel between Daedalus and the node itself, the wallet
is currently used to convey some specific process supervision commands (e.g. `apply-update`). 
This is currently done via the _"Internal"_ HTTP API.

#### Transaction Submission

The wallet needs to submit transactions to the network. Right now, the wallet is hooked up to 
the node's inner diffusion layer from which it can push new transactions.


## Cardano-SL Haskell Backend (legacy / current)

There's an existing working node-to-node communication protocol which is 
used by nodes and the Rust CLI. This protocol enables us (amongst other things) to:

- Submit new transactions to the network
- Receive blocks from the network, from a given hash


## Cardano-SL Haskell Backend (legacy-free)

_? unknown ?_


## Cardano-SL Rust Backend

Node-to-Node protocol interface implemented; Rest being worked on, mostly following the 
implementation of the existing Haskell backend. 

# Coupling Areas, First Pointers

We assume the Wallet **fully trusts the node** it is connected to such that, validation and
verification work is done by the node and acknowledged by the Wallet. 


## Receiving Blocks 

We want the Wallet to be able to receive blocks as they get validated by the underlying node.
The Wallet is interested in the blocks for several reasons:

- It maintains its own UTxO via a set of specified ledger rules (see _Formal specification for a Cardano Wallet_)

- It tracks a bunch of _Protocol Parameters_ that are specified in the genesis block and may 
  change via updates. For the Wallet, the relevant parameters are:

  - slot length
  - epoch length (NOTE: in current version, can't change via update, but will in future versions of Ouroboros).
  - fee policy
  - transaction max size

The parameters are currently not extracted from the block but queried directly via some IO using 
the fact that Wallet runs as a node.
Knowing how to transition from an initial state via applying a block, it would be possible for the
Wallet to keep track of these parameters as it applies blocks (see _A Simplified Formal Specification of a UTxO Ledger_).
There's a major question about rollbacks and how to "un-apply" rules.

Ideally, we want the ability for the Wallet to ask for two things:

  - An initial state (derived from the genesis block by the node) where state can be 
    many things (e.g. protocol parameters, system start, stake pools distribution, utxo)

  - The current state (for fast syncing)

Then, a consumer can apply ledger rules they're interested in as they receive blocks, 
assuming node has validated pre-conditions for applying those rules. Having the initial
state and the transitions is all we need to keep state in sync.


## Node Monitoring 

The wallet currently offers some basic monitoring of the node via the `/api/v1/node-info` and 
`/api/v1/node-settings`. Underneath, this piggy-backs once more on the Node State Adaptor
and the fact that the Wallet is ultimately a node. This includes:

- `syncProgress`: Syncing progression, in percentage.
- `blockchainHeight`: If known, the current blockchain height, in number of blocks.
- `localBlockchainHeight`: Local blockchain height, in number of blocks.
- `localTimeInformation`: Information about the clock on this node.
- `subscriptionStatus`: Is the node connected to the network and to what peers?
- `slotDuration`: Duration of a slot.
- `softwareInfo`: Various pieces of information about the current software.
- `projectVersion`: Current project's version.
- `gitRevision`: Git revision of this deployment.

Strictly considering the wallet, these pieces of information are quite out-of-scope. 
They should instead be provided by a node itself and queried by clients directly from 
the underlying node. 
At the moment, the Wallet is used as a kind of proxy to relay these pieces of information,
but in order to properly decouple the Wallet from the node, we should look into moving this
API onto the node, with a possible redesign as we do it. 

It's not unrealistic (and actually advised) to have a transition period where
the wallet will still respond to the endpoints above via implementing the node's
API. 

Additionally, the Wallet also may require the `systemStart` to be available in
such API in order to compute a few things from it.


## Node Management 

Falls into ths category, actions that are triggered by clients to act upon the node:

- `apply-update`: Make the node shutdown with a special exit code (`20`) which gets picked up by the launcher.

As for the node monitoring, this is a bit out-of-scope for the Wallet. This responsibility should 
be moved outside of the Wallet and, during a transition / compatibility period, may still be 
supported by the Wallet and forwarded to the node. 


## Restoration

With the ability to query any block from a node (as described in [Receiving Blocks](#receiving-blocks)),
a Wallet can avoid reads to a node's database. Note that, in order to perform restoration while the
Wallet is still processing blocks for existing wallets, we need to be able to listen to blocks on 
multiple (possibly different) channels. 


#### Slot Start 

Historically, the Wallet attaches a timestamp to transations' metadata. This information isn't
actually present in the transaction as they're represented in the block and can't really be 
derived from it. Our best approximation during restoration is to consider the beginning of a 
slot as the timestamp of a transaction. To retrieve this slot time, we could still leverage
the functions defined by the node, and make sure they don't require any access to the database.

Knowing the slot length(s), epoch length(s), current time, current slot and system start
should be enough information to recover any slot start.


## Diffusion / Transaction Submission

Being a node, the Wallet "shortcuts" the transaction submission by directly pushing transactions
to the node's queue. In order to fully decouple the wallet, we would need the wallet to use
the Node <-> Node protocol in order to submit transactions. 



# Milestones

## Requirements

1. - [ ] The Wallet _mustn't_ make any implementation assumptions about the node it is connected to.
1. - [ ] The Wallet _mustn't_ have any access to the node's database.
1. - [ ] The Wallet _must_ have a way to receive new and old blocks from a node.
1. - [ ] The Wallet _must_ have a way to submit transactions to a node.
1. - [ ] The Wallet _must_ be able to receive blocks via multiple simultaneous channels (at least two).
1. - [ ] The Wallet _should_ be compatible with the existing / legacy node.
1. - [ ] It _should_ be relatively easy to transition from a legacy node to a new node implementing Ouroboros Genesis.
1. - [ ] The Node _should_ provide an API for process management. 
1. - [ ] The Node _should_ provide an API for process monitoring.
1. - [ ] The protocol(s) by which the Wallet receives blocks and submit transactions _should_ be specified in a language-neutral specification document(s).
1. - [ ] The process management API _should_ be specified in a language-neutral specification document(s).
1. - [ ] The Wallet _may_ verify some data received by the node (checksums, controlling genesis etc.)
1. - [ ] The Wallet _may_ run on the same machine as the node; this becomes a `must` if the node doesn't implement any monitoring API for NTP.
1. - [ ] The Wallet _may not_ forward any supervision commands to nodes.


## A. Organization structure

In order to have proper decoupling and _experiment_ a lightweight development process and set of tools, 
we will isolate the `wallet-new` code from the cardano codebase. The wallet may still import some 
packages from the cardano-sl suite, but will evolve in a different repository itself. We will try to reduce
part of this code coupling as we go but that's more accessory. 

This will hopefully increase productivity of the wallet team and in the long-term, makes the separation between
node and wallet clearer.

### Main U/S

Setup an infrastructure to build and test wallet code inside its own repo    
　　　　　　　　　　　　　　|  
　　　　　　　　　　　　　　|  
　　　　　　　　　　　　　　v  
Transfer existing code 'wallet-new' to this new repository, and have it built there  
　　　　　　　　　　　　　　|  
　　　　　　　　　　　　　　|  
　　　　　　　　　　　　　　v  
_(Bonus) Clean-up and review build and test procedures to be more tailored to the wallet   


## B. Node Monitoring / Management API(s)

We have two options here:

- Roll a new (small) API covering current needs and leaving room for future improvements.  

  | Pros                                  | Cons                                        |
  | ---                                   | ---                                         |
  | New cleaner design and implementation | Design phase (small, but still taking time) |
  |                                       | Integration slightly harder                 |

- Just move the existing API onto a node.   

  | Pros                                       | Cons                                                  |
  | ---                                        | ---                                                   |
  | Time, almost immediate                     | Keep the legacy, just move it from a place to another |
  | Seamless integration with the current code |                                                       |

> NOTE  
> Authentication will be done via x509 certificates, following the same scheme currently used
> by the wallet. Both the node and the wallet will be running an HTTP server with their own
> certificates signed by a common authority. 

### Main U/S

Define the Node Monitoring & Node Management API(s) via Swagger  
　　　　　　　　　　　　　　|  
　　　　　　　　　　　　　　|  
　　　　　　　　　　　　　　v  
Implement the API(s) on the node's side and have them started with it  
　　　　　　　　　　　　　　|  
　　　　　　　　　　　　　　|  
　　　　　　　　　　　　　　v  
Integrate the Wallet with its Node's API for backward-compatibility  
　　　　　　　　　　　　　　|  
　　　　　　　　　　　　　　|  
　　　　　　　　　　　　　　v  
_(Bonus) Extend this API with more elements and / or actions_


## C. Transaction Submission via current Node-To-Node protocol

The Wallet should implement part of the current Node-To-Node protocol in order to submit 
transactions. Note that this could (ought to) be abstracted away in a "submitter" interface
which could give birth later on to a Node <-> Submitter dedicated protocol. 

This shouldn't be too complicated, especially with the Wallet being written in Haskell: we
can leverage most of the types, serialization methods and utils already defined in `networking`
and `infra` to implement the protocol. 

Part of this milestone may concern the design of such protocol, alongside the Haskell team
working on this. 

### Main U/S

Comprehend and integrate with (part of) the Node-To-Node protocol to submit transactions  
　　　　　　　　　　　　　　|  
　　　　　　　　　　　　　　|  
　　　　　　　　　　　　　　v  
Clearly identify currents areas where transactions are submitted / resubmitted  
　　　　　　　　　　　　　　|  
　　　　　　　　　　　　　　|  
　　　　　　　　　　　　　　v  
Design a protocol-neutral interface which abstract transactions submission away  
　　　　　　　　　　　　　　|  
　　　　　　　　　　　　　　|  
　　　　　　　　　　　　　　v  
Implement the interface by using the current Node-To-Node protocol  
　　　　　　　　　　　　　　|  
　　　　　　　　　　　　　　|  
　　　　　　　　　　　　　　v  
Replace current direct usage of the diffusion layer by the interface above  
　　　　　　　　　　　　　　|  
　　　　　　　　　　　　　　|  
　　　　　　　　　　　　　　v  
(Bonus) Design a Node <-> Submitter dedicated protocol with the design team  
　　　　　　　　　　　　　　|  
　　　　　　　　　　　　　　|  
　　　　　　　　　　　　　　v  
(Bonus) Implement the aforementioned interface using this new Node <-> Submitter protocol


## D. Node <-> Consumer protocol

The Node should be able to receive blocks and derive states from those blocks
using the existing Node <-> Node protocol. Again, this part ought to be
abstracted away in a "consumer" interface which will be implemented _at some
point_ using the Node <-> Consumer protocol.

Part of this milestone will be dedicated to designing and specifying this new protocol in CDDL
but only at a later stage. 

### Main U/S

Comprehend and integrate with (part of) the Node-To-Node protocol to receive blocks  
　　　　　　　　　　　　　　|  
　　　　　　　　　　　　　　|  
　　　　　　　　　　　　　　v  
Design a protocol-neutral interface to receive blocks, command a switch to fork, get an initial state, get current state    
　　　　　　　　　　　　　　|  
　　　　　　　　　　　　　　|  
　　　　　　　　　　　　　　v  
Implement the interface by using the current Node-To-Node protocol  
　　　　　　　　　　　　　　|  
　　　　　　　　　　　　　　|  
　　　　　　　　　　　　　　v  
Replace current BListener and NodeStateAdaptor usage by usage of the aforementioned interface  
　　　　　　　　　　　　　　|  
　　　　　　　　　　　　　　|  
　　　　　　　　　　　　　　v  
(Bonus) Sync-up on the Node <-> Consumer protocol, and possibly revisit the Wallet inner interface  
　　　　　　　　　　　　　　|  
　　　　　　　　　　　　　　|  
　　　　　　　　　　　　　　v  
(Bonus) Once ready, implement the Wallet interface using the Node <-> Consumer protocol  


## Some remarks

By tackling all items not listed as _Bonus_, we will have fully decoupled the wallet from 
the current node. Note that the Wallet will only be compatible with nodes that implement
the _current_ Node <-> Node protocol as well as the monitoring and managements API (though
accessory, we only intend to maintain this weak coupling for a few release until users have
transitioned). 

In the meantime, we will have prepared the Wallet to be interfaced with a new Node <-> Consumer
protocol, and it is not unrealistic to imagine the wallet being able to run in different 
compatibility modes; comprehending one protocol or the other. 
