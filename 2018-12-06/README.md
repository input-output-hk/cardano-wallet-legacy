# Wallet Backend Weekly Report 

<p align="right">
  <strong>Week 49</strong>: 2018/11/30 → 2018/12/06
</p>

# Overview

![](overview.png)

## Remarks

- Ante (@akegalj) was on holiday until 2018/05/12
- Ryan (@uroboros) was sick during the early weeks, therefore not at 100%


## Milestones

###  Decoupling

```
[=================================================>.............................] 63% (50/79)
```

|                 | Start Date | Estimated End Date | Done (13 pts) | In Progress (26 pts) | New Bugs | Fixed Bugs |
| -----           | -----      | -----              | -----         | -----                | -----    | -----      |
| ![][Decoupling] | 2018-10-22 | 2019-01-09         | [#15](https://github.com/input-output-hk/cardano-wallet/issues/15), [#65](https://github.com/input-output-hk/cardano-wallet/issues/65)      | [#30](https://github.com/input-output-hk/cardano-wallet/issues/30), [#87](https://github.com/input-output-hk/cardano-wallet/issues/87), [#88](https://github.com/input-output-hk/cardano-wallet/issues/88)        | -        | -          |

---

###  Address Derivation à la BIP-44

```
[===========>...................................................................] 15% (11/72)
```

|             | Start Date | Estimated End Date | Done (8 pts) | In Progress (13 pts) | New Bugs | Fixed Bugs |
| -----       | -----      | -----              | -----        | -----                | -----    | -----      |
| ![][BIP-44] | 2018-10-22 | 2019-01-09         | [#33](https://github.com/input-output-hk/cardano-wallet/issues/33)          | [#31](https://github.com/input-output-hk/cardano-wallet/issues/31), [#34](https://github.com/input-output-hk/cardano-wallet/issues/34), [#45](https://github.com/input-output-hk/cardano-wallet/issues/45)        | -        | -          |

---

### Continuous Integration

```
[=============================>.................................................] 38% (9/24)
```

|         | Start Date | Estimated End Date | Done  | In Progress (13 pts) | New Bugs | Fixed Bugs (3 pts) |
| -----   | -----      | -----              | ----- | -----                | -----    | -----              |
| ![][CI] | 2018-11-19 | 2019-01-09         | -     | [#117](https://github.com/input-output-hk/cardano-wallet/issues/117), [#119](https://github.com/input-output-hk/cardano-wallet/issues/119)           | -        | [#132](https://github.com/input-output-hk/cardano-wallet/issues/132)               |


# Week Restrospective

## Deliverables

### ![][Decoupling] [#15](https://github.com/input-output-hk/cardano-wallet/issues/15) Comprehend (part of) the Node-to-Node protocol to receive blocks

> **Context**  
> We want the Wallet to be able to receive blocks as they get validated by the
> underlying node. Most protocol parameters required for the good functionning
> of the wallet aren't currently extracted from the block but queried directly
> from the surrounding state using the fact that Wallet runs as a node. In a
> similar fashion, during restoration, blocks are read from the database
> instead of being received through the network. This creates strong process
> coupling between the wallet and its underlying node.

> **Action**  
> Before taking any further action, we had to comprehend and document the
> relevant parts of the Node-To-Node protocol that is used to receive blocks.
> Findings have been documented in the wiki and brightened it up with diagrams.


### ![][Decoupling] [#65](https://github.com/input-output-hk/cardano-wallet/issues/65) Decouple wallet dependency from cluster 

> **Context**  
> Historically, we created the cluster package inside the cardano-sl repository
> to have an easy way to run cluster of nodes. However, at this time (and still
> today), edge nodes and wallet backend server are tightly coupled. Starting one
> also starts the other which is rather unpractical as it creates some circular
> dependencies between `cardano-sl/cluster` and `cardano-wallet`.

> **Action**  
> We have removed the dependency of `cardano-wallet` from `cluster` and have
> the wallet started separately. This was quite tricky because, at the moment,
> the wallet still needs to boot an actual node. This enables us to move forward
> with using this cluster library to run our integration tests!

### ![][BIP-44] [#33](https://github.com/input-output-hk/cardano-wallet/issues/33) Address Pool Module

> **Context**  
> We were looking for a module which can keep track of address pools (as described in 
> BIP-44) and answer the question: 'is this address ours?'. 

> **Action**  
> We've built this module in a rather isolated fashion such that it could be tested
> thoroughly and isolate its complexity. 


## Bugs 

### ![][CI] [#132](https://github.com/input-output-hk/cardano-wallet/issues/132) Cache Failures on CI

> **Context**  
> As `cardano-sl` gets updated, it introduces some "dust" in our cache system
> as old packages remain in the cache and new one gets added. Eventually, the
> cache gets bloated and becomes a bit too big.

> **Retrospective**  
> Cache is now properly reset every day by the cron job. This should reduce the
> impact of bumping our external dependencies as long as it doesn't happen too
> often within 24h.


[Decoupling]: https://img.shields.io/badge/-decoupling-%233498db.svg?style=flat-square
[BIP-44]: https://img.shields.io/badge/-BIP--44-%239b59b6.svg?style=flat-square
[CI]: https://img.shields.io/badge/-continuous%20integration-%232ecc71.svg?style=flat-square
[Release/1.4.0]: https://img.shields.io/badge/-release%202.0.0-%2e74c3c.svg?style=flat-square
