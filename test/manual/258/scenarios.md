# Context

This document describes manual test procedure for https://github.com/input-output-hk/cardano-wallet/issues/258.

The change here introduces new endpoint `POST /api/v1/wallets/{wid}/addresses` which allows batch import of unused addresses into the wallet's account. This functionality is targeted to exchanges and the possible use case has been described in the Wallet's API documentation https://input-output-hk.github.io/cardano-wallet/#section/Common-Use-Cases/Importing-(Unused)-Addresses-From-a-Previous-Node-(or-Version).

There have been automated integration tests added for testing new endpoint however there are still areas that need to be tested manually as they cannot be easily instrumented using integration tests. These are:
 - doing batch import of addresses while wallet is being restored
 - importing large amount of addresses (50k)

Tests for this change are described by wallet capability `ADDRESSES_IMPORT` -> https://docs.google.com/spreadsheets/d/1ztF2PnnrMnkAxrhdq3LUbP1KazxcxuMrbiObTs1CuTk.
Manual scenarios cover capabilities ADDRESSES_IMPORT_07 and ADDRESSES_IMPORT_08.


# Scripts
Following scripts can be useful to aid the manual steps below:
 - [wallet.sh](../scripts/wallet.sh)  - allows basic interaction with the wallet on the node (create, restore, delete, read)
 - [generate-addresses.sh](../scripts/generate-addresses.sh) - generate addresses for the wallet's account
 - [import-addresses.sh](../scripts/import-addresses.sh) - batch import of addresses to the wallet's account

**Please note:** data in commands below is exemplary. Mnemonics for the tests can be easily generated with https://iancoleman.io/bip39/.

# Scenarios

## Test 1 - Batch import list of addresses while the wallet is restoring

### Summary:

This test includes importing unused addresses to the wallet that is being restored. Expected is that batch import can be successfully performed while the wallet is being restored.

## Prerequisites
 - Start a node connected to staging with `cardano-wallet` attached as described here -> https://github.com/input-output-hk/cardano-wallet/wiki/Building-Running-and-Testing-a-wallet-node. Have the node 100% synced.


### Steps:
1. Create a wallet above on the node - WalletR.
`./wallet.sh create parrot ugly lock symbol sibling display bright border yellow pencil area dawn`.
2. Create some new wallet - WalletN.
`./wallet.sh create notable welcome tobacco absorb coach warm cheap strong thrive jelly embrace pilot`.
3. Generate few addresses for WalletR and store them inside the file in the form of Json list.
`./generate-addresses.sh Ae2tdPwUPEZKyifkHShCKCbzzPbi7DBV7XuyTPEd13KRES4v8Ut3PmKebLy -j > addr.txt`
4. Generate few addresses for WalletN and add them to the addr.txt file.
5. Delete the restored WalletR
`./wallet.sh delete Ae2tdPwUPEZKyifkHShCKCbzzPbi7DBV7XuyTPEd13KRES4v8Ut3PmKebLy`
6. Start restoring WalletR again.
`./wallet.sh restore parrot ugly lock symbol sibling display bright border yellow pencil area dawn`
7. While the WalletR is restoring - batch import the addresses generated from WalletN and WalletR.
`./import-addresses.sh Ae2tdPwUPEZKyifkHShCKCbzzPbi7DBV7XuyTPEd13KRES4v8Ut3PmKebLy addr.txt`

8. Wait until wallet gets restored on the node and check if imported addresses are available for the wallet's account.
`./wallet.sh get-addresses Ae2tdPwUPEZKyifkHShCKCbzzPbi7DBV7XuyTPEd13KRES4v8Ut3PmKebLy`

### Expected
1. WalletR is created.
2. WalletN is created.
3. Generated successfully.
4. Generated successfully.
5. WalletR deleted.
6. WalletR is restoring.
7. Batch import successful. Only addresses generated previously for WalletR are imported. Addresses generated for WalletN are reported as `failures` in the response.
8. After WalletR is restored all imported addresses are available.

## Test 2 - Import large number of addresses


### Summary:

This test includes:
 - generating 50k addresses on the wallet inside Daedalus staging node. All these generated addresses are obviously _not used_.
 - checking currently existing addresses on the wallet that exists on the node
 - Preparing the import request to be executed on the wallet that exists on the node
 - importing previously generated addresses into the wallet that exists on the node and investigating the results

### Prerequisites
 - Start a node connected to staging with `cardano-wallet` attached as described here -> https://github.com/input-output-hk/cardano-wallet/wiki/Building-Running-and-Testing-a-wallet-node. Have the node 100% synced.

### Steps:
1. Create a wallet above on the node - WalletR.
`./wallet.sh create parrot ugly lock symbol sibling display bright border yellow pencil area dawn`.
2. Get the current **used** addresses on the wallet. Save it for future comparison.
`./wallet.sh get-addresses Ae2tdPwUPEZKyifkHShCKCbzzPbi7DBV7XuyTPEd13KRES4v8Ut3PmKebLy`
3. Generate 50k addresses on the wallet and store it in a file in the form of JSON list. **Please note that the script can take up to 2 hours.**
`./generate-addresses.sh Ae2tdPwUPEZKyifkHShCKCbzzPbi7DBV7XuyTPEd13KRES4v8Ut3PmKebLy -j -n 50000 > 50k.txt`
4. Delete the restored WalletR
`./wallet.sh delete Ae2tdPwUPEZKyifkHShCKCbzzPbi7DBV7XuyTPEd13KRES4v8Ut3PmKebLy`
5. Create WalletR again.
`./wallet.sh create parrot ugly lock symbol sibling display bright border yellow pencil area dawn`
6. Attempt to import 50k addresses into the same wallet that exists on the node.
`./import-addresses.sh Ae2tdPwUPEZKyifkHShCKCbzzPbi7DBV7XuyTPEd13KRES4v8Ut3PmKebLy 50k.txt`

### Expected:
1. WalletR is created.
2. **Used** addresses saved.
3. Generated successfully.
4. WalletR deleted.
5. WalletR created.
6. Batch import successful. Only addresses generated previously for WalletR are imported. Addresses that where already **used** are reported as `failures` in the response.

Exemplary response:
```
{
    "data": {
        "failures": [
            "DdzFFzCqrhskD1VjKUAaViF4kfdtgyK3vH3HpGZ9sovnQTAz6g1AF1Q88EgzqyhMBtVCc56VvxRCpKQPm7V9Va9G8p8uTXEp3kkQ85W1",
            "DdzFFzCqrhsvmqQfb3GZ1vGqvLhUDptFDzFeW8Sb5JR15PMo1Mi2E2q3rjzxX1HyLEYRMuYCBVm2KHi7QFAtJoSDTFkTgXoLxicoGEqV",
            "DdzFFzCqrhsgQUEetVpWz1o1w6MxwH2FqNnDRwzSRhPbscVoWak73Nqe6oCnzcPjKWDioRygbHUS7HGTYFHx7cpTeTKHdTmxMkkFWWhL",
            "DdzFFzCqrht2TTuitt4gLeVpeKdvi8JEFCMMwgQN5iLcWyd4BEdCvC8k4dpNa1MseTkyjiveCQxQNnbX6xQt374o4iDnYctbdELgw8Q7"
        ],
        "totalSuccess": 49956
    },
    "meta": {
        "pagination": {
            "page": 1,
            "perPage": 1,
            "totalEntries": 1,
            "totalPages": 1
        }
    },
    "status": "success"
}
```
**Please note:** the batch import request with 50k addresses took ~7-9 minutes on the local node for the following hardware spec: Ubuntu Linux, SSD, 16GB RAM, Intel Core i5 (8 cores).

## Test 3 - Import large number of addresses while wallet is restoring


### Summary:

This test case essentially combines the both scenarios above. The aim is to import a large amount of addresses while wallet is being restored and make sure there is no misbehavior.

### Prerequisites
 - Start a node connected to staging with `cardano-wallet` attached as described here -> https://github.com/input-output-hk/cardano-wallet/wiki/Building-Running-and-Testing-a-wallet-node. Have the node 100% synced.

### Steps:
1. Create a wallet above on the node - WalletR.
`./wallet.sh create parrot ugly lock symbol sibling display bright border yellow pencil area dawn`.
2. Get the current **used** addresses on the wallet. Save it for future comparison.
`./wallet.sh get-addresses Ae2tdPwUPEZKyifkHShCKCbzzPbi7DBV7XuyTPEd13KRES4v8Ut3PmKebLy`
3. Generate 50k addresses on the wallet and store it in a file in the form of JSON list. **Please note that the script can take up to 2 hours.**
`./generate-addresses.sh Ae2tdPwUPEZKyifkHShCKCbzzPbi7DBV7XuyTPEd13KRES4v8Ut3PmKebLy -j -n 50000 > 50k.txt`
4. Delete the restored WalletR
`./wallet.sh delete Ae2tdPwUPEZKyifkHShCKCbzzPbi7DBV7XuyTPEd13KRES4v8Ut3PmKebLy`
5. Start restoring WalletR again.
`./wallet.sh create parrot ugly lock symbol sibling display bright border yellow pencil area dawn`
6. While the wallet is restoring Attempt to import 50k addresses into the same wallet that exists on the node.
`./import-addresses.sh Ae2tdPwUPEZKyifkHShCKCbzzPbi7DBV7XuyTPEd13KRES4v8Ut3PmKebLy 50k.txt`
7. Wait until wallet gets restored on the node and check if imported addresses are available for the wallet's account.
`./wallet.sh get-addresses Ae2tdPwUPEZKyifkHShCKCbzzPbi7DBV7XuyTPEd13KRES4v8Ut3PmKebLy`

### Expected:
1. WalletR is created.
2. **Used** addresses saved.
3. Generated successfully.
4. WalletR deleted.
5. WalletR created.
6. Batch import successful. Only addresses generated previously for WalletR are imported. Addresses that where already **used** are reported as `failures` in the response.

Exemplary response:
```
{
    "data": {
        "failures": [
            "DdzFFzCqrhskD1VjKUAaViF4kfdtgyK3vH3HpGZ9sovnQTAz6g1AF1Q88EgzqyhMBtVCc56VvxRCpKQPm7V9Va9G8p8uTXEp3kkQ85W1",
            "DdzFFzCqrhsvmqQfb3GZ1vGqvLhUDptFDzFeW8Sb5JR15PMo1Mi2E2q3rjzxX1HyLEYRMuYCBVm2KHi7QFAtJoSDTFkTgXoLxicoGEqV",
            "DdzFFzCqrhsgQUEetVpWz1o1w6MxwH2FqNnDRwzSRhPbscVoWak73Nqe6oCnzcPjKWDioRygbHUS7HGTYFHx7cpTeTKHdTmxMkkFWWhL",
            "DdzFFzCqrht2TTuitt4gLeVpeKdvi8JEFCMMwgQN5iLcWyd4BEdCvC8k4dpNa1MseTkyjiveCQxQNnbX6xQt374o4iDnYctbdELgw8Q7"
        ],
        "totalSuccess": 49956
    },
    "meta": {
        "pagination": {
            "page": 1,
            "perPage": 1,
            "totalEntries": 1,
            "totalPages": 1
        }
    },
    "status": "success"
}
```

7. After wallet is restored all imported addresses are there in the wallet.

**Please note:** the batch import request with 50k addresses took ~7-9 minutes on the local node for the following hardware spec: Ubuntu Linux, SSD, 16GB RAM, Intel Core i5 (8 cores).
