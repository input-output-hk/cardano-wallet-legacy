
# Context

This document describes manual test procedure for https://github.com/input-output-hk/cardano-wallet/issues/258. 

The change here introduces new endpoint `POST /api/v1/wallets/{wid}/accounts/{accix}/addresses` which allows batch import of unused addresses into the wallet's account. This functionality is targeted to exchanges and the possible use case has been described in the Wallet's API documentation https://input-output-hk.github.io/cardano-wallet/#section/Common-Use-Cases/Importing-(Unused)-Addresses-From-a-Previous-Node-(or-Version).

There have been automated integration tests added for testing new endpoint however there are still areas that need to be tested manually as they cannot be easily instrumented using integration tests. These are:
 - doing batch import of addresses while wallet is being restored
 - importing large amount of addresses (50k)


# Data

Following wallets can be used for testing. These are _1.3.1_ wallets existing on staging blockchain:
1. `["quit", "elder", "useless", "love", "foster", "know", "glad", "muffin", "guitar", "shrimp", "glad", "liberty"]` - `Ae2tdPwUPEZLBbAHVdKg9uLBeHxZQjXeVguFWJNASbQ7vugfQbmwHqSFMVy`

2. `["clinic","nuclear","paddle","leg","lounge","fabric","claw","trick","divide","pretty", "argue", "fade"]` - `Ae2tdPwUPEZLXCZQkmwcatABSc5Fw1bp3dk5nBkNnkNHy6teLdxqQm8NPgW`

3. `["offer","need","accuse","yellow","sibling","robust","view","rare","cry","picture", "weather", "love"]` - `Ae2tdPwUPEZLXCZQkmwcatABSc5Fw1bp3dk5nBkNnkNHy6teLdxqQm8NPgW`

# Scenarios

## Test 1 - Batch import list of addresses while the wallet is restoring

### Summary: 

This test includes importing unused addresses to the wallet that is being restored. Expected is that batch import can be successfully performed while the wallet is being restored.

## Prerequisites
 - Start a node connected to staging with `cardano-wallet` attached as described here -> https://github.com/input-output-hk/cardano-wallet/wiki/Building-Running-and-Testing-a-wallet-node. Have the node 100% synced.
 - Have a Daedalus staging installed and synced. Get Daedalus staging from https://buildkite.com/input-output-hk/daedalus.
 - Have a wallet already restored in the Daedalus staging. See above for possible wallet data.


### Steps:
1. In Daedalus, go to the restored wallet (see prerequisites) and create few addresses (Receive tab). Make sure addresses are not used.

2. Start restoring the same wallet that you have on Daedalus on the manually started node.

```
curl  -X POST https://localhost:8090/api/v1/wallets \
  -H "Accept: application/json; charset=utf-8" \
  -H "Content-Type: application/json; charset=utf-8" \
  --cert ./state-staging/tls/client/client.pem \
  --cacert ./state-staging/tls/client/ca.crt \
  -d '{
  "operation": "restore",
  "backupPhrase":["clinic","nuclear","paddle","leg","lounge","fabric","claw","trick","divide","pretty", "argue", "fade"],
  "assuranceLevel": "normal",
  "name": "test#258"
}'  --http1.1 |  python -m json.tool
```

3. Make sure wallet is restoring.

```
curl -X GET https://localhost:8090/api/v1/wallets/Ae2tdPwUPEZLXCZQkmwcatABSc5Fw1bp3dk5nBkNnkNHy6teLdxqQm8NPgW \
  -H "Accept: application/json; charset=utf-8" \
  -H "Content-Type: application/json; charset=utf-8" \
  --cert ./state-staging/tls/client/client.pem \
  --cacert ./state-staging/tls/client/ca.crt --http1.1  |  python -m json.tool

```

4. Batch import addresses to the wallet that is being restored on the node. In the payload put two addresses previously created with Daedalus, but not used. And two that belong to different wallet.

```
curl -X POST https://localhost:8090/api/v1/wallets/Ae2tdPwUPEZLXCZQkmwcatABSc5Fw1bp3dk5nBkNnkNHy6teLdxqQm8NPgW/accounts/2147483648/addresses \
  -H "Accept: application/json; charset=utf-8" \
  -H "Content-Type: application/json; charset=utf-8" \
  --cert ./state-staging/tls/client/client.pem \
  --cacert ./state-staging/tls/client/ca.crt \
  -d '[
  "DdzFFzCqrhsq1fQf66snjqHdZyMB8FMB7DhKAY3cBvqtoo7jw2ZGYMRn36wQZhiHp2Krnf6p7izgky8gJmmCWgEUQBy984ucCn2rs4aY",
  "DdzFFzCqrhsofWdZGrC6jBAgFWroYcULQxpGDLsiYooG4sXE5HksbsAw4j9itz7jL1YNspCHdDX6A5SbFSuNYrqsk9Au7m9XFmNParGc",
  "DdzFFzCqrhtAercxp32BQUzjBXtgYXMPdYxkBSnhVSNQRwDvKEDRXy4k3FYCQerLGZKG4Cqvt514VdXUA9ihze6GtvFHWur3DRRjDexf",
  "DdzFFzCqrht9jruyiSZu4mQQcci9jupFS59pTv9xwFmdTwjTjfKinVXrKWxBWVmQWyVUcBMg4mZ2WD8godj4T7wMnkdWzqKjc3Bj8quL"
  ]'  --http1.1 |  python -m json.tool
```
5. Wait until wallet gets restored on the node and check if imported addresses are available for the wallet's account.

a) get wallet's account id:
```
curl -X GET https://localhost:8090/api/v1/wallets/Ae2tdPwUPEZLBbAHVdKg9uLBeHxZQjXeVguFWJNASbQ7vugfQbmwHqSFMVy/accounts \
  -H "Accept: application/json; charset=utf-8" \
  -H "Content-Type: application/json; charset=utf-8" \
  --cert ./state-staging/tls/client/client.pem \
  --cacert ./state-staging/tls/client/ca.crt --http1.1  |  python -m json.tool
```

b) get addresses (set your account id instead of `2147483648` if needed):
```
curl -vX GET https://localhost:8090/api/v1/wallets/Ae2tdPwUPEZLXCZQkmwcatABSc5Fw1bp3dk5nBkNnkNHy6teLdxqQm8NPgW/accounts/2147483648/addresses \
  -H "Accept: application/json; charset=utf-8" \
  -H "Content-Type: application/json; charset=utf-8" \
  --cert ./state-staging/tls/client/client.pem \
  --cacert ./state-staging/tls/client/ca.crt --http1.1  |  python -m json.tool

```

### Expected:

4. Import should be successful. Two addresses should be imported successfully (the addresses generated in Daedalus). The other two addresses should be listed in the failures of the response, e.g.:

```
{
    "data": {
        "failures": [
            "DdzFFzCqrhsq1fQf66snjqHdZyMB8FMB7DhKAY3cBvqtoo7jw2ZGYMRn36wQZhiHp2Krnf6p7izgky8gJmmCWgEUQBy984ucCn2rs4aY",
            "DdzFFzCqrhsofWdZGrC6jBAgFWroYcULQxpGDLsiYooG4sXE5HksbsAw4j9itz7jL1YNspCHdDX6A5SbFSuNYrqsk9Au7m9XFmNParGc",
       ],
        "totalSuccess": 2
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
5. After wallet is restored all imported addresses are available for it's account.


## Test 2 - Import large number of addresses


### Summary: 

This test includes: 
 - generating 50k addresses on the wallet inside Daedalus staging node. All these generated addresses are obviously _not used_. 
 - checking currently existing addresses on the wallet that exists on the node
 - Preparing the import request to be executed on the wallet that exists on the node
 - importing previously generated addresses into the wallet that exists on the node and investigating the results

### Prerequisites
 - have the same wallet synced on both: Daedalus staging and the node. (node should be started manually using this step-by-step https://github.com/input-output-hk/cardano-wallet/wiki/Building-Running-and-Testing-a-wallet-node)
 - both Daedalus node and manual node are synced.
 - wallets on both nodes are synced.

### Steps:
1. Generate 50k addresses on Daedalus wallet. The following script can be used: [create_addresses.sh](./create_addresses.sh) (Please note that the script may take ~2h to complete)
2. Get all addresses into the file using the script [get_addresses.sh](./get_addresses.sh) (The script gets all addresses into a file which then can be used in subsequent batch import request)
3. Get all the addresses from the wallet that exists on the node and write them down.
```
curl -vX GET https://localhost:8090/api/v1/wallets/Ae2tdPwUPEZLXCZQkmwcatABSc5Fw1bp3dk5nBkNnkNHy6teLdxqQm8NPgW/accounts/2147483648/addresses \
  -H "Accept: application/json; charset=utf-8" \
  -H "Content-Type: application/json; charset=utf-8" \
  --cert ./state-staging/tls/client/client.pem \
  --cacert ./state-staging/tls/client/ca.crt --http1.1  |  python -m json.tool
  
```
5. Attempt to import 50k addresses into the same wallet that exists on the node.
```
curl -X POST https://localhost:8090/api/v1/wallets/Ae2tdPwUPEZLXCZQkmwcatABSc5Fw1bp3dk5nBkNnkNHy6teLdxqQm8NPgW/accounts/2147483648/addresses \
  -H "Accept: application/json; charset=utf-8" \
  -H "Content-Type: application/json; charset=utf-8" \
  --cert ~/wb/cardano-wallet/state-staging/tls/client/client.pem \
  --cacert ~/wb/cardano-wallet/state-staging/tls/client/ca.crt \
  -d @addresses50k.txt --http1.1 | python -m json.tool
```

### Expected:
The import should be successful. If there are any failures make sure that they concern addresses written down in step 3 above.
Please note: the batch import request with 50k addresses took ~7-9 minutes on the local node for the following hardware spec: Ubuntu Linux, SSD, 16GB RAM, Intel Core i5 (8 cores).

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


## Test 3 - Import large number of addresses while wallet is restoring


### Summary: 

This test case essentially combines the both scenarios above. The aim is to import a large amount of addresses while wallet is being restored and make sure there is no misbehaviour.

### Prerequisites
 - have the wallet synced on Daedalus staging. 
 - have a node started using this step-by-step https://github.com/input-output-hk/cardano-wallet/wiki/Building-Running-and-Testing-a-wallet-node)
 - both Daedalus node and manual node are synced.

### Steps:
1. Get all the *used* addresses from the Daedalus wallet and write them down. (Receive tab)
2. Generate 50k addresses on Daedalus wallet. The following script can be used: [create_addresses.sh](./create_addresses.sh) (Please note that the script may take ~2h to complete)
3. Get all addresses into the file using the script [get_addresses.sh](./get_addresses.sh) (The script gets all addresses into a file which then can be used in subsequent batch import request)
4. On the manually started node: start restoring the same wallet that you have on Daedalus.

```
curl  -X POST https://localhost:8090/api/v1/wallets \
  -H "Accept: application/json; charset=utf-8" \
  -H "Content-Type: application/json; charset=utf-8" \
  --cert ./state-staging/tls/client/client.pem \
  --cacert ./state-staging/tls/client/ca.crt \
  -d '{
  "operation": "restore",
  "backupPhrase":["clinic","nuclear","paddle","leg","lounge","fabric","claw","trick","divide","pretty", "argue", "fade"],
  "assuranceLevel": "normal",
  "name": "test#258"
}'  --http1.1 |  python -m json.tool
```

5. Attempt to import 50k addresses into the same wallet that exists on the node.
```
curl -X POST https://localhost:8090/api/v1/wallets/Ae2tdPwUPEZLXCZQkmwcatABSc5Fw1bp3dk5nBkNnkNHy6teLdxqQm8NPgW/accounts/2147483648/addresses \
  -H "Accept: application/json; charset=utf-8" \
  -H "Content-Type: application/json; charset=utf-8" \
  --cert ~/wb/cardano-wallet/state-staging/tls/client/client.pem \
  --cacert ~/wb/cardano-wallet/state-staging/tls/client/ca.crt \
  -d @addresses50k.txt --http1.1 | python -m json.tool
```

### Expected:
The import should be successful. If there are any failures make sure that they concern addresses written down in step 1 above.
Please note: the batch import request with 50k addresses took ~7-9 minutes on the local node for the following hardware spec: Ubuntu Linux, SSD, 16GB RAM, Intel Core i5 (8 cores).

Exemplary response:
```
{
    "data": {
        "failures": [
            "DdzFFzCqrhsovPRUWHeFun6CokFFLNUvzeB31MqW28SFEZf2aaasJCBc1LUQFWBPUqn1FNcFVS5EY9rkxXBJmfeaEZkrdqfjH5dXNo4C"
        ],
        "totalSuccess": 49999
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
