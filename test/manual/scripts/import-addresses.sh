#!/bin/bash
#
# Usage: import-addresses <wallet_id> <filename> [options]
#
# This script imports addresses to the wallet existing on the node.
# One need to provide wallet id and a filename with Json list of addresses e.g.:
#  [
#  "DdzFFzCqrhsv9Ni5Cekz8kDYW6G8nJ9MgVDXyGrhUKgt9wzCAcSwDEs9iEitsoninyD2bnCySuZW4oUXasmrT3L6HQtHimHTHYyiahMP",
#  "DdzFFzCqrht4MYxeuRq2yfoSFn2F5kXjqKLRZQ4G17grWn7uhd7jFapMoMX8wrpE7g7XhcerbCsVA8qXkM175LhDTRcUpggfrkR24UWJ"
#  ]
#
# Arguments:
#   <wallet_id>             A wallet's root id
#   <filename>              filename with list of addresses to import
#
# Options:
#   -h --help
#   -a --account=INT         An optional parent account index [default: 2147483648]
#   -n --number=INT          number of addresses to generate [default: 10]
#   -p --port=INT            Port the server is listening to [default: 8090]
#   -c --client=FILEPATH     TLS client certificate expected by the server [default: ../../../state-staging/tls/client/client.pem]
#
# Examples:
#  ./import-addresses.sh Ae2tdPwUPEZLXCZQkmwcatABSc5Fw1bp3dk5nBkNnkNHy6teLdxqQm8NPgW addr.txt
#

PATH=.:$PATH; source docopts.sh --auto "$@"

curl -kX POST https://localhost:${ARGS[--port]}/api/v1/wallets/${ARGS[<wallet_id>]}/accounts/${ARGS[--account]}/addresses \
  -H "Accept: application/json; charset=utf-8" \
  -H "Content-Type: application/json; charset=utf-8" \
  --cert ${ARGS[--client]} \
  -d @${ARGS[<filename>]}  --http1.1 |  jq
