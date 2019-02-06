#!/bin/bash
#
# Usage: generate-addresses [options]
#
# This script generates 'n' Cardano addresses from a running wallet backend
# server and output them to stdout.
#
# Arguments:
# Options:
#   -h --help
#   -w --wallet=STRING    A parent wallet root Id
#   -a --account=INT      An optional parent account index [default: 2147483648]
#   -n --number=INT       number of addresses to generate [default: 10]
#   -p --port=INT         Port the server is listening to [default: 8090]
#   -c --client=FILEPATH  TLS client certificate expected by the server [default: ../../../state-integration/tls/edge/client.pem]
PATH=.:$PATH; source docopts.sh --auto "$@"

for i in $(seq 1 ${ARGS[--number]})
do
  >&2 echo "Address no: $i"
	ADDRESS=$(curl -kX POST https://localhost:${ARGS[--port]}/api/v1/addresses \
  		-H "Accept: application/json; charset=utf-8" \
  		-H "Content-Type: application/json; charset=utf-8" \
  		--cert ${ARGS[--client]} \
  		-d "{
		  \"accountIndex\": ${ARGS[--account]},
		  \"walletId\": \"${ARGS[--wallet]}\"
      }" | jq .data.id)
  echo ${ADDRESS//\"}
done
