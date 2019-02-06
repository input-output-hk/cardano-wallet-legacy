#!/bin/bash
#
# Usage: generate-addresses <wallet_id> [options]
#
# This script generates 'n' Cardano addresses from a running wallet backend
# server and output them to stdout.
#
# Arguments:
#   <wallet_id>             A wallet's root id
#
# Options:
#   -h --help
#   -j --json                Output as Json list
#   -a --account=INT         An optional parent account index [default: 2147483648]
#   -n --number=INT          number of addresses to generate [default: 10]
#   -p --port=INT            Port the server is listening to [default: 8090]
#   -c --client=FILEPATH     TLS client certificate expected by the server [default: ../../../state-staging/tls/client/client.pem]
#

PATH=.:$PATH; source docopts.sh --auto "$@"

if [ "${ARGS[--json]}" == "true" ]; then
    echo "["
fi

for i in $(seq 1 ${ARGS[--number]})
do
	>&2 echo "Address no: $i"
	ADDRESS=$(curl -skX POST https://localhost:${ARGS[--port]}/api/v1/addresses \
  		-H "Accept: application/json; charset=utf-8" \
  		-H "Content-Type: application/json; charset=utf-8" \
  		--cert ${ARGS[--client]} \
  		-d "{
		  \"accountIndex\": ${ARGS[--account]},
		  \"walletId\": \"${ARGS[<wallet_id>]}\"
      }" | jq .data.id)

      if [ "${ARGS[--json]}" == "true" ]; then
        if [ $i == ${ARGS[--number]} ]; then
	  echo "${ADDRESS}"
        else
          echo "${ADDRESS},"
        fi
      else
        echo ${ADDRESS//\"}
      fi
done

if [ "${ARGS[--json]}" == "true" ]; then
    echo "]"
fi
