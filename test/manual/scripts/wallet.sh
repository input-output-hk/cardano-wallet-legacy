#!/bin/bash
#
# Usage:
#	wallet (create | restore) [options] MNEMONICS...
#	wallet delete <wallet_id> [options]
#	wallet get [<wallet_id>] [options]
#	wallet get-addresses <wallet_id>
#
# The script allows for basic interaction with wallet.
# Please note that you can easily generate mnemonics for the script at https://iancoleman.io/bip39/.
#
# Arguments:
#   <wallet_id>		    A wallet's root id
#
# Options:
#   -h --help
#   -a --assuranceLevel=STRING  Assurance level [default: normal]
#   -n --name=STRING            Name of the wallet [default: TestWallet]
#   -a --account=INT            An optional parent account index [default: 2147483648]
#   -p --port=INT               Port the server is listening to [default: 8090]
#   -c --client=FILEPATH        TLS client certificate expected by the server [default: ../../../state-staging/tls/client/client.pem]
#
# Examples:
#   ./wallet.sh restore parrot ugly lock symbol sibling display bright border yellow pencil area dawn
#   ./wallet.sh create rival visa comfort stamp garbage trim cross awake hold aerobic bid glare
#   ./wallet.sh get
#   ./wallet.sh get Ae2tdPwUPEZKJycNa3AD1mBWThdXpJoyvdEksYBaJUUMoMN4XAfJ3h18Lib
#   ./wallet.sh delete Ae2tdPwUPEZKJycNa3AD1mBWThdXpJoyvdEksYBaJUUMoMN4XAfJ3h18Lib
#   ./wallet.sh get-addresses Ae2tdPwUPEZKyifkHShCKCbzzPbi7DBV7XuyTPEd13KRES4v8Ut3PmKebLy
#

PATH=.:$PATH; source docopts.sh --auto "$@"

if [ ${ARGS[MNEMONICS,#]} -ne 0 ]; then

	mnemonics_num=`expr ${ARGS[MNEMONICS,#]} - 1`
	mnemonics="\"${ARGS[MNEMONICS,0]}\""
	for i in $(seq 1 $mnemonics_num)
	do

 		mnemonics+=",\"${ARGS[MNEMONICS,${i}]}\""

	done
fi


# create
if [ "${ARGS[create]}" == "true" ]; then

	curl  -kX POST https://localhost:${ARGS[--port]}/api/v1/wallets \
  		-H "Accept: application/json; charset=utf-8" \
  		-H "Content-Type: application/json; charset=utf-8" \
  		--cert ${ARGS[--client]} \
  		-d "{
  		  \"operation\": \"create\",
  		  \"backupPhrase\":[${mnemonics}],
  		  \"assuranceLevel\": \"${ARGS[--assuranceLevel]}\",
  		  \"name\": \"${ARGS[--name]}\"
  		    }" | jq

fi

# restore
if [ "${ARGS[restore]}" == "true" ]; then

	curl  -kX POST https://localhost:${ARGS[--port]}/api/v1/wallets \
  		-H "Accept: application/json; charset=utf-8" \
  		-H "Content-Type: application/json; charset=utf-8" \
  		--cert ${ARGS[--client]} \
  		-d "{
  		  \"operation\": \"restore\",
  		  \"backupPhrase\":[${mnemonics}],
  		  \"assuranceLevel\": \"${ARGS[--assuranceLevel]}\",
  		  \"name\": \"${ARGS[--name]}\"
  		    }" | jq

fi

# delete
if [ "${ARGS[delete]}" == "true" ]; then

	curl  -kX DELETE https://localhost:${ARGS[--port]}/api/v1/wallets/${ARGS[<wallet_id>]} \
  		-H "Accept: application/json; charset=utf-8" \
  		-H "Content-Type: application/json; charset=utf-8" \
  		--cert ${ARGS[--client]} | jq

fi

# get
if [ "${ARGS[get]}" == "true" ]; then

	curl -skX GET https://localhost:${ARGS[--port]}/api/v1/wallets/${ARGS[<wallet_id>]} \
  		-H "Accept: application/json; charset=utf-8" \
  		-H "Content-Type: application/json; charset=utf-8" \
  		--cert ${ARGS[--client]} | jq

fi

# get-addresses
if [ "${ARGS[get-addresses]}" == "true" ]; then

	curl -kX GET https://localhost:${ARGS[--port]}/api/v1/wallets/${ARGS[<wallet_id>]}/accounts/${ARGS[--account]}/addresses \
  		-H "Accept: application/json; charset=utf-8" \
  		-H "Content-Type: application/json; charset=utf-8" \
  		--cert ${ARGS[--client]}

fi
