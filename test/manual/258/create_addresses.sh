#!/bin/bash

port=46380 # Note: Daedalus port can be retrieved with the following command `lsof -i -P -n | grep LISTEN` (cardano-n port)
wallet_id="Ae2tdPwUPEZLXCZQkmwcatABSc5Fw1bp3dk5nBkNnkNHy6teLdxqQm8NPgW"
account_index=2147483648

for i in {1..50000}
do

	echo "Address no: $i"

	curl -X POST https://localhost:$port/api/v1/addresses \
  		-H "Accept: application/json; charset=utf-8" \
  		-H "Content-Type: application/json; charset=utf-8" \
  		--cert ~/.local/share/Daedalus/staging/tls/client/client.pem \
 		--cacert ~/.local/share/Daedalus/staging/tls/client/ca.crt  \
  		-d '{
		"accountIndex": '"$account_index"',
		"walletId": "'"$wallet_id"'"
		}'

done
