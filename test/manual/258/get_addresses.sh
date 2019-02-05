#!/bin/bash

#
# The script gets 50k addresses from Daedalus wallet and puts it into a file. 
# The file can be then used as a parameter for a batch import request. 
# The file has the following content:
#
#  [
#    "address1",
#    "address2",
#     ....
#  ]
#
# Prerequisite for the script is to run create_addresses.sh first
#

port=46380 #Note: Daedalus port can be retrieved with the following command `lsof -i -P -n | grep LISTEN` (cardano-n port)
wallet_id="Ae2tdPwUPEZLXCZQkmwcatABSc5Fw1bp3dk5nBkNnkNHy6teLdxqQm8NPgW"
account_index=2147483648
filename="addresses50k.txt"

echo "[" >> $filename
for i in {1..1000}
do	
	url="https://localhost:$port/api/v1/wallets/$wallet_id/accounts/$account_index/addresses?per_page=50&page=$i"
	curl -kX GET $url \
  		-H "Accept: application/json; charset=utf-8" \
  		-H "Content-Type: application/json; charset=utf-8" \
  		--cert ~/.local/share/Daedalus/staging/tls/client/client.pem \
  		--cacert ~/.local/share/Daedalus/staging/tls/client/ca.crt --http1.1 | python -m json.tool | grep id | sed -e 's/"id"://g' | tr -s '[:blank:]' >> $filename

done

truncate -s-2 $filename
echo     >> $filename
echo "]" >> $filename

