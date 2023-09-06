#!/bin/bash


data="$1"
keypath="$2"
name="$3"
txin="$4"
collateral="$5"
redeemer="$data/$6"
body="$data/groth-unlock.txbody"
tx="$data/groth-unlock.tx"
pp="$data/protocol-parameters.json"


# Query the protocol parameters \

cardano-cli query protocol-parameters \
    --testnet-magic 2 \
    --out-file "$pp"

# Build the transaction
cardano-cli transaction build \
    --babbage-era \
    --testnet-magic 2 \
    --tx-in "$txin" \
    --tx-in-script-file "$data/GrothValidator.plutus" \
    --tx-in-inline-datum-present \
    --tx-in-redeemer-file "$redeemer" \
    --tx-in-collateral "$collateral" \
    --change-address "$(cat "$keypath/$name.addr")" \
    --protocol-params-file "$pp" \
    --out-file "$body"
    
# Sign the transaction
cardano-cli transaction sign \
    --tx-body-file "$body" \
    --signing-key-file "$keypath/$name.skey" \
    --testnet-magic 2 \
    --out-file "$tx"

# Submit the transaction
cardano-cli transaction submit \
    --testnet-magic 2 \
    --tx-file "$tx"

tid=$(cardano-cli transaction txid --tx-file "$tx")
echo "transaction id: $tid"
echo "Cardanoscan: https://preview.cardanoscan.io/transaction/$tid"