#!/bin/bash

data="$1"
keypath="$2"
name="$3"
txin="$4"
datum="$data/$5"
body="$data/groth-deploy.txbody"
tx="$data/groth-deploy.tx"

# Build gift address 
cardano-cli address build \
    --payment-script-file "$data/contractdata/GrothValidator.plutus" \
    --testnet-magic 2 \
    --out-file "$data/contractdata/GrothValidator.addr"

# Build the transaction
cardano-cli transaction build \
    --babbage-era \
    --testnet-magic 2 \
    --tx-in "$txin" \
    --tx-out "$(cat "$data/contractdata/GrothValidator.addr") + 2000001 lovelace" \
    --tx-out-inline-datum-file "$datum" \
    --change-address "$(cat "$keypath/$name.addr")" \
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