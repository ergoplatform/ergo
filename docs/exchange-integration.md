# Ergo Exchange Integration Guide
## Overview
This guide provides technical steps for exchanges integrating the Ergo Node Wallet API.
### 1. Node Synchronization
Ensure your node is fully synced with the mainnet before attempting wallet operations.
### 2. Generating Deposit Addresses
Use the `/wallet/address/derive` endpoint to generate unique UTXO-based addresses for users.
### 3. Deposit Recognition
Scan the UTXO set for incoming transactions. Confirmations should be weighted based on the transaction value.
