# Ergo Wallet API and Exchange Integration Documentation

## Table of Contents

1. [Introduction](#introduction)
2. [Getting Started](#getting-started)
3. [Wallet Initialization](#wallet-initialization)
4. [Basic Wallet Operations](#basic-wallet-operations)
5. [Transaction Creation](#transaction-creation)
6. [Exchange Integration Workflow](#exchange-integration-workflow)
7. [Advanced Topics and Best Practices](#advanced-topics-and-best-practices)

---

## 1. Introduction

### What is the Ergo Wallet API?

The Ergo node includes a built-in wallet with a REST API that enables applications, exchanges, and services to interact with the Ergo blockchain. This documentation provides comprehensive guidance for:

- **Wallet management**: Create, restore, and manage wallets
- **Transaction handling**: Create, sign, and broadcast transactions
- **Exchange integration**: Deposit detection, withdrawal processing
- **Mining operations**: Pool payouts and miner withdrawals

### Key Features

- **Extended UTXO Model**: Advanced transaction capabilities with ErgoScript
- **Multi-signature support**: Secure multi-party transactions
- **Token management**: Native support for custom tokens
- **Secure key storage**: Encrypted wallet storage
- **Stateless client support**: Light client capabilities

### API Access

The wallet API is accessible via HTTP REST endpoints on the Ergo node:

- **Default URL**: `http://127.0.0.1:9053`
- **API Documentation**: `http://127.0.0.1:9053/swagger` (when node is running)
- **Authentication**: API key required for most endpoints

---

## 2. Getting Started

### 2.1 Node Installation and Configuration

#### Installing the Ergo Node

**Option 1: Download Pre-built Binary**

```bash
# Download latest release
wget https://github.com/ergoplatform/ergo/releases/latest/download/ergo-[VERSION].jar

# Run the node
java -jar -Xmx4G ergo-[VERSION].jar --mainnet
```

**Option 2: Build from Source**

```bash
# Clone repository
git clone https://github.com/ergoplatform/ergo.git
cd ergo

# Build with SBT
sbt assembly

# Run
java -jar target/scala-2.12/ergo-[VERSION].jar --mainnet
```

**Option 3: Docker**

```bash
# Pull and run
docker run -p 9053:9053 -p 9030:9030 \
  -v /path/to/ergo/data:/home/ergo/.ergo \
  ergoplatform/ergo:latest --mainnet
```

### 2.2 Node Configuration

Create a configuration file `application.conf`:

```hocon
ergo {
  node {
    # Node mining setting (false for exchanges)
    mining = false
  }
  
  wallet {
    # Mnemonic seed strength in bits (128, 160, 192, 224, 256)
    seedStrengthBits = 256
    
    # Mnemonic password (empty string by default)
    mnemonicPhraseLanguage = "english"
    
    # Number of keys to be generated
    defaultTransactionFee = 1000000
    
    # Whether to use pre-EIP3 key derivation
    usePreEip3Derivation = false
  }
}

scorex {
  restApi {
    # API endpoint
    bindAddress = "0.0.0.0:9053"
    
    # API key hash (use /utils/hash/blake2b to generate)
    apiKeyHash = "YOUR_API_KEY_HASH_HERE"
  }
  
  network {
    # Node name
    nodeName = "ergo-exchange-node"
    
    # Known peers (mainnet)
    knownPeers = [
      "213.239.193.208:9030",
      "159.65.11.55:9030",
      "165.227.26.175:9030",
      "159.89.116.15:9030"
    ]
  }
}
```

### 2.3 Generating API Key

The API requires authentication using Blake2b hash of your API key.

**Step 1: Choose a strong API key**

```bash
# Example API key (use your own secure random string)
API_KEY="your-secure-random-api-key-here"
```

**Step 2: Generate hash**

After starting the node, use the `/utils/hash/blake2b` endpoint:

```bash
curl -X POST "http://127.0.0.1:9053/utils/hash/blake2b" \
  -H "Content-Type: application/json" \
  -d "\"your-secure-random-api-key-here\""
```

Response:
```json
"1a2b3c4d5e6f7g8h9i0j..."
```

**Step 3: Add hash to configuration**

Update `application.conf`:
```hocon
scorex.restApi.apiKeyHash = "1a2b3c4d5e6f7g8h9i0j..."
```

**Step 4: Restart node and use API key in requests**

```bash
curl -X GET "http://127.0.0.1:9053/wallet/status" \
  -H "api_key: your-secure-random-api-key-here"
```

### 2.4 Node Synchronization

Before using the wallet, ensure the node is fully synchronized:

```bash
# Check sync status
curl -X GET "http://127.0.0.1:9053/info" \
  -H "api_key: YOUR_API_KEY"
```

Response:
```json
{
  "name": "ergo-exchange-node",
  "appVersion": "5.0.0",
  "fullHeight": 1000000,
  "headersHeight": 1000000,
  "maxPeerHeight": 1000000,
  "bestFullHeaderId": "a1b2c3...",
  "stateType": "utxo",
  "isMining": false,
  "peersCount": 20,
  "unconfirmedCount": 5
}
```

**Sync is complete when**: `fullHeight` ≈ `headersHeight` ≈ `maxPeerHeight`

---

## 3. Wallet Initialization

### 3.1 Creating a New Wallet

**Endpoint**: `POST /wallet/init`

**Request**:
```bash
curl -X POST "http://127.0.0.1:9053/wallet/init" \
  -H "api_key: YOUR_API_KEY" \
  -H "Content-Type: application/json" \
  -d '{
    "pass": "my-secure-wallet-password",
    "mnemonicPass": ""
  }'
```

**Response**:
```json
{
  "mnemonic": "word1 word2 word3 word4 word5 word6 word7 word8 word9 word10 word11 word12 word13 word14 word15"
}
```

**⚠️ CRITICAL**: 
- Store the mnemonic phrase securely offline
- Never share or expose the mnemonic
- This is the ONLY way to recover your wallet
- Loss of mnemonic = permanent loss of funds

### 3.2 Restoring an Existing Wallet

**Endpoint**: `POST /wallet/restore`

**Request**:
```bash
curl -X POST "http://127.0.0.1:9053/wallet/restore" \
  -H "api_key: YOUR_API_KEY" \
  -H "Content-Type: application/json" \
  -d '{
    "pass": "my-secure-wallet-password",
    "mnemonic": "word1 word2 word3 word4 word5 word6 word7 word8 word9 word10 word11 word12 word13 word14 word15",
    "mnemonicPass": ""
  }'
```

**Response**:
```json
{
  "success": true
}
```

### 3.3 Unlocking the Wallet

After node restart, the wallet must be unlocked:

**Endpoint**: `POST /wallet/unlock`

**Request**:
```bash
curl -X POST "http://127.0.0.1:9053/wallet/unlock" \
  -H "api_key: YOUR_API_KEY" \
  -H "Content-Type: application/json" \
  -d '{
    "pass": "my-secure-wallet-password"
  }'
```

**Response**:
```json
{
  "success": true
}
```

### 3.4 Locking the Wallet

For security, lock the wallet when not in use:

**Endpoint**: `POST /wallet/lock`

**Request**:
```bash
curl -X GET "http://127.0.0.1:9053/wallet/lock" \
  -H "api_key: YOUR_API_KEY"
```

**Response**:
```json
{
  "success": true
}
```

---

## 4. Basic Wallet Operations

### 4.1 Checking Wallet Status

**Endpoint**: `GET /wallet/status`

**Request**:
```bash
curl -X GET "http://127.0.0.1:9053/wallet/status" \
  -H "api_key: YOUR_API_KEY"
```

**Response**:
```json
{
  "isInitialized": true,
  "isUnlocked": true,
  "changeAddress": "9f4QF8AD1nQ3nJahQVkMj8hFSVVzVom77b52JU7EW71Zexg6N8v",
  "walletHeight": 950000,
  "error": ""
}
```

**Fields**:
- `isInitialized`: Wallet has been created/restored
- `isUnlocked`: Wallet is unlocked and ready for operations
- `changeAddress`: Default address for receiving change
- `walletHeight`: Last block height processed by wallet

### 4.2 Getting Wallet Addresses

**Endpoint**: `GET /wallet/addresses`

**Request**:
```bash
curl -X GET "http://127.0.0.1:9053/wallet/addresses" \
  -H "api_key: YOUR_API_KEY"
```

**Response**:
```json
[
  "9f4QF8AD1nQ3nJahQVkMj8hFSVVzVom77b52JU7EW71Zexg6N8v",
  "9fRusAarL1KkrVdRXrJhmJeTWzJPkL6mdXW2VPJvj8L3kQy7BXm",
  "9g1p8r2jQ3x4L5nW8m9hK6tY7uR2vX4c5nB3aM8pQ9sT2eR7fH4"
]
```

### 4.3 Generating New Addresses

The wallet automatically derives addresses from the mnemonic seed.

**Endpoint**: `GET /wallet/deriveNextKey`

**Request**:
```bash
curl -X GET "http://127.0.0.1:9053/wallet/deriveNextKey" \
  -H "api_key: YOUR_API_KEY"
```

**Response**:
```json
{
  "derivationPath": "m/44'/429'/0'/0/5",
  "address": "9g1p8r2jQ3x4L5nW8m9hK6tY7uR2vX4c5nB3aM8pQ9sT2eR7fH4"
}
```

**Derivation Path**: Follows BIP-32/BIP-44 standard
- `m/44'/429'/0'/0/x` - External (receiving) addresses
- `m/44'/429'/0'/1/x` - Internal (change) addresses

### 4.4 Checking Balance

**Endpoint**: `GET /wallet/balances`

**Request**:
```bash
curl -X GET "http://127.0.0.1:9053/wallet/balances" \
  -H "api_key: YOUR_API_KEY"
```

**Response**:
```json
{
  "height": 950000,
  "balance": 100000000000,
  "assets": [
    {
      "tokenId": "1234567890abcdef1234567890abcdef1234567890abcdef1234567890abcdef",
      "amount": 1000,
      "decimals": 2,
      "name": "Example Token",
      "tokenType": "EIP-004"
    }
  ]
}
```

**Units**: 
- ERG amounts are in **nanoERG** (1 ERG = 1,000,000,000 nanoERG)
- Example: `100000000000` nanoERG = 100 ERG

### 4.5 Getting Wallet Transactions

**Endpoint**: `GET /wallet/transactions`

**Request with pagination**:
```bash
curl -X GET "http://127.0.0.1:9053/wallet/transactions?minInclusionHeight=0&maxInclusionHeight=1000000&minConfirmations=0&maxConfirmations=1000000" \
  -H "api_key: YOUR_API_KEY"
```

**Response**:
```json
[
  {
    "id": "abc123...",
    "inputs": [...],
    "outputs": [...],
    "inclusionHeight": 949500,
    "timestamp": 1640000000000,
    "confirmationsNum": 500
  }
]
```

### 4.6 Getting Unspent Boxes (UTXOs)

**Endpoint**: `GET /wallet/boxes/unspent`

**Request**:
```bash
curl -X GET "http://127.0.0.1:9053/wallet/boxes/unspent?minConfirmations=0&minInclusionHeight=0" \
  -H "api_key: YOUR_API_KEY"
```

**Response**:
```json
[
  {
    "boxId": "d2a7ac1b2e3f4g5h6i7j8k9l0m1n2o3p4q5r6s7t8u9v0w1x2y3z4a5b6c7d8e9f",
    "value": 50000000000,
    "ergoTree": "0008cd...",
    "creationHeight": 949000,
    "assets": [],
    "additionalRegisters": {},
    "transactionId": "tx123...",
    "index": 0,
    "address": "9f4QF8AD1nQ3nJahQVkMj8hFSVVzVom77b52JU7EW71Zexg6N8v",
    "confirmations": 1000
  }
]
```

**Understanding Boxes**:
- Each box is an unspent output (UTXO)
- `value`: Amount in nanoERG
- `assets`: Tokens contained in the box
- `ergoTree`: Spending condition (smart contract)
- `confirmations`: Number of blocks since creation

---

## 5. Transaction Creation

### 5.1 Simple Payment Transaction

The most common operation is sending ERG to another address.

**Endpoint**: `POST /wallet/payment/send`

**Request**:
```bash
curl -X POST "http://127.0.0.1:9053/wallet/payment/send" \
  -H "api_key: YOUR_API_KEY" \
  -H "Content-Type: application/json" \
  -d '{
    "address": "9f4QF8AD1nQ3nJahQVkMj8hFSVVzVom77b52JU7EW71Zexg6N8v",
    "value": 1000000000,
    "fee": 1000000
  }'
```

**Parameters**:
- `address`: Recipient's Ergo address (P2PK format)
- `value`: Amount in nanoERG (1000000000 = 1 ERG)
- `fee`: Transaction fee in nanoERG (optional, default: 1000000)

**Response**:
```json
{
  "id": "a1b2c3d4e5f6g7h8i9j0k1l2m3n4o5p6q7r8s9t0u1v2w3x4y5z6a7b8c9d0e1f2",
  "inputs": [...],
  "dataInputs": [],
  "outputs": [...],
  "size": 342
}
```

The transaction is automatically signed and broadcast to the network.

### 5.2 Sending Multiple Payments

Send ERG to multiple recipients in a single transaction.

**Endpoint**: `POST /wallet/transaction/send`

**Request**:
```bash
curl -X POST "http://127.0.0.1:9053/wallet/transaction/send" \
  -H "api_key: YOUR_API_KEY" \
  -H "Content-Type: application/json" \
  -d '{
    "requests": [
      {
        "address": "9f4QF8AD1nQ3nJahQVkMj8hFSVVzVom77b52JU7EW71Zexg6N8v",
        "value": 1000000000
      },
      {
        "address": "9fRusAarL1KkrVdRXrJhmJeTWzJPkL6mdXW2VPJvj8L3kQy7BXm",
        "value": 2000000000
      },
      {
        "address": "9g1p8r2jQ3x4L5nW8m9hK6tY7uR2vX4c5nB3aM8pQ9sT2eR7fH4",
        "value": 3000000000
      }
    ],
    "fee": 1100000,
    "inputsRaw": [],
    "dataInputsRaw": []
  }'
```

**Response**:
```json
{
  "id": "tx_id_here",
  "inputs": [...],
  "dataInputs": [],
  "outputs": [...]
}
```

**Benefits of Batch Payments**:
- Lower total fees (one transaction instead of multiple)
- Atomic execution (all payments succeed or fail together)
- Reduced blockchain bloat

### 5.3 Working with Tokens

Ergo supports native tokens without smart contracts. Each box can contain multiple tokens.

#### 5.3.1 Sending Tokens

**Endpoint**: `POST /wallet/transaction/send`

**Request**:
```bash
curl -X POST "http://127.0.0.1:9053/wallet/transaction/send" \
  -H "api_key: YOUR_API_KEY" \
  -H "Content-Type: application/json" \
  -d '{
    "requests": [
      {
        "address": "9f4QF8AD1nQ3nJahQVkMj8hFSVVzVom77b52JU7EW71Zexg6N8v",
        "value": 1000000,
        "assets": [
          {
            "tokenId": "1234567890abcdef1234567890abcdef1234567890abcdef1234567890abcdef",
            "amount": 100
          }
        ]
      }
    ],
    "fee": 1000000
  }'
```

**Important Notes**:
- `value` must include minimum ERG (usually 0.001 ERG = 1,000,000 nanoERG)
- Token amounts are in the token's base unit (check `decimals`)
- Multiple tokens can be sent in one transaction

#### 5.3.2 Issuing New Tokens

Tokens are created by including them in a transaction output where they didn't exist before.

**Endpoint**: `POST /wallet/transaction/send`

**Request**:
```bash
curl -X POST "http://127.0.0.1:9053/wallet/transaction/send" \
  -H "api_key: YOUR_API_KEY" \
  -H "Content-Type: application/json" \
  -d '{
    "requests": [
      {
        "address": "9f4QF8AD1nQ3nJahQVkMj8hFSVVzVom77b52JU7EW71Zexg6N8v",
        "value": 1000000,
        "assets": [
          {
            "tokenId": "TOKEN_ID_FROM_INPUT_BOX",
            "amount": 1000000,
            "name": "My Token",
            "description": "My custom token on Ergo",
            "decimals": 2
          }
        ],
        "registers": {
          "R4": "0e0a4d7920546f6b656e",
          "R5": "0e1e4d7920637573746f6d20746f6b656e206f6e204572676f",
          "R6": "0e0132"
        }
      }
    ],
    "fee": 1000000
  }'
```

**Token ID**: The token ID is the ID of the first input box in the minting transaction.

**Registers for Token Metadata** (EIP-004):
- **R4**: Token name (hex-encoded)
- **R5**: Token description (hex-encoded)
- **R6**: Token decimals (hex-encoded)

**Example**: Encoding token metadata
```bash
# Name: "My Token"
echo -n "My Token" | xxd -p
# Output: 4d7920546f6b656e
# With length prefix: 0e0a4d7920546f6b656e

# Decimals: 2
echo -n $'\x02' | xxd -p
# With type prefix: 0e0132
```

### 5.4 Transaction Fees

Understanding and calculating appropriate fees is crucial for transaction confirmation.

#### 5.4.1 Default Fee Structure

**Minimum Fee**: 1,000,000 nanoERG (0.001 ERG)

**Fee Calculation Factors**:
- Transaction size (bytes)
- Number of inputs and outputs
- Network congestion
- Computational complexity (for smart contracts)

#### 5.4.2 Recommended Fees

```
Simple payment:        1,000,000 nanoERG (0.001 ERG)
Multi-output (2-5):    1,100,000 nanoERG (0.0011 ERG)
Multi-output (6-10):   1,500,000 nanoERG (0.0015 ERG)
With tokens:           1,500,000 nanoERG (0.0015 ERG)
Complex contracts:     2,000,000+ nanoERG (0.002+ ERG)
```

#### 5.4.3 Checking Current Recommended Fee

**Endpoint**: `GET /wallet/payment/fee`

**Request**:
```bash
curl -X GET "http://127.0.0.1:9053/wallet/payment/fee" \
  -H "api_key: YOUR_API_KEY"
```

**Response**:
```json
{
  "value": 1000000
}
```

### 5.5 Unsigned Transaction Creation

For advanced use cases or offline signing, you can generate unsigned transactions.

**Endpoint**: `POST /wallet/transaction/generate`

**Request**:
```bash
curl -X POST "http://127.0.0.1:9053/wallet/transaction/generate" \
  -H "api_key: YOUR_API_KEY" \
  -H "Content-Type: application/json" \
  -d '{
    "requests": [
      {
        "address": "9f4QF8AD1nQ3nJahQVkMj8hFSVVzVom77b52JU7EW71Zexg6N8v",
        "value": 1000000000
      }
    ],
    "fee": 1000000,
    "inputsRaw": [],
    "dataInputsRaw": []
  }'
```

**Response** (unsigned transaction):
```json
{
  "id": "unsigned_tx_id",
  "inputs": [
    {
      "boxId": "box_id_here",
      "spendingProof": {
        "proofBytes": "",
        "extension": {}
      }
    }
  ],
  "dataInputs": [],
  "outputs": [
    {
      "boxId": null,
      "value": 1000000000,
      "ergoTree": "0008cd...",
      "assets": [],
      "creationHeight": 950000,
      "additionalRegisters": {},
      "transactionId": null,
      "index": null
    }
  ]
}
```

### 5.6 Transaction Signing

#### 5.6.1 Signing with Wallet

**Endpoint**: `POST /wallet/transaction/sign`

**Request**:
```bash
curl -X POST "http://127.0.0.1:9053/wallet/transaction/sign" \
  -H "api_key: YOUR_API_KEY" \
  -H "Content-Type: application/json" \
  -d '{
    "tx": {
      "id": "unsigned_tx_id",
      "inputs": [...],
      "dataInputs": [],
      "outputs": [...]
    },
    "secrets": {},
    "inputsRaw": [],
    "dataInputsRaw": []
  }'
```

**Response** (signed transaction):
```json
{
  "id": "signed_tx_id",
  "inputs": [
    {
      "boxId": "box_id_here",
      "spendingProof": {
        "proofBytes": "a1b2c3d4e5f6...",
        "extension": {}
      }
    }
  ],
  "dataInputs": [],
  "outputs": [...]
}
```

#### 5.6.2 External Signing (Hardware Wallets, etc.)

For external signing, you need:
1. The unsigned transaction
2. The private key or signing device
3. Sigma protocol implementation

**Process**:
```
1. Generate unsigned transaction → GET from /wallet/transaction/generate
2. Export to external signer → Convert to appropriate format
3. Sign with external tool → Use sigma-rust or similar
4. Import signed transaction → POST to /wallet/transaction/submit
```

### 5.7 Broadcasting Transactions

#### 5.7.1 Submit Signed Transaction

**Endpoint**: `POST /wallet/transaction/submit`

**Request**:
```bash
curl -X POST "http://127.0.0.1:9053/wallet/transaction/submit" \
  -H "api_key: YOUR_API_KEY" \
  -H "Content-Type: application/json" \
  -d '{
    "id": "signed_tx_id",
    "inputs": [...],
    "dataInputs": [],
    "outputs": [...]
  }'
```

**Response**:
```json
{
  "id": "a1b2c3d4e5f6g7h8i9j0k1l2m3n4o5p6q7r8s9t0u1v2w3x4y5z6a7b8c9d0e1f2"
}
```

#### 5.7.2 Check Transaction Status

**Endpoint**: `GET /blockchain/transaction/byId/{txId}`

**Request**:
```bash
curl -X GET "http://127.0.0.1:9053/blockchain/transaction/byId/a1b2c3d4e5f6g7h8i9j0k1l2m3n4o5p6q7r8s9t0u1v2w3x4y5z6a7b8c9d0e1f2" \
  -H "api_key: YOUR_API_KEY"
```

**Response**:
```json
{
  "id": "a1b2c3d4e5f6g7h8i9j0k1l2m3n4o5p6q7r8s9t0u1v2w3x4y5z6a7b8c9d0e1f2",
  "inputs": [...],
  "dataInputs": [],
  "outputs": [...],
  "inclusionHeight": 950500,
  "timestamp": 1640000000000,
  "index": 15,
  "globalIndex": 15000000,
  "numConfirmations": 100,
  "blockId": "block_id_here",
  "size": 342
}
```

**Transaction States**:
- **Not found**: Transaction not in mempool or blockchain
- **In mempool**: Transaction received but not yet mined (`inclusionHeight` is null)
- **Confirmed**: Transaction included in blockchain (`inclusionHeight` is set)
- **Fully confirmed**: Transaction has sufficient confirmations (typically 10+)

### 5.8 Multi-Signature Transactions

Ergo supports complex multi-signature schemes through ErgoScript.

#### 5.8.1 Creating Multi-Sig Address

**Example: 2-of-3 Multi-Signature**

```scala
// ErgoScript for 2-of-3 multisig
{
  atLeast(2, Coll(
    PK("pubkey1"),
    PK("pubkey2"),
    PK("pubkey3")
  ))
}
```

**Using the API to compile ErgoScript**:

**Endpoint**: `POST /script/p2sAddress`

**Request**:
```bash
curl -X POST "http://127.0.0.1:9053/script/p2sAddress" \
  -H "api_key: YOUR_API_KEY" \
  -H "Content-Type: application/json" \
  -d '{
    "source": "{ atLeast(2, Coll(PK(\"9f4QF8AD1nQ3nJahQVkMj8hFSVVzVom77b52JU7EW71Zexg6N8v\"), PK(\"9fRusAarL1KkrVdRXrJhmJeTWzJPkL6mdXW2VPJvj8L3kQy7BXm\"), PK(\"9g1p8r2jQ3x4L5nW8m9hK6tY7uR2vX4c5nB3aM8pQ9sT2eR7fH4\"))) }"
  }'
```

**Response**:
```json
{
  "address": "2Z4YBkDsDvQj8BX7xiySFewjitqp2ge9c99jfes2whbtKitZTxdBYqbrVZUvZvKv6aqn9by4kp3LE1c26LjbXgf4NLoFGGxb42MRYwRUbZ7jZrLp9t3v5LZyj6"
}
```

#### 5.8.2 Signing Multi-Sig Transactions

**Process**:
1. Generate unsigned transaction
2. Each signer signs independently
3. Combine signatures
4. Submit transaction

**Signing by first party**:
```bash
curl -X POST "http://127.0.0.1:9053/wallet/transaction/sign" \
  -H "api_key: PARTY1_API_KEY" \
  -H "Content-Type: application/json" \
  -d '{
    "tx": {...unsigned_transaction...}
  }'
```

**Signing by additional parties**: Each party adds their signature to the `spendingProof`.

### 5.9 Transaction with Data Inputs

Data inputs allow reading box data without spending it.

**Use Case**: Oracle data, price feeds, external state

**Endpoint**: `POST /wallet/transaction/send`

**Request**:
```bash
curl -X POST "http://127.0.0.1:9053/wallet/transaction/send" \
  -H "api_key: YOUR_API_KEY" \
  -H "Content-Type: application/json" \
  -d '{
    "requests": [
      {
        "address": "9f4QF8AD1nQ3nJahQVkMj8hFSVVzVom77b52JU7EW71Zexg6N8v",
        "value": 1000000000
      }
    ],
    "fee": 1000000,
    "inputsRaw": [],
    "dataInputsRaw": ["oracle_box_id_here"]
  }'
```

**Benefits**:
- Read oracle data without spending
- Multiple transactions can use same data input simultaneously
- Lower fees (data inputs are cheaper than regular inputs)

### 5.10 Transaction Error Handling

Common transaction errors and solutions:

#### Error: "Not enough coins in wallet"
```json
{
  "error": 400,
  "reason": "Not enough coins in wallet",
  "detail": "..."
}
```
**Solution**: Ensure wallet has sufficient ERG for amount + fee

#### Error: "Not enough tokens"
```json
{
  "error": 400,
  "reason": "Not enough tokens",
  "detail": "..."
}
```
**Solution**: Check token balance with `/wallet/balances`

#### Error: "Transaction is invalid"
```json
{
  "error": 400,
  "reason": "Transaction is invalid",
  "detail": "..."
}
```
**Solutions**:
- Check all outputs have minimum ERG value
- Verify addresses are valid
- Ensure inputs haven't been spent
- Check script conditions are satisfied

#### Error: "Transaction too large"
```json
{
  "error": 400,
  "reason": "Transaction size exceeds limit",
  "detail": "..."
}
```
**Solution**: Split into multiple smaller transactions

---

## 6. Exchange Integration Workflow

### 6.1 Overview

Integrating Ergo into an exchange requires handling:
- **Deposits**: Users sending ERG/tokens to exchange-controlled addresses
- **Withdrawals**: Users requesting ERG/tokens to external addresses
- **Balance tracking**: Maintaining accurate user balances
- **Security**: Protecting hot and cold wallets
- **Monitoring**: Detecting and handling edge cases

### 6.2 Architecture Overview

**Recommended Architecture**:

```
┌─────────────────┐         ┌──────────────────┐         ┌─────────────────┐
│                 │         │                  │         │                 │
│  User Deposits  │────────▶│  Ergo Full Node  │────────▶│  Hot Wallet     │
│                 │         │  + Wallet API    │         │  (< 5% funds)   │
└─────────────────┘         └──────────────────┘         └─────────────────┘
                                     │                            │
                                     │                            │
                                     ▼                            ▼
                            ┌──────────────────┐         ┌─────────────────┐
                            │                  │         │                 │
                            │  Exchange DB     │◀────────│  Cold Wallet    │
                            │  (PostgreSQL)    │         │  (> 95% funds)  │
                            │                  │         │  (Offline)      │
                            └──────────────────┘         └─────────────────┘
                                     │
                                     │
                                     ▼
                            ┌──────────────────┐
                            │                  │
                            │  Withdrawal      │
                            │  Processing      │
                            │  Service         │
                            └──────────────────┘
```

### 6.3 Address Management

#### 6.3.1 Unique Address per User

**Best Practice**: Generate unique deposit address for each user.

**Implementation**:

```python
import requests

def generate_user_deposit_address(user_id, api_key):
    """Generate a new deposit address for a user"""
    
    # Derive next address from wallet
    response = requests.get(
        "http://127.0.0.1:9053/wallet/deriveNextKey",
        headers={"api_key": api_key}
    )
    
    if response.status_code == 200:
        address_data = response.json()
        address = address_data["address"]
        derivation_path = address_data["derivationPath"]
        
        # Store in database
        save_to_db(user_id, address, derivation_path)
        
        return address
    else:
        raise Exception(f"Failed to generate address: {response.text}")
```

**Database Schema**:
```sql
CREATE TABLE user_addresses (
    id SERIAL PRIMARY KEY,
    user_id INTEGER NOT NULL,
    address VARCHAR(64) NOT NULL UNIQUE,
    derivation_path VARCHAR(64),
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    INDEX idx_address (address),
    INDEX idx_user_id (user_id)
);
```

#### 6.3.2 Alternative: Shared Address with Payment ID

**Not Recommended for Ergo**: Ergo doesn't have built-in payment IDs like Monero. However, you can use:
- Additional registers in outputs
- Unique address per user (recommended)

### 6.4 Deposit Detection and Processing

#### 6.4.1 Monitoring for Deposits

**Method 1: Polling Wallet Transactions**

```python
import requests
import time

def poll_new_deposits(api_key, last_height):
    """Poll for new deposits since last check"""
    
    response = requests.get(
        "http://127.0.0.1:9053/wallet/transactions",
        headers={"api_key": api_key},
        params={
            "minInclusionHeight": last_height,
            "minConfirmations": 0
        }
    )
    
    if response.status_code == 200:
        transactions = response.json()
        return transactions
    else:
        raise Exception(f"Failed to get transactions: {response.text}")

def process_deposits(api_key, db_connection):
    """Main deposit processing loop"""
    
    last_processed_height = get_last_processed_height(db_connection)
    
    while True:
        try:
            # Get new transactions
            transactions = poll_new_deposits(api_key, last_processed_height)
            
            for tx in transactions:
                process_transaction_for_deposits(tx, db_connection)
            
            # Update last processed height
            current_height = get_current_height(api_key)
            update_last_processed_height(db_connection, current_height)
            last_processed_height = current_height
            
            # Wait before next poll
            time.sleep(30)  # Poll every 30 seconds
            
        except Exception as e:
            print(f"Error processing deposits: {e}")
            time.sleep(60)
```

**Method 2: Monitoring Specific Addresses**

```python
def check_address_balance(address, api_key):
    """Check balance for specific address"""
    
    response = requests.post(
        "http://127.0.0.1:9053/blockchain/box/unspent/byAddress",
        headers={"api_key": api_key},
        json={"address": address}
    )
    
    if response.status_code == 200:
        boxes = response.json()
        
        total_erg = sum(box["value"] for box in boxes)
        
        # Process tokens
        tokens = {}
        for box in boxes:
            for asset in box.get("assets", []):
                token_id = asset["tokenId"]
                amount = asset["amount"]
                tokens[token_id] = tokens.get(token_id, 0) + amount
        
        return {
            "erg": total_erg,
            "tokens": tokens,
            "boxes": boxes
        }
    else:
        raise Exception(f"Failed to check balance: {response.text}")
```

#### 6.4.2 Processing Deposit Transactions

```python
def process_transaction_for_deposits(tx, db_connection):
    """Process a transaction to detect deposits"""
    
    tx_id = tx["id"]
    confirmations = tx.get("confirmationsNum", 0)
    
    # Skip if already processed
    if is_transaction_processed(tx_id, db_connection):
        return
    
    # Check each output
    for output_idx, output in enumerate(tx["outputs"]):
        address = output.get("address")
        value = output["value"]
        
        # Check if address belongs to a user
        user_id = get_user_by_address(address, db_connection)
        
        if user_id:
            # Process ERG deposit
            process_erg_deposit(
                user_id=user_id,
                tx_id=tx_id,
                address=address,
                amount=value,
                confirmations=confirmations,
                output_index=output_idx,
                db_connection=db_connection
            )
            
            # Process token deposits
            for asset in output.get("assets", []):
                process_token_deposit(
                    user_id=user_id,
                    tx_id=tx_id,
                    address=address,
                    token_id=asset["tokenId"],
                    amount=asset["amount"],
                    confirmations=confirmations,
                    output_index=output_idx,
                    db_connection=db_connection
                )
```

#### 6.4.3 Confirmation Requirements

**Recommended Confirmations**:
- **Small amounts (< 10 ERG)**: 10 confirmations (~20 minutes)
- **Medium amounts (10-100 ERG)**: 20 confirmations (~40 minutes)
- **Large amounts (> 100 ERG)**: 30+ confirmations (~60+ minutes)
- **Critical/High-value**: 100+ confirmations (~3+ hours)

**Why Multiple Confirmations?**
- Protection against chain reorganization
- Protection against double-spend attacks
- Industry standard for security

**Implementation**:

```python
def get_required_confirmations(amount_erg):
    """Determine required confirmations based on amount"""
    
    if amount_erg < 10:
        return 10
    elif amount_erg < 100:
        return 20
    elif amount_erg < 1000:
        return 30
    else:
        return 100

def update_deposit_confirmations(db_connection):
    """Update confirmations for pending deposits"""
    
    pending_deposits = get_pending_deposits(db_connection)
    
    for deposit in pending_deposits:
        tx_id = deposit["tx_id"]
        current_confirmations = get_transaction_confirmations(tx_id)
        required_confirmations = get_required_confirmations(deposit["amount_erg"])
        
        # Update confirmations in database
        update_deposit_confirmation_count(
            deposit["id"],
            current_confirmations,
            db_connection
        )
        
        # Credit user account if sufficient confirmations
        if current_confirmations >= required_confirmations:
            if not deposit["credited"]:
                credit_user_account(
                    deposit["user_id"],
                    deposit["amount_erg"],
                    deposit["token_id"],
                    deposit["token_amount"],
                    db_connection
                )
                mark_deposit_credited(deposit["id"], db_connection)
```

#### 6.4.4 Database Schema for Deposits

```sql
-- Deposits table
CREATE TABLE deposits (
    id SERIAL PRIMARY KEY,
    user_id INTEGER NOT NULL,
    address VARCHAR(64) NOT NULL,
    tx_id VARCHAR(64) NOT NULL,
    output_index INTEGER NOT NULL,
    amount_erg BIGINT NOT NULL,
    token_id VARCHAR(64),
    token_amount BIGINT,
    confirmations INTEGER DEFAULT 0,
    required_confirmations INTEGER NOT NULL,
    credited BOOLEAN DEFAULT FALSE,
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    credited_at TIMESTAMP,
    UNIQUE(tx_id, output_index),
    INDEX idx_user_id (user_id),
    INDEX idx_tx_id (tx_id),
    INDEX idx_credited (credited)
);

-- User balances table
CREATE TABLE user_balances (
    id SERIAL PRIMARY KEY,
    user_id INTEGER NOT NULL UNIQUE,
    erg_balance BIGINT DEFAULT 0,
    erg_locked BIGINT DEFAULT 0,
    updated_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
);

-- User token balances table
CREATE TABLE user_token_balances (
    id SERIAL PRIMARY KEY,
    user_id INTEGER NOT NULL,
    token_id VARCHAR(64) NOT NULL,
    balance BIGINT DEFAULT 0,
    locked BIGINT DEFAULT 0,
    updated_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    UNIQUE(user_id, token_id),
    INDEX idx_user_token (user_id, token_id)
);
```

### 6.5 Withdrawal Processing

#### 6.5.1 Withdrawal Request Validation

```python
def validate_withdrawal_request(user_id, address, amount_erg, db_connection):
    """Validate withdrawal request"""
    
    # Check if address is valid
    if not is_valid_ergo_address(address):
        return False, "Invalid Ergo address"
    
    # Check user balance
    user_balance = get_user_balance(user_id, db_connection)
    
    # Include withdrawal fee
    withdrawal_fee = 2000000  # 0.002 ERG
    total_needed = amount_erg + withdrawal_fee
    
    if user_balance < total_needed:
        return False, "Insufficient balance"
    
    # Check minimum withdrawal
    min_withdrawal = 1000000  # 0.001 ERG
    if amount_erg < min_withdrawal:
        return False, f"Minimum withdrawal is {min_withdrawal / 1e9} ERG"
    
    # Check maximum withdrawal (anti-fraud)
    max_withdrawal = 1000000000000  # 1000 ERG
    if amount_erg > max_withdrawal:
        return False, "Amount exceeds maximum withdrawal limit"
    
    return True, "Valid"

def is_valid_ergo_address(address):
    """Validate Ergo address format"""
    
    try:
        # Ergo mainnet addresses start with '9'
        # Testnet addresses start with '3'
        if not address.startswith('9'):
            return False
        
        # Address length should be between 51-55 characters
        if len(address) < 51 or len(address) > 55:
            return False
        
        # Could use node API to validate
        response = requests.post(
            "http://127.0.0.1:9053/utils/addressToRaw/" + address,
            headers={"api_key": API_KEY}
        )
        
        return response.status_code == 200
        
    except:
        return False
```

#### 6.5.2 Creating Withdrawal Transaction

```python
def process_withdrawal(withdrawal_id, api_key, db_connection):
    """Process a withdrawal request"""
    
    # Get withdrawal details
    withdrawal = get_withdrawal_by_id(withdrawal_id, db_connection)
    
    user_id = withdrawal["user_id"]
    destination_address = withdrawal["address"]
    amount_erg = withdrawal["amount_erg"]
    token_id = withdrawal.get("token_id")
    token_amount = withdrawal.get("token_amount")
    
    # Lock user balance
    lock_user_balance(user_id, amount_erg, token_id, token_amount, db_connection)
    
    try:
        # Prepare transaction request
        tx_request = {
            "requests": [
                {
                    "address": destination_address,
                    "value": amount_erg
                }
            ],
            "fee": 1000000  # 0.001 ERG
        }
        
        # Add tokens if present
        if token_id and token_amount:
            tx_request["requests"][0]["assets"] = [
                {
                    "tokenId": token_id,
                    "amount": token_amount
                }
            ]
        
        # Send transaction
        response = requests.post(
            "http://127.0.0.1:9053/wallet/transaction/send",
            headers={"api_key": api_key},
            json=tx_request
        )
        
        if response.status_code == 200:
            tx_data = response.json()
            tx_id = tx_data["id"]
            
            # Update withdrawal record
            update_withdrawal_status(
                withdrawal_id,
                "sent",
                tx_id,
                db_connection
            )
            
            # Deduct from user balance
            deduct_user_balance(
                user_id,
                amount_erg,
                token_id,
                token_amount,
                db_connection
            )
            
            return True, tx_id
        else:
            # Unlock balance on failure
            unlock_user_balance(user_id, amount_erg, token_id, token_amount, db_connection)
            return False, response.text
            
    except Exception as e:
        # Unlock balance on error
        unlock_user_balance(user_id, amount_erg, token_id, token_amount, db_connection)
        return False, str(e)
```

#### 6.5.3 Batch Withdrawal Processing

For efficiency, process multiple withdrawals in a single transaction:

```python
def process_batch_withdrawals(withdrawal_ids, api_key, db_connection):
    """Process multiple withdrawals in one transaction"""
    
    requests_list = []
    total_amount = 0
    
    for withdrawal_id in withdrawal_ids:
        withdrawal = get_withdrawal_by_id(withdrawal_id, db_connection)
        
        request = {
            "address": withdrawal["address"],
            "value": withdrawal["amount_erg"]
        }
        
        # Add tokens if present
        if withdrawal.get("token_id"):
            request["assets"] = [
                {
                    "tokenId": withdrawal["token_id"],
                    "amount": withdrawal["token_amount"]
                }
            ]
        
        requests_list.append(request)
        total_amount += withdrawal["amount_erg"]
    
    # Calculate appropriate fee based on number of outputs
    fee = calculate_batch_fee(len(requests_list))
    
    # Send batch transaction
    response = requests.post(
        "http://127.0.0.1:9053/wallet/transaction/send",
        headers={"api_key": api_key},
        json={
            "requests": requests_list,
            "fee": fee
        }
    )
    
    if response.status_code == 200:
        tx_data = response.json()
        tx_id = tx_data["id"]
        
        # Update all withdrawals
        for withdrawal_id in withdrawal_ids:
            update_withdrawal_status(withdrawal_id, "sent", tx_id, db_connection)
        
        return True, tx_id
    else:
        return False, response.text

def calculate_batch_fee(num_outputs):
    """Calculate fee for batch transaction"""
    
    base_fee = 1000000  # 0.001 ERG
    per_output_fee = 100000  # 0.0001 ERG per additional output
    
    return base_fee + (num_outputs - 1) * per_output_fee
```

#### 6.5.4 Withdrawal Database Schema

```sql
-- Withdrawals table
CREATE TABLE withdrawals (
    id SERIAL PRIMARY KEY,
    user_id INTEGER NOT NULL,
    address VARCHAR(64) NOT NULL,
    amount_erg BIGINT NOT NULL,
    token_id VARCHAR(64),
    token_amount BIGINT,
    fee_erg BIGINT NOT NULL,
    status VARCHAR(20) DEFAULT 'pending',
    tx_id VARCHAR(64),
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    sent_at TIMESTAMP,
    confirmed_at TIMESTAMP,
    INDEX idx_user_id (user_id),
    INDEX idx_status (status),
    INDEX idx_tx_id (tx_id)
);

-- Withdrawal status values: 'pending', 'processing', 'sent', 'confirmed', 'failed'
```

### 6.6 Hot and Cold Wallet Management

#### 6.6.1 Hot Wallet Strategy

**Hot Wallet**: Connected to internet, handles day-to-day operations

**Recommended Limits**:
- Keep < 5% of total funds in hot wallet
- Set maximum single transaction limit
- Implement automatic cold wallet refill

**Monitoring Hot Wallet Balance**:

```python
def check_hot_wallet_balance(api_key):
    """Check hot wallet balance"""
    
    response = requests.get(
        "http://127.0.0.1:9053/wallet/balances",
        headers={"api_key": api_key}
    )
    
    if response.status_code == 200:
        balance_data = response.json()
        balance_erg = balance_data["balance"]
        
        return balance_erg / 1e9  # Convert to ERG
    
    return 0

def monitor_hot_wallet(api_key, min_balance_erg, cold_wallet_address):
    """Monitor hot wallet and refill from cold if needed"""
    
    current_balance = check_hot_wallet_balance(api_key)
    
    if current_balance < min_balance_erg:
        # Alert administrators
        send_alert(f"Hot wallet balance low: {current_balance} ERG")
        
        # Request manual refill from cold wallet
        request_cold_wallet_transfer(cold_wallet_address, min_balance_erg * 2)
```

#### 6.6.2 Cold Wallet Strategy

**Cold Wallet**: Offline storage, holds majority of funds

**Best Practices**:
- Generate on air-gapped computer
- Store mnemonic in secure physical location (safe, vault)
- Use multi-signature for large cold wallets
- Regularly test recovery process

**Cold Wallet Setup**:

```bash
# On air-gapped computer
# 1. Generate wallet
curl -X POST "http://127.0.0.1:9053/wallet/init" \
  -H "api_key: YOUR_API_KEY" \
  -H "Content-Type: application/json" \
  -d '{"pass": "secure-cold-wallet-password"}'

# 2. Get first address
curl -X GET "http://127.0.0.1:9053/wallet/addresses" \
  -H "api_key: YOUR_API_KEY"

# 3. Backup mnemonic securely (write on paper, store in safe)

# 4. Shut down node, remove from internet
```

### 6.7 Monitoring and Error Handling

#### 6.7.1 Node Health Monitoring

```python
def check_node_health(api_key):
    """Check if node is healthy and synced"""
    
    try:
        response = requests.get(
            "http://127.0.0.1:9053/info",
            headers={"api_key": api_key},
            timeout=10
        )
        
        if response.status_code == 200:
            info = response.json()
            
            full_height = info["fullHeight"]
            headers_height = info["headersHeight"]
            max_peer_height = info["maxPeerHeight"]
            peers_count = info["peersCount"]
            
            # Check if synced
            is_synced = abs(full_height - max_peer_height) < 5
            
            # Check if enough peers
            has_peers = peers_count >= 3
            
            # Check if headers are synced
            headers_synced = abs(headers_height - max_peer_height) < 5
            
            return {
                "healthy": is_synced and has_peers and headers_synced,
                "synced": is_synced,
                "peers": peers_count,
                "height": full_height,
                "max_height": max_peer_height
            }
        else:
            return {"healthy": False, "error": "Node unreachable"}
            
    except Exception as e:
        return {"healthy": False, "error": str(e)}
```

#### 6.7.2 Handling Edge Cases

**Case 1: Transaction Stuck in Mempool**

```python
def check_stuck_transactions(api_key, max_mempool_time_hours=24):
    """Check for transactions stuck in mempool"""
    
    current_time = time.time()
    
    withdrawals = get_sent_withdrawals_without_confirmation(db_connection)
    
    for withdrawal in withdrawals:
        tx_id = withdrawal["tx_id"]
        sent_at = withdrawal["sent_at"].timestamp()
        
        # Check if in mempool too long
        if (current_time - sent_at) > (max_mempool_time_hours * 3600):
            # Get transaction status
            tx_status = get_transaction_status(tx_id, api_key)
            
            if tx_status == "not_found":
                # Transaction dropped from mempool
                # Need to resend with higher fee
                resubmit_withdrawal_with_higher_fee(withdrawal["id"], api_key)

def resubmit_withdrawal_with_higher_fee(withdrawal_id, api_key):
    """Resubmit withdrawal with increased fee"""
    
    withdrawal = get_withdrawal_by_id(withdrawal_id, db_connection)
    
    # Increase fee by 50%
    new_fee = int(withdrawal["fee_erg"] * 1.5)
    
    # Create new transaction
    # ... (similar to process_withdrawal but with higher fee)
```

**Case 2: Chain Reorganization**

```python
def handle_chain_reorg(api_key, db_connection):
    """Handle chain reorganization"""
    
    # Get recent confirmed deposits
    recent_deposits = get_deposits_in_last_n_blocks(100, db_connection)
    
    for deposit in recent_deposits:
        tx_id = deposit["tx_id"]
        
        # Check if transaction still exists
        tx_status = get_transaction_status(tx_id, api_key)
        
        if tx_status == "not_found":
            # Transaction was reorganized out
            
            # Reverse credit if already credited
            if deposit["credited"]:
                reverse_deposit_credit(deposit["id"], db_connection)
            
            # Mark as reorganized
            mark_deposit_reorganized(deposit["id"], db_connection)
```

**Case 3: Double Spend Detection**

```python
def detect_double_spends(db_connection):
    """Detect potential double spend attempts"""
    
    # Find deposits using same box in multiple transactions
    potential_double_spends = find_duplicate_box_usage(db_connection)
    
    for group in potential_double_spends:
        # One transaction will be confirmed, others invalid
        # Keep monitoring and only credit the confirmed one
        
        for deposit in group:
            if not is_transaction_confirmed(deposit["tx_id"]):
                flag_potential_double_spend(deposit["id"], db_connection)
```

---

## 7. Advanced Topics and Best Practices

### 7.1 Mining Pool Integration

Mining pools need to handle miner payouts efficiently. Ergo's UTXO model is well-suited for batch payments.

#### 7.1.1 Mining Pool Architecture

```
┌─────────────┐         ┌─────────────┐         ┌─────────────┐
│   Miners    │────────▶│   Pool      │────────▶│  Ergo Node  │
│  (Workers)  │         │   Server    │         │  + Wallet   │
└─────────────┘         └─────────────┘         └─────────────┘
                              │                        │
                              │                        │
                              ▼                        ▼
                        ┌─────────────┐         ┌─────────────┐
                        │  Payment    │         │   Block     │
                        │  Queue DB   │         │  Rewards    │
                        └─────────────┘         └─────────────┘
```

#### 7.1.2 Tracking Miner Shares

**Database Schema**:

```sql
-- Miners table
CREATE TABLE miners (
    id SERIAL PRIMARY KEY,
    address VARCHAR(64) NOT NULL UNIQUE,
    worker_name VARCHAR(100),
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    last_share_at TIMESTAMP,
    total_shares BIGINT DEFAULT 0,
    INDEX idx_address (address)
);

-- Shares table (for PPLNS or similar)
CREATE TABLE shares (
    id SERIAL PRIMARY KEY,
    miner_id INTEGER NOT NULL,
    difficulty BIGINT NOT NULL,
    submitted_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    block_height INTEGER,
    valid BOOLEAN DEFAULT TRUE,
    INDEX idx_miner_submitted (miner_id, submitted_at),
    INDEX idx_block_height (block_height)
);

-- Blocks found
CREATE TABLE blocks_found (
    id SERIAL PRIMARY KEY,
    height INTEGER NOT NULL UNIQUE,
    block_id VARCHAR(64) NOT NULL,
    miner_id INTEGER,
    reward BIGINT NOT NULL,
    timestamp TIMESTAMP NOT NULL,
    status VARCHAR(20) DEFAULT 'pending',
    confirmations INTEGER DEFAULT 0,
    INDEX idx_status (status),
    INDEX idx_height (height)
);

-- Payments queue
CREATE TABLE payment_queue (
    id SERIAL PRIMARY KEY,
    miner_id INTEGER NOT NULL,
    address VARCHAR(64) NOT NULL,
    amount_erg BIGINT NOT NULL,
    block_height INTEGER,
    status VARCHAR(20) DEFAULT 'pending',
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    INDEX idx_status (status),
    INDEX idx_miner (miner_id)
);

-- Payment history
CREATE TABLE payment_history (
    id SERIAL PRIMARY KEY,
    payment_id INTEGER NOT NULL,
    tx_id VARCHAR(64) NOT NULL,
    amount_erg BIGINT NOT NULL,
    fee_erg BIGINT NOT NULL,
    sent_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    confirmed_at TIMESTAMP,
    INDEX idx_tx_id (tx_id)
);
```

#### 7.1.3 Calculating Payouts (PPLNS Method)

**PPLNS (Pay Per Last N Shares)**: Miners paid based on shares submitted in last N shares before block.

```python
import math
from datetime import datetime, timedelta

def calculate_pplns_payouts(block_id, block_reward, n_shares, db_connection):
    """Calculate PPLNS payouts for a found block"""
    
    # Get the block
    block = get_block_by_id(block_id, db_connection)
    block_height = block["height"]
    
    # Pool fee (e.g., 1%)
    pool_fee_percent = 1.0
    pool_fee = int(block_reward * (pool_fee_percent / 100))
    
    # Amount available for miners
    miner_reward = block_reward - pool_fee
    
    # Get last N shares before this block
    shares = get_last_n_shares_before_block(n_shares, block_height, db_connection)
    
    # Calculate total difficulty
    total_difficulty = sum(share["difficulty"] for share in shares)
    
    if total_difficulty == 0:
        return []
    
    # Calculate payout for each miner
    miner_payouts = {}
    
    for share in shares:
        miner_id = share["miner_id"]
        difficulty = share["difficulty"]
        
        # Proportional payout
        payout = int((difficulty / total_difficulty) * miner_reward)
        
        if miner_id in miner_payouts:
            miner_payouts[miner_id] += payout
        else:
            miner_payouts[miner_id] = payout
    
    # Create payment queue entries
    payment_entries = []
    
    for miner_id, amount in miner_payouts.items():
        miner = get_miner_by_id(miner_id, db_connection)
        
        # Minimum payout threshold (e.g., 0.1 ERG)
        if amount >= 100000000:  # 0.1 ERG in nanoERG
            payment_entries.append({
                "miner_id": miner_id,
                "address": miner["address"],
                "amount_erg": amount,
                "block_height": block_height
            })
        else:
            # Accumulate for next payout
            accumulate_miner_balance(miner_id, amount, db_connection)
    
    return payment_entries
```

#### 7.1.4 Batch Payment Processing

Mining pools should batch payments to minimize fees and blockchain bloat.

```python
def process_miner_payments_batch(api_key, db_connection, max_outputs=50):
    """Process pending miner payments in batches"""
    
    # Get pending payments
    pending_payments = get_pending_payments(db_connection, limit=max_outputs)
    
    if not pending_payments:
        return None
    
    # Group by minimum payout threshold
    payments_to_send = []
    total_amount = 0
    
    for payment in pending_payments:
        # Check if miner has reached minimum payout
        miner_total = get_miner_total_pending(payment["miner_id"], db_connection)
        
        if miner_total >= 100000000:  # 0.1 ERG minimum
            payments_to_send.append(payment)
            total_amount += payment["amount_erg"]
    
    if not payments_to_send:
        return None
    
    # Build transaction request
    requests = []
    
    for payment in payments_to_send:
        requests.append({
            "address": payment["address"],
            "value": payment["amount_erg"]
        })
    
    # Calculate fee (larger fee for many outputs)
    base_fee = 1000000  # 0.001 ERG
    per_output = 50000  # 0.00005 ERG per output
    total_fee = base_fee + (len(requests) * per_output)
    
    # Send transaction
    try:
        response = requests.post(
            "http://127.0.0.1:9053/wallet/transaction/send",
            headers={"api_key": api_key},
            json={
                "requests": requests,
                "fee": total_fee
            },
            timeout=30
        )
        
        if response.status_code == 200:
            tx_data = response.json()
            tx_id = tx_data["id"]
            
            # Record payment
            for payment in payments_to_send:
                record_payment_sent(
                    payment["id"],
                    tx_id,
                    payment["amount_erg"],
                    total_fee,
                    db_connection
                )
            
            return {
                "success": True,
                "tx_id": tx_id,
                "num_payments": len(payments_to_send),
                "total_amount": total_amount,
                "fee": total_fee
            }
        else:
            return {
                "success": False,
                "error": response.text
            }
            
    except Exception as e:
        return {
            "success": False,
            "error": str(e)
        }
```

#### 7.1.5 Monitoring Block Confirmations

```python
def monitor_block_confirmations(api_key, db_connection):
    """Monitor confirmations for found blocks"""
    
    # Get unconfirmed blocks
    pending_blocks = get_pending_blocks(db_connection)
    
    for block in pending_blocks:
        block_height = block["height"]
        
        # Get current blockchain height
        current_height = get_current_blockchain_height(api_key)
        
        # Calculate confirmations
        confirmations = current_height - block_height
        
        # Update confirmations
        update_block_confirmations(block["id"], confirmations, db_connection)
        
        # Mature blocks (120+ confirmations)
        if confirmations >= 120:
            if block["status"] == "pending":
                # Mark as mature and calculate payouts
                mark_block_mature(block["id"], db_connection)
                
                # Calculate and queue payouts
                payouts = calculate_pplns_payouts(
                    block["id"],
                    block["reward"],
                    n_shares=10000,
                    db_connection=db_connection
                )
                
                # Add to payment queue
                for payout in payouts:
                    add_to_payment_queue(payout, db_connection)
```

#### 7.1.6 Miner Statistics API

Provide miners with detailed statistics:

```python
from flask import Flask, jsonify, request

app = Flask(__name__)

@app.route('/api/miner/stats', methods=['GET'])
def get_miner_stats():
    """Get miner statistics"""
    
    address = request.args.get('address')
    
    if not address:
        return jsonify({"error": "Address required"}), 400
    
    miner = get_miner_by_address(address, db_connection)
    
    if not miner:
        return jsonify({"error": "Miner not found"}), 404
    
    # Get statistics
    stats = {
        "address": address,
        "worker_name": miner["worker_name"],
        "total_shares": miner["total_shares"],
        "last_share": miner["last_share_at"].isoformat() if miner["last_share_at"] else None,
        "pending_balance": get_miner_pending_balance(miner["id"], db_connection),
        "paid_balance": get_miner_paid_balance(miner["id"], db_connection),
        "total_payments": get_miner_payment_count(miner["id"], db_connection),
        "recent_shares": get_miner_recent_shares(miner["id"], 100, db_connection),
        "hashrate_24h": calculate_miner_hashrate(miner["id"], 24, db_connection),
        "blocks_found": get_miner_blocks_found(miner["id"], db_connection)
    }
    
    return jsonify(stats)

def calculate_miner_hashrate(miner_id, hours, db_connection):
    """Calculate miner hashrate over time period"""
    
    from_time = datetime.now() - timedelta(hours=hours)
    
    shares = get_miner_shares_since(miner_id, from_time, db_connection)
    
    if not shares:
        return 0
    
    total_difficulty = sum(share["difficulty"] for share in shares)
    time_seconds = hours * 3600
    
    # Average block time in Ergo is 120 seconds
    hashrate = (total_difficulty * 2**32) / time_seconds
    
    return hashrate
```

### 7.2 Token Integration

Ergo's native token support allows exchanges to list custom tokens alongside ERG.

#### 7.2.1 Token Discovery and Metadata

**Fetching Token Information**:

```python
def get_token_info(token_id, api_key):
    """Get token information from blockchain"""
    
    # Get token info from node
    response = requests.get(
        f"http://127.0.0.1:9053/blockchain/token/byId/{token_id}",
        headers={"api_key": api_key}
    )
    
    if response.status_code == 200:
        token_data = response.json()
        
        return {
            "id": token_data["id"],
            "boxId": token_data["boxId"],
            "emissionAmount": token_data["emissionAmount"],
            "name": token_data.get("name", "Unknown"),
            "description": token_data.get("description", ""),
            "decimals": token_data.get("decimals", 0),
            "type": token_data.get("type", "EIP-004")
        }
    
    return None

def parse_token_metadata_from_registers(registers):
    """Parse token metadata from registers (EIP-004)"""
    
    import binascii
    
    metadata = {}
    
    # R4: Name
    if "R4" in registers:
        r4_hex = registers["R4"][4:]  # Skip type prefix
        try:
            metadata["name"] = binascii.unhexlify(r4_hex).decode('utf-8')
        except:
            metadata["name"] = "Unknown"
    
    # R5: Description
    if "R5" in registers:
        r5_hex = registers["R5"][4:]
        try:
            metadata["description"] = binascii.unhexlify(r5_hex).decode('utf-8')
        except:
            metadata["description"] = ""
    
    # R6: Decimals
    if "R6" in registers:
        r6_hex = registers["R6"][4:]
        try:
            metadata["decimals"] = int(r6_hex, 16)
        except:
            metadata["decimals"] = 0
    
    return metadata
```

#### 7.2.2 Token Balance Management

**Database Schema for Tokens**:

```sql
-- Tokens catalog
CREATE TABLE tokens (
    id SERIAL PRIMARY KEY,
    token_id VARCHAR(64) NOT NULL UNIQUE,
    name VARCHAR(100),
    description TEXT,
    decimals INTEGER DEFAULT 0,
    total_supply BIGINT,
    box_id VARCHAR(64),
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    verified BOOLEAN DEFAULT FALSE,
    tradable BOOLEAN DEFAULT FALSE,
    INDEX idx_token_id (token_id),
    INDEX idx_tradable (tradable)
);

-- Token deposits (separate from ERG deposits)
CREATE TABLE token_deposits (
    id SERIAL PRIMARY KEY,
    user_id INTEGER NOT NULL,
    token_id VARCHAR(64) NOT NULL,
    tx_id VARCHAR(64) NOT NULL,
    output_index INTEGER NOT NULL,
    amount BIGINT NOT NULL,
    confirmations INTEGER DEFAULT 0,
    required_confirmations INTEGER DEFAULT 30,
    credited BOOLEAN DEFAULT FALSE,
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    UNIQUE(tx_id, output_index, token_id),
    INDEX idx_user_token (user_id, token_id),
    INDEX idx_credited (credited)
);

-- Token withdrawals
CREATE TABLE token_withdrawals (
    id SERIAL PRIMARY KEY,
    user_id INTEGER NOT NULL,
    token_id VARCHAR(64) NOT NULL,
    address VARCHAR(64) NOT NULL,
    amount BIGINT NOT NULL,
    erg_amount BIGINT NOT NULL,
    status VARCHAR(20) DEFAULT 'pending',
    tx_id VARCHAR(64),
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    sent_at TIMESTAMP,
    INDEX idx_user_token (user_id, token_id),
    INDEX idx_status (status)
);
```

#### 7.2.3 Token Trading Pairs

**Creating Token Markets**:

```python
def create_token_market(token_id, base_currency, api_key, db_connection):
    """Create a new trading market for a token"""
    
    # Get token info
    token_info = get_token_info(token_id, api_key)
    
    if not token_info:
        return {"error": "Token not found"}
    
    # Verify token
    verified = verify_token_legitimacy(token_id, token_info)
    
    # Create market entry
    market = {
        "token_id": token_id,
        "token_name": token_info["name"],
        "base_currency": base_currency,  # ERG, USD, BTC, etc.
        "decimals": token_info["decimals"],
        "min_order_size": 1,
        "max_order_size": token_info["emissionAmount"],
        "maker_fee": 0.001,  # 0.1%
        "taker_fee": 0.002,  # 0.2%
        "verified": verified,
        "active": True
    }
    
    save_market(market, db_connection)
    
    return market

def verify_token_legitimacy(token_id, token_info):
    """Verify token is legitimate (not scam)"""
    
    checks = {
        "has_name": bool(token_info.get("name")),
        "has_description": bool(token_info.get("description")),
        "reasonable_supply": token_info["emissionAmount"] < 10**18,
        "proper_decimals": 0 <= token_info["decimals"] <= 18,
    }
    
    # Check against known token registry
    # (could integrate with tokenjay.app or similar)
    
    # Manual verification by exchange team recommended
    
    return all(checks.values())
```

#### 7.2.4 Token Deposit Processing

```python
def process_token_deposit(user_id, tx_id, output_index, token_id, amount, confirmations, db_connection):
    """Process a token deposit"""
    
    # Check if already processed
    existing = get_token_deposit(tx_id, output_index, token_id, db_connection)
    
    if existing:
        # Update confirmations
        update_token_deposit_confirmations(existing["id"], confirmations, db_connection)
        
        # Credit if sufficient confirmations
        if confirmations >= existing["required_confirmations"] and not existing["credited"]:
            credit_user_token_balance(user_id, token_id, amount, db_connection)
            mark_token_deposit_credited(existing["id"], db_connection)
        
        return existing
    
    # Create new deposit record
    deposit = {
        "user_id": user_id,
        "token_id": token_id,
        "tx_id": tx_id,
        "output_index": output_index,
        "amount": amount,
        "confirmations": confirmations,
        "required_confirmations": 30  # Higher for tokens
    }
    
    deposit_id = save_token_deposit(deposit, db_connection)
    
    return deposit
```

#### 7.2.5 Token Withdrawal with ERG

Tokens must be sent with minimum ERG for box value:

```python
def process_token_withdrawal(user_id, token_id, address, token_amount, api_key, db_connection):
    """Process token withdrawal"""
    
    # Check user token balance
    user_balance = get_user_token_balance(user_id, token_id, db_connection)
    
    if user_balance < token_amount:
        return {"error": "Insufficient token balance"}
    
    # Minimum ERG required for token box
    min_erg = 1000000  # 0.001 ERG
    
    # Check user ERG balance for box value + fee
    user_erg_balance = get_user_balance(user_id, db_connection)
    total_erg_needed = min_erg + 2000000  # box value + fee
    
    if user_erg_balance < total_erg_needed:
        return {"error": "Insufficient ERG for token box"}
    
    # Create transaction
    tx_request = {
        "requests": [
            {
                "address": address,
                "value": min_erg,
                "assets": [
                    {
                        "tokenId": token_id,
                        "amount": token_amount
                    }
                ]
            }
        ],
        "fee": 2000000  # Higher fee for token transactions
    }
    
    response = requests.post(
        "http://127.0.0.1:9053/wallet/transaction/send",
        headers={"api_key": api_key},
        json=tx_request
    )
    
    if response.status_code == 200:
        tx_data = response.json()
        tx_id = tx_data["id"]
        
        # Deduct from user balances
        deduct_user_token_balance(user_id, token_id, token_amount, db_connection)
        deduct_user_balance(user_id, total_erg_needed, db_connection)
        
        # Record withdrawal
        record_token_withdrawal(
            user_id, token_id, address, token_amount,
            min_erg, tx_id, db_connection
        )
        
        return {"success": True, "tx_id": tx_id}
    else:
        return {"error": response.text}
```

### 7.3 Advanced Security Practices

#### 7.3.1 Rate Limiting and DDoS Protection

```python
from functools import wraps
from flask import request, jsonify
import time
from collections import defaultdict

# Simple in-memory rate limiter
rate_limit_data = defaultdict(list)

def rate_limit(max_requests=10, window_seconds=60):
    """Rate limiting decorator"""
    
    def decorator(f):
        @wraps(f)
        def wrapped(*args, **kwargs):
            # Get client identifier
            client_ip = request.remote_addr
            
            current_time = time.time()
            
            # Clean old requests
            rate_limit_data[client_ip] = [
                req_time for req_time in rate_limit_data[client_ip]
                if current_time - req_time < window_seconds
            ]
            
            # Check rate limit
            if len(rate_limit_data[client_ip]) >= max_requests:
                return jsonify({
                    "error": "Rate limit exceeded",
                    "retry_after": window_seconds
                }), 429
            
            # Record request
            rate_limit_data[client_ip].append(current_time)
            
            return f(*args, **kwargs)
        
        return wrapped
    return decorator

# Usage
@app.route('/api/withdraw', methods=['POST'])
@rate_limit(max_requests=5, window_seconds=60)
def withdraw():
    # Withdrawal logic
    pass
```

#### 7.3.2 Multi-Signature Security

**Setting up 2-of-3 Multi-Sig for Exchange Hot Wallet**:

```python
def create_multisig_wallet(pubkeys, threshold=2, api_key):
    """Create multi-signature wallet"""
    
    # Create ErgoScript for threshold signature
    script_source = f"""
    {{
        atLeast(
            {threshold},
            Coll(
                {', '.join(f'PK("{pk}")' for pk in pubkeys)}
            )
        )
    }}
    """
    
    # Compile to address
    response = requests.post(
        "http://127.0.0.1:9053/script/p2sAddress",
        headers={"api_key": api_key},
        json={"source": script_source}
    )
    
    if response.status_code == 200:
        address_data = response.json()
        return address_data["address"]
    
    return None

def sign_multisig_transaction(unsigned_tx, signing_party_wallet, api_key):
    """Sign transaction with one party's signature"""
    
    # Each party signs independently
    response = requests.post(
        "http://127.0.0.1:9053/wallet/transaction/sign",
        headers={"api_key": api_key},
        json={
            "tx": unsigned_tx,
            "inputsRaw": [],
            "dataInputsRaw": []
        }
    )
    
    if response.status_code == 200:
        return response.json()
    
    return None

def combine_multisig_signatures(tx_with_sig1, tx_with_sig2):
    """Combine signatures from multiple parties"""
    
    # Ergo automatically combines valid signatures
    # If threshold is met, transaction is valid
    
    # Submit the transaction with sufficient signatures
    return tx_with_sig2  # Already contains both signatures
```

#### 7.3.3 Audit Logging

```sql
-- Comprehensive audit log
CREATE TABLE audit_log (
    id SERIAL PRIMARY KEY,
    timestamp TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    user_id INTEGER,
    action VARCHAR(50) NOT NULL,
    entity_type VARCHAR(50),
    entity_id VARCHAR(100),
    ip_address INET,
    user_agent TEXT,
    request_data JSONB,
    response_data JSONB,
    success BOOLEAN,
    error_message TEXT,
    INDEX idx_user_action (user_id, action),
    INDEX idx_timestamp (timestamp),
    INDEX idx_entity (entity_type, entity_id)
);

-- Critical operations log
CREATE TABLE critical_operations (
    id SERIAL PRIMARY KEY,
    operation_type VARCHAR(50) NOT NULL,
    initiated_by INTEGER NOT NULL,
    approved_by INTEGER,
    parameters JSONB NOT NULL,
    status VARCHAR(20) DEFAULT 'pending',
    executed_at TIMESTAMP,
    result JSONB,
    INDEX idx_status (status)
);
```

```python
def log_audit(user_id, action, entity_type, entity_id, request_data, success, db_connection):
    """Log audit entry"""
    
    audit_entry = {
        "user_id": user_id,
        "action": action,
        "entity_type": entity_type,
        "entity_id": entity_id,
        "ip_address": request.remote_addr,
        "user_agent": request.headers.get('User-Agent'),
        "request_data": request_data,
        "success": success,
        "timestamp": datetime.now()
    }
    
    save_audit_log(audit_entry, db_connection)

# Usage
@app.route('/api/withdraw', methods=['POST'])
def withdraw():
    user_id = get_authenticated_user()
    data = request.json
    
    try:
        result = process_withdrawal(user_id, data)
        log_audit(user_id, "WITHDRAWAL", "transaction", result["tx_id"], data, True, db)
        return jsonify(result)
    except Exception as e:
        log_audit(user_id, "WITHDRAWAL", "transaction", None, data, False, db)
        return jsonify({"error": str(e)}), 500
```

#### 7.3.4 Two-Factor Authentication for Withdrawals

```python
import pyotp
import qrcode

def setup_2fa(user_id, db_connection):
    """Setup 2FA for user"""
    
    # Generate secret
    secret = pyotp.random_base32()
    
    # Save to database (encrypted)
    save_user_2fa_secret(user_id, secret, db_connection)
    
    # Generate QR code
    user_email = get_user_email(user_id, db_connection)
    totp_uri = pyotp.totp.TOTP(secret).provisioning_uri(
        name=user_email,
        issuer_name="Exchange Name"
    )
    
    return {
        "secret": secret,
        "qr_code_uri": totp_uri
    }

def verify_2fa(user_id, token, db_connection):
    """Verify 2FA token"""
    
    secret = get_user_2fa_secret(user_id, db_connection)
    
    if not secret:
        return False
    
    totp = pyotp.TOTP(secret)
    
    # Verify with 30-second window
    return totp.verify(token, valid_window=1)

@app.route('/api/withdraw', methods=['POST'])
def withdraw():
    user_id = get_authenticated_user()
    data = request.json
    
    # Verify 2FA token
    if not verify_2fa(user_id, data.get('2fa_token'), db):
        return jsonify({"error": "Invalid 2FA token"}), 403
    
    # Process withdrawal
    result = process_withdrawal(user_id, data)
    return jsonify(result)
```

#### 7.3.5 IP Whitelisting for API Access

```python
def check_ip_whitelist(f):
    """Decorator to check IP whitelist"""
    
    @wraps(f)
    def wrapped(*args, **kwargs):
        client_ip = request.remote_addr
        
        # Get whitelisted IPs from database
        whitelisted_ips = get_whitelisted_ips(db)
        
        if client_ip not in whitelisted_ips:
            log_security_event("UNAUTHORIZED_IP", client_ip)
            return jsonify({"error": "IP not whitelisted"}), 403
        
        return f(*args, **kwargs)
    
    return wrapped

@app.route('/api/admin/withdraw', methods=['POST'])
@check_ip_whitelist
def admin_withdraw():
    # Admin withdrawal logic
    pass
```

### 7.4 Performance Optimization

#### 7.4.1 Database Indexing Strategy

```sql
-- Optimize deposit queries
CREATE INDEX CONCURRENTLY idx_deposits_user_status 
ON deposits(user_id, credited) 
WHERE credited = false;

-- Optimize transaction lookups
CREATE INDEX CONCURRENTLY idx_deposits_tx_output 
ON deposits(tx_id, output_index);

-- Optimize balance queries
CREATE INDEX CONCURRENTLY idx_user_balances_user 
ON user_balances(user_id) 
INCLUDE (erg_balance, erg_locked);

-- Optimize token balance queries
CREATE INDEX CONCURRENTLY idx_token_balances_user_token 
ON user_token_balances(user_id, token_id) 
INCLUDE (balance, locked);

-- Optimize withdrawal status queries
CREATE INDEX CONCURRENTLY idx_withdrawals_status_created 
ON withdrawals(status, created_at) 
WHERE status IN ('pending', 'processing');

-- Partial index for active addresses
CREATE INDEX CONCURRENTLY idx_user_addresses_active 
ON user_addresses(address) 
WHERE user_id IS NOT NULL;
```

#### 7.4.2 Connection Pooling

```python
from sqlalchemy import create_engine
from sqlalchemy.pool import QueuePool

# Database connection pool
engine = create_engine(
    'postgresql://user:pass@localhost/exchange_db',
    poolclass=QueuePool,
    pool_size=20,
    max_overflow=40,
    pool_pre_ping=True,
    pool_recycle=3600
)

def get_db_connection():
    """Get database connection from pool"""
    return engine.connect()
```

#### 7.4.3 Caching Strategy

```python
import redis
import json

# Redis cache
redis_client = redis.Redis(host='localhost', port=6379, db=0)

def cache_token_info(token_id, token_info, ttl=3600):
    """Cache token information"""
    
    key = f"token:{token_id}"
    redis_client.setex(key, ttl, json.dumps(token_info))

def get_cached_token_info(token_id):
    """Get cached token information"""
    
    key = f"token:{token_id}"
    data = redis_client.get(key)
    
    if data:
        return json.loads(data)
    
    return None

def get_token_info_with_cache(token_id, api_key):
    """Get token info with caching"""
    
    # Try cache first
    cached = get_cached_token_info(token_id)
    if cached:
        return cached
    
    # Fetch from API
    token_info = get_token_info(token_id, api_key)
    
    if token_info:
        # Cache for 1 hour
        cache_token_info(token_id, token_info, 3600)
    
    return token_info
```

#### 7.4.4 Asynchronous Processing

```python
from celery import Celery
import celery

# Celery for async tasks
celery_app = Celery('exchange', broker='redis://localhost:6379/0')

@celery_app.task
def process_deposit_async(tx_id, db_config):
    """Asynchronously process deposit"""
    
    db = create_db_connection(db_config)
    
    try:
        transaction = get_transaction(tx_id, API_KEY)
        process_transaction_for_deposits(transaction, db)
        db.commit()
    except Exception as e:
        logger.error(f"Error processing deposit {tx_id}: {e}")
        db.rollback()
    finally:
        db.close()

@celery_app.task
def process_withdrawal_async(withdrawal_id, api_key, db_config):
    """Asynchronously process withdrawal"""
    
    db = create_db_connection(db_config)
    
    try:
        result = process_withdrawal(withdrawal_id, api_key, db)
        db.commit()
        return result
    except Exception as e:
        logger.error(f"Error processing withdrawal {withdrawal_id}: {e}")
        db.rollback()
        return {"error": str(e)}
    finally:
        db.close()

# Periodic tasks
@celery_app.task
def monitor_deposits():
    """Periodic task to monitor deposits"""
    
    poll_new_deposits(API_KEY, db)

@celery_app.task
def process_pending_withdrawals():
    """Periodic task to process withdrawals"""
    
    process_miner_payments_batch(API_KEY, db, max_outputs=50)

# Schedule periodic tasks
celery_app.conf.beat_schedule = {
    'monitor-deposits': {
        'task': 'tasks.monitor_deposits',
        'schedule': 30.0,  # Every 30 seconds
    },
    'process-withdrawals': {
        'task': 'tasks.process_pending_withdrawals',
        'schedule': 60.0,  # Every minute
    },
}
```

### 7.5 Disaster Recovery

#### 7.5.1 Backup Strategy

```bash
#!/bin/bash
# backup_exchange.sh

# Configuration
BACKUP_DIR="/backups/exchange"
DATE=$(date +%Y%m%d_%H%M%S)
RETENTION_DAYS=30

# Backup PostgreSQL database
pg_dump -U exchange_user exchange_db | gzip > "$BACKUP_DIR/db_$DATE.sql.gz"

# Backup wallet
cp ~/.ergo/wallet/wallet.json "$BACKUP_DIR/wallet_$DATE.json"

# Backup node data (if needed)
tar -czf "$BACKUP_DIR/node_data_$DATE.tar.gz" ~/.ergo/data

# Encrypt backups
gpg --encrypt --recipient exchange@example.com "$BACKUP_DIR/db_$DATE.sql.gz"
gpg --encrypt --recipient exchange@example.com "$BACKUP_DIR/wallet_$DATE.json"

# Upload to remote storage (S3, etc.)
aws s3 cp "$BACKUP_DIR/" "s3://exchange-backups/" --recursive

# Clean old backups
find "$BACKUP_DIR" -name "*.sql.gz" -mtime +$RETENTION_DAYS -delete
find "$BACKUP_DIR" -name "*.json" -mtime +$RETENTION_DAYS -delete

echo "Backup completed: $DATE"
```

#### 7.5.2 Disaster Recovery Plan

**Recovery Steps**:

1. **Restore Database**:
```bash
# Decrypt backup
gpg --decrypt backup.sql.gz.gpg > backup.sql.gz

# Restore database
gunzip -c backup.sql.gz | psql -U exchange_user exchange_db
```

2. **Restore Wallet**:
```bash
# Stop node
systemctl stop ergo-node

# Restore wallet file
gpg --decrypt wallet_backup.json.gpg > ~/.ergo/wallet/wallet.json

# Start node
systemctl start ergo-node

# Unlock wallet
curl -X POST "http://127.0.0.1:9053/wallet/unlock" \
  -H "api_key: $API_KEY" \
  -d '{"pass": "wallet_password"}'
```

3. **Verify Balances**:
```python
def verify_balances_after_recovery(db_connection, api_key):
    """Verify balances match after recovery"""
    
    # Get wallet balance
    wallet_balance = check_hot_wallet_balance(api_key)
    
    # Get database balance
    total_user_balances = get_total_user_balances(db_connection)
    total_locked = get_total_locked_balances(db_connection)
    total_pending_withdrawals = get_total_pending_withdrawals(db_connection)
    
    # Expected wallet balance
    expected = total_user_balances + total_locked - total_pending_withdrawals
    
    # Check if matches
    difference = abs(wallet_balance - expected)
    tolerance = 100000000  # 0.1 ERG tolerance
    
    if difference > tolerance:
        alert_admins(f"Balance mismatch: {difference} nanoERG")
        return False
    
    return True
```

#### 7.5.3 High Availability Setup

**Load Balancer Configuration** (nginx):

```nginx
upstream ergo_nodes {
    least_conn;
    server node1.internal:9053 max_fails=3 fail_timeout=30s;
    server node2.internal:9053 max_fails=3 fail_timeout=30s;
    server node3.internal:9053 max_fails=3 fail_timeout=30s;
}

server {
    listen 443 ssl http2;
    server_name api.exchange.com;
    
    ssl_certificate /etc/ssl/certs/exchange.crt;
    ssl_certificate_key /etc/ssl/private/exchange.key;
    
    location /api/ {
        proxy_pass http://ergo_nodes/;
        proxy_set_header Host $host;
        proxy_set_header X-Real-IP $remote_addr;
        proxy_connect_timeout 10s;
        proxy_read_timeout 30s;
        
        # Health check
        proxy_next_upstream error timeout http_500 http_502 http_503;
    }
}
```

### 7.6 Testing Strategies

#### 7.6.1 Unit Tests

```python
import unittest
from decimal import Decimal

class TestWithdrawalValidation(unittest.TestCase):
    
    def test_valid_address(self):
        """Test valid Ergo address"""
        address = "9f4QF8AD1nQ3nJahQVkMj8hFSVVzVom77b52JU7EW71Zexg6N8v"
        self.assertTrue(is_valid_ergo_address(address))
    
    def test_invalid_address(self):
        """Test invalid Ergo address"""
        address = "invalid_address"
        self.assertFalse(is_valid_ergo_address(address))
    
    def test_minimum_withdrawal(self):
        """Test minimum withdrawal amount"""
        amount = 500000  # Below minimum
        valid, msg = validate_withdrawal_request(1, "9f4Q...", amount, db)
        self.assertFalse(valid)
    
    def test_insufficient_balance(self):
        """Test withdrawal with insufficient balance"""
        # Mock user with 1 ERG balance
        user_balance = 1000000000
        withdrawal_amount = 2000000000  # 2 ERG
        
        valid, msg = validate_withdrawal_request(1, "9f4Q...", withdrawal_amount, db)
        self.assertFalse(valid)
        self.assertIn("Insufficient", msg)

class TestDepositProcessing(unittest.TestCase):
    
    def test_deposit_detection(self):
        """Test deposit detection"""
        # Mock transaction
        tx = create_mock_transaction()
        process_transaction_for_deposits(tx, db)
        
        # Verify deposit recorded
        deposits = get_user_deposits(user_id, db)
        self.assertEqual(len(deposits), 1)
    
    def test_duplicate_deposit(self):
        """Test duplicate deposit prevention"""
        tx = create_mock_transaction()
        
        # Process twice
        process_transaction_for_deposits(tx, db)
        process_transaction_for_deposits(tx, db)
        
        # Should only create one deposit
        deposits = get_user_deposits(user_id, db)
        self.assertEqual(len(deposits), 1)

if __name__ == '__main__':
    unittest.main()
```

#### 7.6.2 Integration Tests

```python
import requests
import time

def test_full_deposit_workflow():
    """Test complete deposit workflow"""
    
    # 1. Generate deposit address
    response = requests.get(
        f"{API_BASE}/wallet/deriveNextKey",
        headers={"api_key": API_KEY}
    )
    address = response.json()["address"]
    
    # 2. Send test transaction (on testnet)
    tx_response = requests.post(
        f"{API_BASE}/wallet/payment/send",
        headers={"api_key": API_KEY},
        json={
            "address": address,
            "value": 1000000000,  # 1 ERG
            "fee": 1000000
        }
    )
    tx_id = tx_response.json()["id"]
    
    # 3. Wait for confirmations
    confirmations = 0
    while confirmations < 10:
        time.sleep(120)  # Wait 2 minutes (1 block)
        
        tx_status = requests.get(
            f"{API_BASE}/blockchain/transaction/byId/{tx_id}",
            headers={"api_key": API_KEY}
        )
        
        if tx_status.status_code == 200:
            confirmations = tx_status.json().get("numConfirmations", 0)
    
    # 4. Verify deposit credited
    user_balance = get_user_balance(user_id, db)
    assert user_balance >= 1000000000

def test_full_withdrawal_workflow():
    """Test complete withdrawal workflow"""
    
    # 1. Create withdrawal request
    withdrawal_data = {
        "address": "9fRusAarL1KkrVdRXrJhmJeTWzJPkL6mdXW2VPJvj8L3kQy7BXm",
        "amount": 500000000,  # 0.5 ERG
        "2fa_token": generate_2fa_token(user_id)
    }
    
    response = requests.post(
        f"{EXCHANGE_API}/withdraw",
        headers={"Authorization": f"Bearer {user_token}"},
        json=withdrawal_data
    )
    
    assert response.status_code == 200
    tx_id = response.json()["tx_id"]
    
    # 2. Wait for transaction broadcast
    time.sleep(30)
    
    # 3. Verify transaction on blockchain
    tx_status = requests.get(
        f"{API_BASE}/blockchain/transaction/byId/{tx_id}",
        headers={"api_key": API_KEY}
    )
    
    assert tx_status.status_code == 200
```

#### 7.6.3 Load Testing

```python
from locust import HttpUser, task, between

class ExchangeUser(HttpUser):
    wait_time = between(1, 5)
    
    def on_start(self):
        """Login before testing"""
        response = self.client.post("/login", json={
            "username": "testuser",
            "password": "testpass"
        })
        self.token = response.json()["token"]
    
    @task(3)
    def get_balance(self):
        """Test balance endpoint"""
        self.client.get(
            "/api/balance",
            headers={"Authorization": f"Bearer {self.token}"}
        )
    
    @task(2)
    def get_deposit_address(self):
        """Test deposit address generation"""
        self.client.get(
            "/api/deposit/address",
            headers={"Authorization": f"Bearer {self.token}"}
        )
    
    @task(1)
    def create_withdrawal(self):
        """Test withdrawal creation"""
        self.client.post(
            "/api/withdraw",
            headers={"Authorization": f"Bearer {self.token}"},
            json={
                "address": "9f4QF8AD1nQ3nJahQVkMj8hFSVVzVom77b52JU7EW71Zexg6N8v",
                "amount": 1000000000
            }
        )

# Run with: locust -f load_test.py --host=https://exchange.com
```

### 7.7 Compliance and Regulations

#### 7.7.1 KYC/AML Integration

```python
def verify_user_kyc(user_id, kyc_data, db_connection):
    """Verify user KYC information"""
    
    # Basic validation
    required_fields = ['full_name', 'date_of_birth', 'country', 'document_type', 'document_number']
    
    for field in required_fields:
        if field not in kyc_data:
            return {"verified": False, "error": f"Missing {field}"}
    
    # Age verification
    dob = datetime.strptime(kyc_data['date_of_birth'], '%Y-%m-%d')
    age = (datetime.now() - dob).days / 365.25
    
    if age < 18:
        return {"verified": False, "error": "User must be 18+"}
    
    # Country sanctions check
    if is_sanctioned_country(kyc_data['country']):
        return {"verified": False, "error": "Country not supported"}
    
    # External KYC provider integration (e.g., Onfido, Jumio)
    kyc_result = verify_with_provider(kyc_data)
    
    if kyc_result["verified"]:
        # Update user status
        update_user_kyc_status(user_id, "verified", db_connection)
        
        # Set limits
        set_user_limits(user_id, {
            "daily_withdrawal": 10000000000000,  # 10,000 ERG
            "monthly_withdrawal": 100000000000000  # 100,000 ERG
        }, db_connection)
    
    return kyc_result
```

#### 7.7.2 Transaction Monitoring

```python
def monitor_suspicious_activity(user_id, transaction_data, db_connection):
    """Monitor for suspicious activity"""
    
    flags = []
    
    # Check rapid withdrawals
    recent_withdrawals = get_recent_withdrawals(user_id, hours=24, db_connection)
    if len(recent_withdrawals) > 10:
        flags.append("rapid_withdrawals")
    
    # Check large amounts
    if transaction_data["amount"] > 100000000000000:  # 100,000 ERG
        flags.append("large_amount")
    
    # Check new address
    if not is_previously_used_address(user_id, transaction_data["address"], db_connection):
        flags.append("new_address")
    
    # Check withdrawal pattern
    user_history = get_user_transaction_history(user_id, days=30, db_connection)
    if is_unusual_pattern(user_history, transaction_data):
        flags.append("unusual_pattern")
    
    # If suspicious, flag for review
    if flags:
        flag_transaction_for_review(
            user_id,
            transaction_data,
            flags,
            db_connection
        )
        
        # Require additional verification
        return {
            "requires_review": True,
            "flags": flags
        }
    
    return {"requires_review": False}
```

### 7.8 Complete Example: Exchange Integration

```python
#!/usr/bin/env python3
"""
Complete Ergo Exchange Integration Example
"""

import requests
import psycopg2
import logging
from decimal import Decimal
from typing import Dict, List, Optional

# Configuration
ERGO_NODE_URL = "http://127.0.0.1:9053"
API_KEY = "your-api-key-here"
DB_CONFIG = {
    "host": "localhost",
    "database": "exchange_db",
    "user": "exchange_user",
    "password": "secure_password"
}

# Setup logging
logging.basicConfig(
    level=logging.INFO,
    format='%(asctime)s - %(name)s - %(levelname)s - %(message)s'
)
logger = logging.getLogger(__name__)

class ErgoExchangeIntegration:
    """Complete Ergo exchange integration"""
    
    def __init__(self, node_url: str, api_key: str, db_config: Dict):
        self.node_url = node_url
        self.api_key = api_key
        self.db_config = db_config
        self.session = requests.Session()
        self.session.headers.update({"api_key": api_key})
    
    def check_node_health(self) -> bool:
        """Check if Ergo node is healthy"""
        try:
            response = self.session.get(f"{self.node_url}/info", timeout=10)
            if response.status_code == 200:
                info = response.json()
                synced = abs(info["fullHeight"] - info["maxPeerHeight"]) < 5
                has_peers = info["peersCount"] >= 3
                return synced and has_peers
            return False
        except Exception as e:
            logger.error(f"Node health check failed: {e}")
            return False
    
    def generate_deposit_address(self, user_id: int) -> Optional[str]:
        """Generate unique deposit address for user"""
        try:
            response = self.session.get(f"{self.node_url}/wallet/deriveNextKey")
            if response.status_code == 200:
                data = response.json()
                address = data["address"]
                
                # Save to database
                with psycopg2.connect(**self.db_config) as conn:
                    with conn.cursor() as cur:
                        cur.execute(
                            "INSERT INTO user_addresses (user_id, address) VALUES (%s, %s)",
                            (user_id, address)
                        )
                        conn.commit()
                
                logger.info(f"Generated address {address} for user {user_id}")
                return address
            
            logger.error(f"Failed to generate address: {response.text}")
            return None
            
        except Exception as e:
            logger.error(f"Error generating address: {e}")
            return None
    
    def monitor_deposits(self):
        """Monitor blockchain for deposits"""
        try:
            with psycopg2.connect(**self.db_config) as conn:
                with conn.cursor() as cur:
                    # Get last processed height
                    cur.execute("SELECT MAX(last_height) FROM sync_status WHERE sync_type = 'deposits'")
                    result = cur.fetchone()
                    last_height = result[0] if result[0] else 0
                    
                    # Get wallet transactions
                    response = self.session.get(
                        f"{self.node_url}/wallet/transactions",
                        params={"minInclusionHeight": last_height}
                    )
                    
                    if response.status_code == 200:
                        transactions = response.json()
                        
                        for tx in transactions:
                            self._process_transaction(tx, conn)
                        
                        # Update sync status
                        current_height = self._get_current_height()
                        cur.execute(
                            "UPDATE sync_status SET last_height = %s WHERE sync_type = 'deposits'",
                            (current_height,)
                        )
                        conn.commit()
                        
                        logger.info(f"Processed {len(transactions)} transactions")
        
        except Exception as e:
            logger.error(f"Error monitoring deposits: {e}")
    
    def _process_transaction(self, tx: Dict, conn):
        """Process transaction for deposits"""
        tx_id = tx["id"]
        confirmations = tx.get("confirmationsNum", 0)
        
        with conn.cursor() as cur:
            for idx, output in enumerate(tx["outputs"]):
                address = output.get("address")
                value = output["value"]
                
                # Check if address belongs to exchange
                cur.execute(
                    "SELECT user_id FROM user_addresses WHERE address = %s",
                    (address,)
                )
                result = cur.fetchone()
                
                if result:
                    user_id = result[0]
                    
                    # Check if already processed
                    cur.execute(
                        "SELECT id FROM deposits WHERE tx_id = %s AND output_index = %s",
                        (tx_id, idx)
                    )
                    
                    if not cur.fetchone():
                        # Create deposit record
                        cur.execute(
                            """INSERT INTO deposits 
                            (user_id, address, tx_id, output_index, amount_erg, confirmations)
                            VALUES (%s, %s, %s, %s, %s, %s)""",
                            (user_id, address, tx_id, idx, value, confirmations)
                        )
                        logger.info(f"New deposit: {value} nanoERG for user {user_id}")
                    else:
                        # Update confirmations
                        cur.execute(
                            """UPDATE deposits SET confirmations = %s 
                            WHERE tx_id = %s AND output_index = %s""",
                            (confirmations, tx_id, idx)
                        )
                        
                        # Credit if sufficient confirmations
                        if confirmations >= 10:
                            self._credit_deposit(user_id, value, conn)
    
    def _credit_deposit(self, user_id: int, amount: int, conn):
        """Credit user account"""
        with conn.cursor() as cur:
            cur.execute(
                """UPDATE user_balances SET erg_balance = erg_balance + %s 
                WHERE user_id = %s""",
                (amount, user_id)
            )
            logger.info(f"Credited {amount} nanoERG to user {user_id}")
    
    def process_withdrawal(self, user_id: int, address: str, amount: int) -> Dict:
        """Process user withdrawal"""
        try:
            with psycopg2.connect(**self.db_config) as conn:
                with conn.cursor() as cur:
                    # Check balance
                    cur.execute(
                        "SELECT erg_balance FROM user_balances WHERE user_id = %s",
                        (user_id,)
                    )
                    result = cur.fetchone()
                    
                    if not result or result[0] < amount:
                        return {"error": "Insufficient balance"}
                    
                    # Create transaction
                    response = self.session.post(
                        f"{self.node_url}/wallet/payment/send",
                        json={
                            "address": address,
                            "value": amount,
                            "fee": 1000000
                        }
                    )
                    
                    if response.status_code == 200:
                        tx_data = response.json()
                        tx_id = tx_data["id"]
                        
                        # Deduct balance
                        cur.execute(
                            """UPDATE user_balances 
                            SET erg_balance = erg_balance - %s 
                            WHERE user_id = %s""",
                            (amount + 1000000, user_id)
                        )
                        
                        # Record withdrawal
                        cur.execute(
                            """INSERT INTO withdrawals 
                            (user_id, address, amount_erg, tx_id, status)
                            VALUES (%s, %s, %s, %s, 'sent')""",
                            (user_id, address, amount, tx_id)
                        )
                        
                        conn.commit()
                        
                        logger.info(f"Withdrawal processed: {tx_id}")
                        return {"success": True, "tx_id": tx_id}
                    else:
                        return {"error": response.text}
        
        except Exception as e:
            logger.error(f"Withdrawal error: {e}")
            return {"error": str(e)}
    
    def _get_current_height(self) -> int:
        """Get current blockchain height"""
        response = self.session.get(f"{self.node_url}/info")
        if response.status_code == 200:
            return response.json()["fullHeight"]
        return 0

# Main execution
if __name__ == "__main__":
    exchange = ErgoExchangeIntegration(ERGO_NODE_URL, API_KEY, DB_CONFIG)
    
    # Check node health
    if not exchange.check_node_health():
        logger.error("Node is not healthy!")
        exit(1)
    
    # Monitor deposits (run continuously)
    import time
    while True:
        exchange.monitor_deposits()
        time.sleep(30)  # Check every 30 seconds
```

---

## 8. Conclusion

This comprehensive documentation covers all aspects of Ergo Wallet API and Exchange Integration, including:

✅ **Basic Operations**: Wallet setup, address generation, balance checking  
✅ **Transaction Handling**: Creating, signing, and broadcasting transactions  
✅ **Exchange Integration**: Complete deposit/withdrawal workflows  
✅ **Mining Pools**: Payout processing and miner management  
✅ **Token Support**: Native token integration  
✅ **Security**: Multi-sig, 2FA, audit logging, rate limiting  
✅ **Performance**: Database optimization, caching, async processing  
✅ **Disaster Recovery**: Backup strategies and recovery procedures  
✅ **Testing**: Unit tests, integration tests, load testing  
✅ **Compliance**: KYC/AML integration and transaction monitoring  

### Additional Resources

- **Official Documentation**: https://docs.ergoplatform.com/
- **API Reference**: https://api.ergoplatform.com/
- **Swagger UI**: http://127.0.0.1:9053/swagger (when node running)
- **Community**: https://www.ergoforum.org/
- **Discord**: https://discord.gg/ergo
- **GitHub**: https://github.com/ergoplatform/ergo

### Support

For technical support:
- Open an issue on GitHub: https://github.com/ergoplatform/ergo/issues
- Community forum: https://www.ergoforum.org/

---

**End of Documentation**

*Last Updated: December 14, 2025*  
*Version: 1.0*  
*Author: Ergo Platform Community*
