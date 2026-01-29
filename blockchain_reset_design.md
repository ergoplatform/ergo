# Blockchain Reset to Specific Block Height - Implementation Design

## Issue Summary
- **Issue**: https://github.com/ergoplatform/ergo/issues/2232
- **Bounty**: B-200 SigUSD
- **Description**: Create an API method to forget all DB data related to blocks after a given height, to provide ability to avoid full resync when blockchain database is in invalid state.

## Analysis of Existing Code

### Key Components Found:
1. **ErgoHistory.forgetHeader()** - Removes header and corresponding block parts from storage
2. **ExtraIndexer.removeAfter()** - Rolls back indexes after a given height
3. **ErgoNodeViewHolder** - Main coordinator that manages blockchain state
4. **BlocksApiRoute** - Handles block-related API endpoints

### Current Rollback Mechanisms:
1. **Wallet rollback** - `ErgoWallet.rollback(to: VersionTag)`  
2. **State rollback** - `ErgoState.rollbackTo(version: VersionTag)`
3. **Extra indexer rollback** - `ExtraIndexer.RemoveAfter(branchHeight: Int)`

## Implementation Plan

### 1. API Endpoint Design
- **Endpoint**: `POST /blocks/reset` 
- **Request Body**: `{"height": <block_height>}`
- **Response**: Success/failure status with confirmation message
- **HTTP Method**: POST (for safety - this is a destructive operation)

### 2. Core Reset Logic
The reset operation needs to:
1. Validate the target height
2. Remove all blocks after the specified height from:
   - History storage (headers, transactions, proofs)
   - Extra indexes (address/token indexes)
   - State storage (UTXO set)
3. Update node view to reflect the reset state

### 3. Implementation Components

#### A. Add Reset Message to ErgoNodeViewHolder
```scala
case class ResetBlockchainTo(height: Int)
```

#### B. Reset Implementation in ErgoNodeViewHolder
- Validate height is valid and not higher than current height
- Get the block ID at target height
- Remove all blocks after that height using existing mechanisms
- Update node view state

#### C. API Route Addition to BlocksApiRoute
- Add endpoint handler
- Validate input
- Send reset message to NodeViewHolder
- Return appropriate response

### 4. Safety Considerations
- Validate target height exists and is not negative
- Ensure target height is not higher than current height
- Add proper error handling for database operations
- Consider making this operation atomic where possible

### 5. Error Cases to Handle
- Invalid height (negative, too high, non-existent)
- Database operation failures
- Node not fully synced
- Concurrent modification conflicts