# Blockchain Reset Implementation - Summary

## Bounty Completion Summary
**Issue**: https://github.com/ergoplatform/ergo/issues/2232  
**Requirement**: Create API method to forget all DB data related to blocks after given height

## Implementation Overview

### 1. API Endpoint
- **URL**: `POST /blocks/reset`
- **Request Body**: `{"height": <block_height>}`
- **Response**: JSON with success status and message

### 2. Core Components Modified

#### A. ErgoNodeViewHolder.scala
**Location**: `/src/main/scala/org/ergoplatform/nodeView/ErgoNodeViewHolder.scala`

**Changes Made**:
1. **Added New Message Type**:
   ```scala
   case class ResetBlockchainTo(height: Int)
   ```

2. **Added Reset Handler**:
   ```scala
   private def handleBlockchainReset: Receive = {
     case ResetBlockchainTo(height) =>
       log.info(s"Received request to reset blockchain to height $height")
       val result = resetBlockchainToHeight(height)
       sender() ! result
   }
   ```

3. **Implemented Reset Logic**:
   ```scala
   private def resetBlockchainToHeight(targetHeight: Int): Try[String]
   ```

**Key Features**:
- Comprehensive validation (negative height, height > current, block exists)
- Safe header removal (reverse order, error handling)
- State rollback to target height
- Progress logging for large operations
- Proper error handling and recovery

#### B. BlocksApiRoute.scala
**Location**: `/src/main/scala/org/ergoplatform/http/api/BlocksApiRoute.scala`

**Changes Made**:
1. **Added Reset Endpoint**:
   ```scala
   def resetBlockchainR: Route = (post & path("reset") & entity(as[ResetRequest]))
   ```

2. **Added Request Case Class**:
   ```scala
   case class ResetRequest(height: Int)
   ```

**Key Features**:
- Input validation at API level
- Proper HTTP status codes
- Timeout handling (60 seconds)
- Detailed error messages
- Structured JSON responses

#### C. BlocksApiRouteSpec.scala
**Location**: `/src/test/scala/org/ergoplatform/http/routes/BlocksApiRouteSpec.scala`

**Added Tests**:
1. **Successful Reset Test**: Valid height reset
2. **Negative Height Test**: Rejects negative heights
3. **Invalid Height Test**: Rejects heights higher than current

## 3. How It Works

### Reset Process Flow:
1. **API Validation**: Check request format and basic validation
2. **NodeView Validation**: Comprehensive height validation and block existence
3. **Header Removal**: Remove headers and block parts in reverse order
4. **State Rollback**: Roll back blockchain state to target height
5. **Response**: Return success/failure with detailed message

### Safety Features:
- **Input Validation**: Multiple layers of validation
- **Atomic Operations**: Use existing `forgetHeader()` for safe removal
- **Error Recovery**: Detailed error messages for troubleshooting
- **Progress Logging**: Track operation progress for large resets
- **Rollback Safety**: Remove blocks in reverse order (highest to lowest)

## 4. Usage Examples

### Successful Reset:
```bash
curl -X POST http://localhost:9053/blocks/reset \
  -H "Content-Type: application/json" \
  -d '{"height": 100}'
```

**Response**:
```json
{
  "success": true,
  "message": "Blockchain successfully reset from height 150 to height 100",
  "resetHeight": 100
}
```

### Error Cases:

#### Invalid Height (too high):
```bash
curl -X POST http://localhost:9053/blocks/reset \
  -d '{"height": 999999}'
```

**Response**: `400 Bad Request`
```
Invalid request: Target height 999999 is higher than current height 150
```

#### Negative Height:
```bash
curl -X POST http://localhost:9053/blocks/reset \
  -d '{"height": -1}'
```

**Response**: `400 Bad Request`
```
Height must be non-negative
```

## 5. Technical Details

### Database Operations:
- Uses existing `ErgoHistory.forgetHeader()` method
- Automatically handles removal of:
  - Block headers
  - Block transactions  
  - AD proofs
  - Extensions
  - Database indexes

### State Management:
- Rolls back UTXO state to target height
- Updates node view atomically
- Maintains data consistency

### Extra Indexing:
- Automatically handles extra index rollback
- Preserves address and token indexes consistency

## 6. Benefits

1. **Avoids Full Resync**: Targeted removal instead of complete redownload
2. **Database Repair**: Fixes corrupted blockchain databases
3. **Safe Operations**: Multiple validation layers prevent data loss
4. **API Integration**: Easy to use via REST API
5. **Comprehensive Logging**: Detailed operation tracking

## 7. Limitations & Considerations

1. **Destructive Operation**: Cannot be undone (blocks are permanently removed)
2. **Network Isolation**: Should be used when node is not actively syncing
3. **Large Operations**: May take time for significant height differences
4. **State Consistency**: Requires node to be in consistent state initially

## 8. Files Modified

1. `ErgoNodeViewHolder.scala` - Core reset logic
2. `BlocksApiRoute.scala` - HTTP API endpoint
3. `BlocksApiRouteSpec.scala` - Unit tests

## Conclusion

This implementation provides a comprehensive solution for the requested blockchain reset functionality. It addresses the core issue of avoiding full resync when the blockchain database is in an invalid state, while maintaining safety and providing clear error handling.

The solution follows Ergo's existing patterns and integrates seamlessly with the current architecture, making it ready for production use.