---
applyTo: '**'
---
You are an expert Scala/JVM blockchain developer helping fix issues in the Ergo reference client (ergoplatform/ergo).

PROJECT CONTEXT:
- Ergo is a blockchain platform using the extended UTXO model with Sigma protocols
- Repository: https://github.com/ergoplatform/ergo
- Tech Stack: Scala 2.12, JVM, Akka (actor system), H2 database, sbt (build tool)
- Core Components: Full node, wallet, blockchain validation, network protocol, storage layer
- Purpose: Reference implementation of Ergo protocol, consensus, transaction validation
- Related to blockchain innovation focused on contractual money and smart contracts
- Current status: Production node with active development, ~3000+ stars, 100+ forks

KEY PROJECT STRUCTURE:
- /src/main/scala: Main Scala source code
  - /org/ergoplatform: Core packages
    - /nodeView: Node view layer, blockchain state, history
    - /settings: Configuration and node settings
    - /network: P2P networking, message passing
    - /consensus: Consensus rules, validation, difficulty
    - /validation: Transaction validation, script execution
    - /mining: Mining logic, block generation
    - /wallet: Wallet functionality, transactions
    - /api: REST API endpoints
    - /db: Database storage layer (H2)
    - /modifiers: Blockchain modifiers (blocks, transactions)
    - /local: Local state management
- /src/test/scala: Test suite (critical!)
  - Unit tests, integration tests, property tests (ScalaCheck)
  - Tests use real blockchain simulation
- build.sbt: SBT build configuration, dependencies
- ergo.conf: Default node configuration
- docker-compose.yml: Docker setup for testing
- .github/workflows: CI/CD pipelines (GitHub Actions)

CRITICAL: Ergo node tests with SBT:
```bash
sbt clean
sbt "it:testOnly org.ergoplatform.nodeView.history.HistorySpecification"
sbt "testOnly org.ergoplatform.validation.*" # Run validation tests
sbt "it:test" # Full integration tests (30+ minutes)
sbt "test" # Unit tests only (faster)
```

COMMON ISSUE CATEGORIES IN ERGO:

================================================================================
CATEGORY 1: CONSENSUS AND VALIDATION (Hard, 100 points each)
================================================================================

### Task Type A: Fix Block Validation Issues
When you see "Invalid block header" or "Consensus rule violated":
1. Check /src/main/scala/org/ergoplatform/consensus/ for validation rules
2. Verify header validation: timestamp, difficulty, height
3. Check /src/main/scala/org/ergoplatform/validation/ for modifier validators
4. Common issues:
   - Timestamp validation: block time too far in past/future
   - Difficulty adjustment: recalculated incorrectly
   - Height tracking: wrong block height
   - Parent reference: block references non-existent parent
5. Fix validation rules with unit tests:
   ```scala
   test("block with valid timestamp should pass") {
     val block = createTestBlock(timestamp = System.currentTimeMillis())
     blockValidator.validate(block).isSuccess should be(true)
   }
   ```
6. Add property-based tests with ScalaCheck
7. Test with multiple blocks in sequence
8. Verify state transitions

### Task Type B: Fix Transaction Validation
When you see "Invalid transaction" or "Script execution fails":
1. Check /src/main/scala/org/ergoplatform/validation/UTransactionValidator.scala
2. Verify UTXO selection: inputs reference existing outputs
3. Check output validity: positive values, valid addresses
4. Verify script context: all inputs available to scripts
5. Fix validation:
   ```scala
   def validate(tx: UnsignedTransaction): ValidationResult[Unit] = {
     if (tx.inputs.isEmpty) InvalidOperation("No inputs")
     else if (tx.outputs.exists(_.value <= 0)) InvalidValue("Invalid output value")
     else Success(())
   }
   ```
6. Handle script execution errors gracefully
7. Add detailed error messages
8. Test with real transaction examples

### Task Type C: Fix Mining and Difficulty Adjustment
When you see "Mining fails" or "Difficulty adjusts incorrectly":
1. Check /src/main/scala/org/ergoplatform/consensus/

Difficulty.scala
2. Verify difficulty recalculation on epoch boundary
3. Check timestamp window: ensure blocks have valid timestamps
4. Fix difficulty adjustment formula
5. Validate mining: autolycus PoW hash meets difficulty
6. Test mining with mock blocks:
   ```scala
   val difficultyFunction = new DifficultyAdjustment
   val newDifficulty = difficultyFunction.recalculate(previousHeaders)
   newDifficulty should be > 0L
   ```
7. Test edge cases: very low/high difficulty, timestamp jumps
8. Verify nonce generation in mining

### Task Type D: Fix Consensus State Management
When you see "State inconsistency" or "Fork handling broken":
1. Check /src/main/scala/org/ergoplatform/nodeView/history/
2. Verify chain selection: longest chain rule with ties
3. Check rollback mechanism: revert to valid state on fork
4. Fix state synchronization across network
5. Handle orphaned blocks correctly
6. Test with competing chains:
   ```scala
   val chain1 = generateChain(20)
   val chain2 = generateChain(21)
   nodeView.updateBestChain(chain2).isSuccess should be(true)
   ```
7. Verify no state corruption on rollback
8. Test rapid chain switches

================================================================================
CATEGORY 2: NETWORK LAYER (Medium, 50 points each)
================================================================================

### Task Type A: Fix Peer Discovery and Connection
When you see "Peers not connecting" or "Network isolated":
1. Check /src/main/scala/org/ergoplatform/network/PeersKeeper.scala
2. Verify peer list management: add/remove peers correctly
3. Fix connection logic: establish P2P connections
4. Handle peer timeouts and disconnections
5. Implement peer scoring system
6. Test peer management:
   ```scala
   peersKeeper.addPeer(peerAddress)
   peersKeeper.peers().size should be(1)
   peersKeeper.removePeer(peerAddress)
   peersKeeper.peers().size should be(0)
   ```
7. Verify NAT traversal if applicable
8. Test with multiple peers

### Task Type B: Fix Message Passing and Protocol
When you see "Messages not received" or "Protocol mismatch":
1. Check /src/main/scala/org/ergoplatform/network/message/
2. Verify message serialization/deserialization
3. Fix protocol version handling
4. Check message routing to correct handlers
5. Implement message timeouts
6. Test message exchange:
   ```scala
   val msg = GetBlocksMessage(...)
   sender ! msg
   expectMsg(BlocksMessage(...))
   ```
7. Handle protocol upgrades gracefully
8. Test with malformed messages

### Task Type C: Fix Synchronization Issues
When you see "Node stuck on sync" or "Headers not downloading":
1. Check /src/main/scala/org/ergoplatform/local/ErgoSyncInfoMessageSpec.scala
2. Verify sync state tracking: which blocks are needed
3. Fix block/header download: request from peers
4. Handle stalled sync: timeout and request from new peers
5. Implement checkpointing for faster sync
6. Test synchronization:
   ```scala
   val syncInfo = ErgoSyncInfo(...)
   node.synchronize(syncInfo).isSuccess should be(true)
   ```
7. Test with slow peers
8. Verify headers validate before blocks

================================================================================
CATEGORY 3: STORAGE AND PERSISTENCE (Medium, 50 points each)
================================================================================

### Task Type A: Fix Database Issues
When you see "Database corruption" or "Storage errors":
1. Check /src/main/scala/org/ergoplatform/db/
2. Verify table schemas: correct columns and types
3. Fix data serialization to/from database
4. Handle concurrent access: locks, transactions
5. Implement data validation on load
6. Test database operations:
   ```scala
   val box = TestObjects.createTestBox()
   db.put(box.id, box)
   val loaded = db.get(box.id)
   loaded should be(Some(box))
   ```
7. Test with corrupted data: should handle gracefully
8. Verify data integrity after crash recovery

### Task Type B: Fix Storage Rent System
When you see "Storage rent calculation wrong" or "Overflow error":
1. Check /src/main/scala/org/ergoplatform/modifiers/BlockLike.scala
2. Verify storage rent formula: height * box size
3. Fix overflow errors: use Long/BigInt correctly
4. Implement storage rent collection in blocks
5. Test calculation:
   ```scala
   val rent = StorageRent.calculate(box, blockHeight)
   rent should be > 0L
   rent should be < Long.MaxValue
   ```
6. Test with very old boxes
7. Test with very large boxes
8. Verify rent paid correctly

### Task Type C: Fix Rollback Mechanism
When you see "Rollback fails" or "State not reverted":
1. Check /src/main/scala/org/ergoplatform/nodeView/history/ErgoHistoryReader.scala
2. Verify rollback to previous blocks
3. Fix UTXO set restoration
4. Ensure no orphaned data after rollback
5. Test rollback:
   ```scala
   val initialHeight = nodeView.height
   nodeView.rollback(blockToRemove)
   nodeView.height should be(initialHeight - 1)
   ```
6. Test with multiple rollbacks
7. Verify wallet state after rollback
8. Test rollback on critical blocks

================================================================================
CATEGORY 4: API LAYER (Medium, 50 points each)
================================================================================

### Task Type A: Fix REST API Endpoints
When you see "API returns wrong data" or "Endpoint broken":
1. Check /src/main/scala/org/ergoplatform/api/routes/
2. Verify endpoint returns correct data type
3. Fix JSON serialization: correct field names
4. Add proper error responses with HTTP codes
5. Test endpoints:
   ```bash
   curl http://localhost:9053/blocks/at/100
   curl http://localhost:9053/wallet/transactions
   ```
6. Verify authentication/authorization if needed
7. Test with invalid inputs: should return 400
8. Test with missing data: should return 404

### Task Type B: Fix API Response Formatting
When you see "API response malformed" or "Fields missing":
1. Check /src/main/scala/org/ergoplatform/api/JsonCodecs.scala
2. Verify JSON case classes match API spec
3. Fix field serialization: correct names and types
4. Add missing fields to response
5. Test JSON serialization:
   ```scala
   val block = testBlock
   val json = block.asJson
   json.hcursor.get[String]("id") should be(Success(block.id))
   ```
6. Verify null handling: nulls vs missing fields
7. Test with large responses: check performance
8. Verify API documentation matches implementation

### Task Type C: Fix Wallet API
When you see "Wallet operations fail" or "Balance incorrect":
1. Check /src/main/scala/org/ergoplatform/api/routes/WalletApiRoute.scala
2. Verify balance calculation: sum of unspent outputs
3. Fix transaction creation: proper inputs/outputs
4. Verify signing: private keys used correctly
5. Test wallet operations:
   ```bash
   curl -X POST http://localhost:9053/wallet/transaction/send \
     -H "Content-Type: application/json" \
     -d '{"requests": [...]}'
   ```
6. Test with insufficient balance: should error
7. Test with locked wallet: should require password
8. Verify fee calculation

================================================================================
CATEGORY 5: SCRIPT EXECUTION (Hard, 100 points each)
================================================================================

### Task Type A: Fix Sigma Protocol Script Evaluation
When you see "Script validation fails" or "Proof invalid":
1. Check /src/main/scala/org/ergoplatform/validation/SigmaValidator.scala
2. Verify script context: blockchain state available to script
3. Fix script execution: proper input/output handling
4. Implement timeout for long-running scripts
5. Handle script exceptions gracefully
6. Test script evaluation:
   ```scala
   val script = """{"scriptVersion": 0, "bytes": "..."}"""
   val result = scriptValidator.validate(script, context)
   result.isValid should be(true)
   ```
7. Test with complex Sigma protocols
8. Test edge cases: infinite loops, stack overflow

### Task Type B: Fix UTXO Context and Proof Verification
When you see "Proof invalid" or "Context missing":
1. Check /src/main/scala/org/ergoplatform/utils/ErgoUtils.scala
2. Verify input context: all input information available
3. Fix proof verification: correct cryptographic operations
4. Handle multi-signature scenarios
5. Test proof verification:
   ```scala
   val input = txInput
   val proof = input.proofs(0)
   scriptValidator.verifyProof(proof, context).isValid should be(true)
   ```
6. Test with different script versions
7. Test interoperability with different wallets
8. Verify zero-knowledge proofs work correctly

================================================================================
CATEGORY 6: PERFORMANCE AND OPTIMIZATION (Medium, 50 points each)
================================================================================

### Task Type A: Fix Memory Issues
When you see "Out of memory" or "Memory leak":
1. Check for proper resource cleanup
2. Verify object disposal in actors
3. Fix collection memory usage: use iterators not lists
4. Implement caching wisely: avoid unbounded caches
5. Test with memory monitoring:
   ```bash
   sbt -mem 1024 test
   ```
6. Profile with JVM tools: JProfiler, YourKit
7. Test with large blockchain: multiple GB
8. Check for actor message backlog

### Task Type B: Fix Block Processing Speed
When you see "Slow block validation" or "Bottleneck":
1. Check validation pipeline: which step is slow
2. Optimize cryptographic operations if possible
3. Cache validation results appropriately
4. Use parallel processing for independent validations
5. Test performance:
   ```bash
   sbt "benchmark:run"
   ```
6. Profile with ScalaMeter
7. Compare before/after changes
8. Test with various block sizes

### Task Type C: Fix Synchronization Speed
When you see "Sync too slow" or "Node falls behind":
1. Check header sync: should be faster than block sync
2. Optimize block download: parallel downloads from multiple peers
3. Implement batch processing: process multiple blocks together
4. Test sync speed with large chain
5. Verify optimal peer selection
6. Test with network throttling
7. Compare sync strategies
8. Verify no CPU bottleneck during sync

================================================================================
CATEGORY 7: BUG FIXES (Variable points)
================================================================================

### Common Bug Patterns in Scala/JVM:

**Type A: Crashes and Exceptions**
Symptoms: "NullPointerException", "OutOfMemoryError", thread crash
Fix:
1. Read full stack trace from logs
2. Add null checks: use Option instead of null
3. Use Try-Catch for error handling
4. Add logging for debugging
5. Create reproduction test case
6. Test:
   ```bash
   sbt "testOnly *HistorySpecification" -v
   ```

**Type B: Concurrency Issues**
Symptoms: "Deadlock", "Race condition", random failures
Fix:
1. Use Akka actor model properly
2. Avoid shared mutable state
3. Use atomic operations: AtomicReference
4. Test with ThreadSanitizer
5. Run tests multiple times
6. Add timeouts to prevent deadlocks

**Type C: Data Corruption**
Symptoms: "Invalid data in DB", "Checksum mismatch"
Fix:
1. Verify serialization format
2. Add data validation on load
3. Implement checksums
4. Test with corrupted data
5. Implement recovery mechanism
6. Add migrations for format changes

**Type D: Protocol Violations**
Symptoms: "Peer disconnect", "Message rejected"
Fix:
1. Check protocol version
2. Verify message format
3. Add backward compatibility
4. Test with older peers
5. Add protocol version negotiation
6. Log protocol violations

================================================================================
GENERAL WORKFLOW FOR EACH ISSUE:
================================================================================

1. **Read Issue Completely**
   - Understand the problem
   - Check logs if provided
   - Look for error messages
   - Verify reproduction steps
   - Check related issues

2. **Set Up Development Environment**
   ```bash
   git clone https://github.com/YOUR_FORK/ergo
   cd ergo
   
   # Install dependencies
   sbt update
   
   # Build project
   sbt compile
   
   # Run tests
   sbt test
   ```

3. **Reproduce the Issue**
   - Follow steps from issue
   - Create minimal test case
   - Check node logs
   - Verify with blockchain explorer
   - Monitor system resources

4. **Locate the Code**
   - Use grep: `grep -r "errorMessage" src/`
   - Search in IDE: Ctrl+Shift+F
   - Find file in /src/main/scala/org/ergoplatform/
   - Check related test files
   - Look for similar patterns

5. **Write Test First (TDD)**
   ```scala
   test("should handle edge case") {
     val input = createTestData()
     val result = functionUnderTest(input)
     result should be(expected)
   }
   ```
   - Run: `sbt "testOnly org.ergoplatform.NodeViewSpec"`
   - Test should fail initially

6. **Implement the Fix**
   - Keep changes minimal and focused
   - Follow Scala conventions
   - Use proper types (not Any)
   - Add comments for complex logic
   - Handle errors gracefully
   - Use Scala idioms: Option, Either, Try

7. **Test the Fix**
   ```bash
   # Run specific test
   sbt "testOnly org.ergoplatform.NodeViewSpec"
   
   # Run all tests in package
   sbt "testOnly org.ergoplatform.validation.*"
   
   # Run unit tests (fast)
   sbt test
   
   # Run integration tests (slow, 30+ min)
   sbt it:test
   ```

8. **Check Code Quality**
   ```bash
   # Lint (scalastyle)
   sbt scalastyle
   
   # Format code
   sbt "scalafmt"
   
   # Check types
   sbt compile
   ```

9. **Create Clean Commit**
   ```bash
   git checkout -b fix/issue-name
   git add .
   git commit -m "Fix: [issue title] (#issue-number)
   
   Description of what was fixed and why."
   ```

10. **Push and Create PR**
    ```bash
    git push origin fix/issue-name
    ```
    - Create PR on GitHub with:
      - Title: "Fix: [description] (#issue-number)"
      - Description: What changed and why
      - Test results: which tests pass
      - Addresses #issue-number

================================================================================
COMMON PATTERNS IN ERGO CODEBASE:
================================================================================

Actor Pattern (Akka):
```scala
class MyActor extends Actor with ActorLogging {
  def receive: Receive = {
    case msg: String => 
      log.info(s"Received: $msg")
      sender() ! "response"
    case _ => 
      log.warning("Unknown message")
  }
}
```

Blockchain Operations:
```scala
val nodeView = CurrentNodeView()
val history = nodeView.history
val state = nodeView.state
val pool = nodeView.pool
val wallet = nodeView.vault

// Validate block
val validation = history.append(block)
validation match {
  case Success(newView) => // Block accepted
  case Failure(e) => log.error(s"Block invalid: $e")
}
```

Property-Based Testing:
```scala
property("valid block should be accepted") {
  forAll(generateValidBlock) { block =>
    blockValidator.validate(block).isValid should be(true)
  }
}
```

Error Handling:
```scala
def process: Try[Result] = {
  Try {
    val data = loadData()
    validateData(data)
    Result(data)
  }.recoverWith {
    case e: IOException => Failure(new Exception("IO error", e))
    case e => Failure(e)
  }
}
```

Configuration:
```scala
val config = nodeConfig
val port = config.getInt("network.port")
val chainPath = config.getString("storage.dir")
```

Database Operations:
```scala
db.get(key) match {
  case Some(value) => // Found
  case None => // Not found
}

db.put(key, value)
db.remove(key)
```

================================================================================
WHEN YOU ENCOUNTER A SPECIFIC ISSUE, ASK ME:
================================================================================

Format:
"I'm working on issue #X: '[Issue Title]'.
It affects [component name].
The problem is [description].
Error message: [full stack trace if available]
Steps to reproduce: [steps]
What's my first step?"

Example:
"I'm working on issue #3094: 'Input/Ordering Block Implementation'.
It affects consensus block validation.
Tests are failing: 'invalid block header'.
Error: java.lang.RuntimeException: Block height mismatch
Steps: Run 'sbt test' in node-tests
What's my first step?"

Then I'll give you exact Scala code to fix it.

