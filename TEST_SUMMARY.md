# Version 6.0 Activation Parameter Processing Test

## Overview
This test verifies that the Ergo node can successfully process parameters generated after the version 6.0 activation (EIP-50 implementation). The test focuses on ensuring that parameter processing, validation, and state management work correctly with the new protocol version.

## Test Files Created

### 1. Version60ActivationSpec.scala
Comprehensive test that simulates the complete soft-fork voting process leading to version 6.0 activation and verifies parameter processing.

### 2. Version60ParameterProcessingSpec.scala 
Simplified test focusing on core parameter processing functionality after version 6.0 activation.

## Key Test Scenarios

### 1. Soft-Fork Voting Simulation
- Simulates the complete soft-fork voting process from version 3 to version 4 (6.0)
- Tests voting epochs, activation epochs, and final activation
- Verifies that block version increments correctly from 3 to 4

### 2. Parameter Validation
- Verifies all system parameters remain valid and processable after activation
- Tests parameter conversion to/from extension format
- Ensures parameter updates work correctly post-activation

### 3. State Context Processing
- Tests that ErgoStateContext works correctly with version 6.0 parameters
- Verifies validation rule handling with the new protocol version
- Ensures no exceptions are thrown during state processing

### 4. Header Processing
- Tests processing of blocks with version 6.0 headers
- Verifies that the node can handle the new protocol version
- Ensures backward compatibility is maintained

## Key Parameters Tested

- **Block Version**: Should increment from 3 to 4 after activation
- **Storage Fee Factor**: Should remain positive and processable
- **Min Value Per Byte**: Should remain positive
- **Block Size/Cost Limits**: Should remain valid
- **Transaction Costs**: Input, output, data input, and token access costs

## Validation Rules

Tests ensure that:
- All disableable validation rules can be properly checked
- Validation settings work correctly with version 6.0 parameters
- No exceptions are thrown during rule validation

## Soft-Fork State Management

Verifies that soft-fork voting state is properly cleaned up after activation:
- `softForkStartingHeight` is cleared
- `softForkVotesCollected` is cleared
- Only the block version parameter persists

## Running the Tests

```bash
sbt "testOnly org.ergoplatform.settings.Version60ActivationSpec"
sbt "testOnly org.ergoplatform.settings.Version60ParameterProcessingSpec"
```

## Test Coverage

The tests cover:
- ✅ Parameter creation and validation
- ✅ Soft-fork voting simulation
- ✅ Parameter serialization/deserialization
- ✅ State context processing
- ✅ Validation rule handling
- ✅ Header processing with new version
- ✅ Backward compatibility

These tests ensure that the node will successfully process parameters generated after the 6.0 activation as requested.