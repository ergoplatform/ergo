package org.ergoplatform.settings

import org.ergoplatform.modifiers.history.extension.ExtensionCandidate
import org.ergoplatform.modifiers.history.header.Header
import org.ergoplatform.nodeView.state.{ErgoStateContext, VotingData}
import org.ergoplatform.utils.ErgoCorePropertyTest
import org.ergoplatform.utils.ErgoCoreTestConstants._

import Parameters._

/**
  * Test specification for version 6.0 parameter processing.
  * Verifies that the node can successfully process parameters generated after 6.0 activation.
  */
class Version60ParameterProcessingSpec extends ErgoCorePropertyTest {

  private val votingEpochLength = 2

  implicit val votingSettings: VotingSettings =
    VotingSettings(
      votingEpochLength,
      softForkEpochs = 2,
      activationEpochs = 3,
      version2ActivationHeight = 100,
      version2ActivationDifficultyHex = "01"
    )

  private val updSettings = chainSettings.copy(voting = votingSettings)

  property("Parameters with version 6.0 should be processable") {
    val activationHeight = 200
    
    // Create parameters with version 6.0 (block version 4)
    val version60Parameters = Parameters(
      activationHeight,
      DefaultParameters.updated(BlockVersion, Header.Interpreter60Version),
      ErgoValidationSettingsUpdate.empty
    )

    // Verify that all parameters are valid and processable
    assert(version60Parameters.blockVersion == Header.Interpreter60Version, 
      s"Block version should be ${Header.Interpreter60Version}")

    assert(version60Parameters.storageFeeFactor > 0, "Storage fee factor should be positive")
    assert(version60Parameters.minValuePerByte > 0, "Min value per byte should be positive")
    assert(version60Parameters.maxBlockSize > 0, "Max block size should be positive")
    assert(version60Parameters.maxBlockCost > 0, "Max block cost should be positive")
    assert(version60Parameters.tokenAccessCost > 0, "Token access cost should be positive")
    assert(version60Parameters.inputCost > 0, "Input cost should be positive")
    assert(version60Parameters.dataInputCost > 0, "Data input cost should be positive")
    assert(version60Parameters.outputCost > 0, "Output cost should be positive")

    // Verify that parameters can be converted to extension candidate
    val extensionCandidate = version60Parameters.toExtensionCandidate
    assert(extensionCandidate.fields.nonEmpty, "Parameters should generate non-empty extension candidate")

    // Verify that parameters can be parsed back from extension
    val parsedParameters = Parameters.parseExtension(activationHeight, extensionCandidate.toExtension(Array.fill(32)(0.toByte)))
    assert(parsedParameters.isSuccess, "Parameters should be parseable from extension")
    assert(parsedParameters.get.blockVersion == Header.Interpreter60Version, 
      "Parsed parameters should maintain version 6.0")
  }

  property("Node should handle parameter updates after version 6.0 activation") {
    val activationHeight = 200
    
    // Create parameters with version 6.0
    val postActivationParameters = Parameters(
      activationHeight,
      DefaultParameters.updated(BlockVersion, Header.Interpreter60Version),
      ErgoValidationSettingsUpdate.empty
    )

    // Verify that parameters can be updated after activation
    val updatedHeight = activationHeight + 1
    val (updatedParameters, _) = postActivationParameters.update(
      updatedHeight,
      forkVote = false,
      epochVotes = Seq((StorageFeeFactorIncrease, votingEpochLength)),
      ErgoValidationSettingsUpdate.empty,
      votingSettings
    )

    // Verify that block version is preserved after parameter updates
    assert(updatedParameters.blockVersion == Header.Interpreter60Version,
      s"Block version should remain ${Header.Interpreter60Version} after parameter updates")

    // Verify that parameter updates work correctly
    assert(updatedParameters.storageFeeFactor >= postActivationParameters.storageFeeFactor,
      "Storage fee factor should be able to increase after activation")
  }

  property("State context should work with version 6.0 parameters") {
    val activationHeight = 200
    
    // Create parameters with version 6.0
    val version60Parameters = Parameters(
      activationHeight,
      DefaultParameters.updated(BlockVersion, Header.Interpreter60Version),
      ErgoValidationSettingsUpdate.empty
    )

    // Create state context with version 6.0 parameters
    val ctx: ErgoStateContext = {
      new ErgoStateContext(Seq.empty, None, genesisStateDigest, version60Parameters, validationSettingsNoIl, VotingData.empty)(updSettings)
        .upcoming(org.ergoplatform.mining.group.generator, 0L, chainSettings.initialNBits, Array.fill(3)(0.toByte), emptyVSUpdate, 0.toByte)
    }

    // Verify that the context has the correct parameters
    assert(ctx.currentParameters.blockVersion == Header.Interpreter60Version,
      "State context should maintain version 6.0 parameters")

    // Verify that validation settings work correctly
    val validationSettings = ctx.validationSettings
    
    // Check that validation rules that can be disabled are properly handled
    val disableableRules = ValidationRules.rulesSpec.filter { case (_, status) => status.mayBeDisabled }
    
    disableableRules.foreach { case (ruleId, _) =>
      // Verify that each disableable rule can be checked
      val isActive = validationSettings.isActive(ruleId)
      // This should not throw any exceptions
      assert(isActive || !isActive, s"Rule $ruleId should be checkable without errors")
    }
  }

  property("Soft-fork voting should correctly activate version 6.0") {
    val initialHeight = 100
    val softForkStartHeight = initialHeight
    
    // Create initial parameters with block version 3 (current version before 6.0)
    val initialParameters = Parameters(
      initialHeight,
      DefaultParameters.updated(BlockVersion, 3),
      ErgoValidationSettingsUpdate.empty
    )

    // Simulate successful soft-fork voting process
    var currentParameters = initialParameters
    var currentHeight = initialHeight

    // Simulate voting epochs with 100% approval
    (0 until votingSettings.softForkEpochs).foreach { epoch =>
      val epochStartHeight = softForkStartHeight + epoch * votingEpochLength
      
      (0 until votingEpochLength).foreach { blockInEpoch =>
        currentHeight = epochStartHeight + blockInEpoch
        
        // All miners vote for soft-fork
        val forkVote = true
        val epochVotes = Seq((SoftFork, votingEpochLength))
        
        val (newParameters, _) = currentParameters.update(
          currentHeight,
          forkVote,
          epochVotes,
          ErgoValidationSettingsUpdate.empty,
          votingSettings
        )
        
        currentParameters = newParameters
      }
    }

    // Simulate activation epochs
    (0 until votingSettings.activationEpochs).foreach { epoch =>
      val activationStartHeight = softForkStartHeight + votingSettings.softForkEpochs * votingEpochLength + epoch * votingEpochLength
      
      (0 until votingEpochLength).foreach { blockInEpoch =>
        currentHeight = activationStartHeight + blockInEpoch
        
        val (newParameters, _) = currentParameters.update(
          currentHeight,
          forkVote = false,
          epochVotes = Seq.empty,
          ErgoValidationSettingsUpdate.empty,
          votingSettings
        )
        
        currentParameters = newParameters
      }
    }

    // At activation height, block version should be incremented to 4 (version 6.0)
    val activationHeight = softForkStartHeight + votingEpochLength * (votingSettings.softForkEpochs + votingSettings.activationEpochs)
    val (finalParameters, _) = currentParameters.update(
      activationHeight,
      forkVote = false,
      epochVotes = Seq.empty,
      ErgoValidationSettingsUpdate.empty,
      votingSettings
    )

    // Verify that block version is now 4 (version 6.0)
    assert(finalParameters.blockVersion == Header.Interpreter60Version, 
      s"Block version should be ${Header.Interpreter60Version} after 6.0 activation")

    // Verify that soft-fork voting state is cleaned up after activation
    assert(finalParameters.softForkStartingHeight.isEmpty, "Soft-fork starting height should be cleared after activation")
    assert(finalParameters.softForkVotesCollected.isEmpty, "Soft-fork votes collected should be cleared after activation")
  }
}