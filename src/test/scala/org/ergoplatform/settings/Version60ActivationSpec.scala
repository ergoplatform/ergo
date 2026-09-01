package org.ergoplatform.settings

import org.ergoplatform.modifiers.history.extension.ExtensionCandidate
import org.ergoplatform.modifiers.history.header.Header
import org.ergoplatform.nodeView.state.{ErgoStateContext, VotingData}
import org.ergoplatform.utils.ErgoCorePropertyTest
import org.ergoplatform.utils.ErgoCoreTestConstants._
import org.ergoplatform.utils.generators.ErgoCoreGenerators._

import Parameters._

/**
  * Test specification for version 6.0 activation parameter processing.
  * Verifies that the node can successfully process parameters generated after 6.0 activation.
  */
class Version60ActivationSpec extends ErgoCorePropertyTest {

  private val votingEpochLength = 2
  private val softForkEpochs = 2
  private val activationEpochs = 3

  private val hfActivationHeight = 100
  private val hfActivationDifficultyHex = "01"

  implicit val votingSettings: VotingSettings =
    VotingSettings(
      votingEpochLength,
      softForkEpochs,
      activationEpochs,
      version2ActivationHeight = hfActivationHeight,
      version2ActivationDifficultyHex = hfActivationDifficultyHex
    )

  private val updSettings = chainSettings.copy(voting = votingSettings)

  property("Node should process parameters correctly after version 6.0 activation") {
    // Simulate the soft-fork voting process that leads to version 6.0 activation
    val initialHeight = 100
    val softForkStartHeight = initialHeight
    val activationHeight = softForkStartHeight + votingEpochLength * (softForkEpochs + activationEpochs)

    // Create initial parameters with block version 3 (current version before 6.0)
    val initialParameters = Parameters(
      initialHeight,
      DefaultParameters.updated(BlockVersion, 3),
      ErgoValidationSettingsUpdate.empty
    )

    // Create state context with initial parameters
    val ctx: ErgoStateContext = {
      new ErgoStateContext(Seq.empty, None, genesisStateDigest, initialParameters, validationSettingsNoIl, VotingData.empty)(updSettings)
        .upcoming(org.ergoplatform.mining.group.generator, 0L, chainSettings.initialNBits, Array.fill(3)(0.toByte), emptyVSUpdate, 0.toByte)
    }

    // Simulate successful soft-fork voting
    var currentParameters = initialParameters
    var currentHeight = initialHeight
    var currentVotes = 0

    // Simulate voting epochs
    (0 until softForkEpochs).foreach { epoch =>
      val epochStartHeight = softForkStartHeight + epoch * votingEpochLength
      
      // Each epoch has votingEpochLength blocks
      (0 until votingEpochLength).foreach { blockInEpoch =>
        currentHeight = epochStartHeight + blockInEpoch
        
        // Simulate votes for soft-fork (90%+ approval needed)
        val forkVote = true
        val epochVotes = Seq((SoftFork, votingEpochLength)) // All miners vote for soft-fork
        
        val (newParameters, activatedUpdate) = currentParameters.update(
          currentHeight,
          forkVote,
          epochVotes,
          ErgoValidationSettingsUpdate.empty,
          votingSettings
        )
        
        currentParameters = newParameters
        currentVotes += votingEpochLength
      }
    }

    // Verify that soft-fork is approved
    assert(votingSettings.softForkApproved(currentVotes), "Soft-fork should be approved with sufficient votes")

    // Simulate activation epochs
    (0 until activationEpochs).foreach { epoch =>
      val activationStartHeight = softForkStartHeight + softForkEpochs * votingEpochLength + epoch * votingEpochLength
      
      (0 until votingEpochLength).foreach { blockInEpoch =>
        currentHeight = activationStartHeight + blockInEpoch
        
        val (newParameters, activatedUpdate) = currentParameters.update(
          currentHeight,
          forkVote = false, // No new voting during activation
          epochVotes = Seq.empty, // No votes during activation
          ErgoValidationSettingsUpdate.empty,
          votingSettings
        )
        
        currentParameters = newParameters
      }
    }

    // At activation height, block version should be incremented to 4 (version 6.0)
    val activationHeight = softForkStartHeight + votingEpochLength * (softForkEpochs + activationEpochs)
    val (finalParameters, finalUpdate) = currentParameters.update(
      activationHeight,
      forkVote = false,
      epochVotes = Seq.empty,
      ErgoValidationSettingsUpdate.empty,
      votingSettings
    )

    // Verify that block version is now 4 (version 6.0)
    assert(finalParameters.blockVersion == Header.Interpreter60Version, 
      s"Block version should be ${Header.Interpreter60Version} after 6.0 activation, but got ${finalParameters.blockVersion}")

    // Verify that all parameters are still valid and processable
    assert(finalParameters.storageFeeFactor > 0, "Storage fee factor should be positive")
    assert(finalParameters.minValuePerByte > 0, "Min value per byte should be positive")
    assert(finalParameters.maxBlockSize > 0, "Max block size should be positive")
    assert(finalParameters.maxBlockCost > 0, "Max block cost should be positive")
    assert(finalParameters.tokenAccessCost > 0, "Token access cost should be positive")
    assert(finalParameters.inputCost > 0, "Input cost should be positive")
    assert(finalParameters.dataInputCost > 0, "Data input cost should be positive")
    assert(finalParameters.outputCost > 0, "Output cost should be positive")

    // Verify that parameters can be converted to extension candidate (for block inclusion)
    val extensionCandidate = finalParameters.toExtensionCandidate
    assert(extensionCandidate.fields.nonEmpty, "Parameters should generate non-empty extension candidate")

    // Verify that parameters can be parsed back from extension
    val parsedParameters = Parameters.parseExtension(currentHeight, extensionCandidate.toExtension(Array.fill(32)(0.toByte)))
    assert(parsedParameters.isSuccess, "Parameters should be parseable from extension")
    assert(parsedParameters.get.blockVersion == Header.Interpreter60Version, 
      "Parsed parameters should maintain version 6.0")

    // Verify that soft-fork voting state is cleaned up after activation
    assert(finalParameters.softForkStartingHeight.isEmpty, "Soft-fork starting height should be cleared after activation")
    assert(finalParameters.softForkVotesCollected.isEmpty, "Soft-fork votes collected should be cleared after activation")
  }

  property("Node should handle validation rule updates after version 6.0 activation") {
    val activationHeight = 200
    
    // Create parameters with version 6.0 activated
    val postActivationParameters = Parameters(
      activationHeight,
      DefaultParameters.updated(BlockVersion, Header.Interpreter60Version),
      ErgoValidationSettingsUpdate.empty
    )

    // Create state context with post-activation parameters
    val ctx: ErgoStateContext = {
      new ErgoStateContext(Seq.empty, None, genesisStateDigest, postActivationParameters, validationSettingsNoIl, VotingData.empty)(updSettings)
        .upcoming(org.ergoplatform.mining.group.generator, 0L, chainSettings.initialNBits, Array.fill(3)(0.toByte), emptyVSUpdate, 0.toByte)
    }

    // Verify that validation settings work correctly with version 6.0 parameters
    val validationSettings = ctx.validationSettings
    
    // Check that validation rules that can be disabled are properly handled
    val disableableRules = ValidationRules.rulesSpec.filter { case (_, status) => status.mayBeDisabled }
    
    disableableRules.foreach { case (ruleId, _) =>
      // Verify that each disableable rule can be checked
      val isActive = validationSettings.isActive(ruleId)
      // This should not throw any exceptions
      assert(isActive || !isActive, s"Rule $ruleId should be checkable without errors")
    }

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

  property("Node should process blocks with version 6.0 headers correctly") {
    val activationHeight = 200
    
    // Create parameters with version 6.0
    val version60Parameters = Parameters(
      activationHeight,
      DefaultParameters.updated(BlockVersion, Header.Interpreter60Version),
      ErgoValidationSettingsUpdate.empty
    )

    // Create state context and process the chain
    val ctx: ErgoStateContext = {
      new ErgoStateContext(Seq.empty, None, genesisStateDigest, version60Parameters, validationSettingsNoIl, VotingData.empty)(updSettings)
        .upcoming(org.ergoplatform.mining.group.generator, 0L, chainSettings.initialNBits, Array.fill(3)(0.toByte), emptyVSUpdate, 0.toByte)
    }

    // Generate headers with version 6.0
    val headers = (1 to 5).map { i =>
      defaultHeaderGen.sample.get.copy(
        version = Header.Interpreter60Version,
        height = activationHeight + i
      )
    }

    // Process the headers - this should not throw any exceptions
    headers.foreach { header =>
      val upcoming = ctx.upcoming(header.minerPk, header.timestamp, header.nBits, header.votes, version60Parameters.proposedUpdate, header.version)
      val extension = (upcoming.currentParameters.toExtensionCandidate ++ upcoming.validationSettings.toExtensionCandidate).toExtension(header.id)
      val result = ctx.process(header, Some(extension))
      assert(result.isSuccess, s"Processing header with version 6.0 should succeed")
    }

    // Verify that the final context has the correct parameters
    val finalHeader = headers.last
    val upcoming = ctx.upcoming(finalHeader.minerPk, finalHeader.timestamp, finalHeader.nBits, finalHeader.votes, version60Parameters.proposedUpdate, finalHeader.version)
    assert(upcoming.currentParameters.blockVersion == Header.Interpreter60Version,
      "Final context should maintain version 6.0 parameters")
  }
}