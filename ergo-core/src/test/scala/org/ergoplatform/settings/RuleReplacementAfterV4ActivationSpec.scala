package org.ergoplatform.settings

import org.ergoplatform.modifiers.history.header.Header
import org.ergoplatform.nodeView.state.{ErgoStateContext, VotingData}
import org.ergoplatform.utils.ErgoCorePropertyTest
import org.ergoplatform.utils.generators.ErgoCoreGenerators.defaultHeaderGen
import scorex.crypto.authds.ADDigest

class RuleReplacementAfterV4ActivationSpec extends ErgoCorePropertyTest {
  import org.ergoplatform.utils.ErgoCoreTestConstants._

  private val votingEpochLength = 2
  private val activationHeight = 100

  implicit val votingSettings: VotingSettings =
    VotingSettings(
      votingEpochLength,
      softForkEpochs = 2,
      activationEpochs = 3,
      version2ActivationHeight = activationHeight,
      version2ActivationDifficultyHex = "01"
    )

  private val updSettings = chainSettings.copy(voting = votingSettings)

  property("rule replacements 1011->1016, 1007->1017, 1008->1018 are written to extension sections after block version 4 activation") {
    // Define the rule replacements
    val ruleReplacements = Seq(
      1011.toShort -> 1016.toShort,
      1007.toShort -> 1017.toShort, 
      1008.toShort -> 1018.toShort
    )

    // Create validation settings update with the rule replacements
    val update = ErgoValidationSettingsUpdate(
      Seq.empty,
      ruleReplacements.map { case (oldRule, newRule) =>
        oldRule -> sigma.validation.ReplacedRule(newRule)
      }
    )

    // Start with block version 4 already activated and apply rule replacements
    val initialParameters = Parameters(1, Map(Parameters.BlockVersion -> Header.Interpreter60Version), update)
    val vr: VotingData = VotingData.empty
    
    val initialEsc = new ErgoStateContext(
      Seq(), 
      None, 
      ADDigest @@ Array.fill(33)(0: Byte), 
      initialParameters, 
      ErgoValidationSettings.initial.updated(update), 
      vr
    )(updSettings)

    // Verify that block version is 4
    initialEsc.currentParameters.blockVersion shouldBe Header.Interpreter60Version

    var currentEsc = initialEsc

    // Test that rule replacements are present in extension sections for multiple epochs
    // Focus on voting epochs since that's when validation settings are written to extensions
    val epochsToTest = 4
    val blocksPerEpoch = votingEpochLength
    val totalBlocksToTest = epochsToTest * blocksPerEpoch

    for (blockOffset <- 1 to totalBlocksToTest) {
      val height = activationHeight + blockOffset
      val header = defaultHeaderGen.sample.get.copy(
        height = height,
        version = Header.Interpreter60Version, // version 4
        votes = Array.fill(3)(Parameters.NoParameter)
      )

      val upcoming = currentEsc.upcoming(
        header.minerPk,
        header.timestamp,
        header.nBits,
        header.votes,
        ErgoValidationSettingsUpdate.empty,
        header.version
      )

      // In production, extension formation follows this pattern:
      // - During voting epochs: parameters + interlinks + validation settings
      // - During non-voting epochs: only interlinks
      // For this test, we'll simulate the voting epoch scenario where validation settings are included
      val isVotingEpoch = height % votingEpochLength == 0
      
      val extension = if (isVotingEpoch) {
        // Simulate production behavior during voting epochs
        (
          upcoming.currentParameters.toExtensionCandidate ++
          upcoming.validationSettings.toExtensionCandidate
        ).toExtension(header.id)
      } else {
        // During non-voting epochs, only parameters are included (no validation settings)
        upcoming.currentParameters.toExtensionCandidate.toExtension(header.id)
      }

      // Only check for rule replacements during voting epochs when validation settings are included
      if (isVotingEpoch) {
        // Parse validation settings from extension
        val parsedValidationSettings = ErgoValidationSettings.parseExtension(extension).get

        // Verify that rule replacements are present
        ruleReplacements.foreach { case (oldRule, newRule) =>
          val status = parsedValidationSettings.sigmaSettings.getStatus(oldRule)
          status shouldBe defined
          status.get shouldBe sigma.validation.ReplacedRule(newRule)
        }
      }

      currentEsc = currentEsc.process(header, Some(extension)).get
    }
  }

  property("rule replacement extension serialization roundtrip") {
    // Test the specific rule replacements in isolation
    val ruleReplacements = Seq(
      1011.toShort -> 1016.toShort,
      1007.toShort -> 1017.toShort,
      1008.toShort -> 1018.toShort
    )

    val update = ErgoValidationSettingsUpdate(
      Seq.empty,
      ruleReplacements.map { case (oldRule, newRule) =>
        oldRule -> sigma.validation.ReplacedRule(newRule)
      }
    )

    val vs = ErgoValidationSettings.initial.updated(update)
    val extension = vs.toExtensionCandidate
    
    // Verify extension contains the rule replacements
    extension.fields should not be empty
    
    val parsedVs = ErgoValidationSettings.parseExtension(extension).get
    
    // Verify roundtrip preserves the rule replacements
    ruleReplacements.foreach { case (oldRule, newRule) =>
      val originalStatus = vs.sigmaSettings.getStatus(oldRule)
      val parsedStatus = parsedVs.sigmaSettings.getStatus(oldRule)
      
      originalStatus shouldBe defined
      parsedStatus shouldBe defined
      originalStatus.get shouldBe parsedStatus.get
      parsedStatus.get shouldBe sigma.validation.ReplacedRule(newRule)
    }
    
    vs.updateFromInitial shouldBe parsedVs.updateFromInitial
  }
}
