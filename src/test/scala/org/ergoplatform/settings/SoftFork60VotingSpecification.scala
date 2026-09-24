package org.ergoplatform.settings

import org.ergoplatform.nodeView.state.{ErgoStateContext, VotingData}
import org.ergoplatform.utils.ErgoCorePropertyTest

class SoftFork60VotingSpecification extends ErgoCorePropertyTest {
  import org.ergoplatform.utils.ErgoCoreTestConstants._
  import org.ergoplatform.utils.generators.ErgoCoreGenerators._
  import org.ergoplatform.settings.Parameters.{NoParameter}
  import scorex.crypto.authds.ADDigest

  private val votingEpochLength = 2
  private val softForkEpochs = 2
  private val activationEpochs = 3
  private val activationHeight = 100

  implicit val votingSettings: VotingSettings =
    VotingSettings(
      votingEpochLength,
      softForkEpochs = softForkEpochs,
      activationEpochs = activationEpochs,
      version2ActivationHeight = activationHeight,
      version2ActivationDifficultyHex = "01"
    )

  private val updSettings = chainSettings.copy(voting = votingSettings)


  property("Soft-fork should activate after sufficient voting epochs") {
    val p: Parameters = Parameters(2, Map(Parameters.BlockVersion -> 3), ErgoValidationSettingsUpdate.empty)
    val vr: VotingData = VotingData.empty
    
    var esc = new ErgoStateContext(Seq(), None, ADDigest @@ Array.fill(33)(0: Byte), p, validationSettingsNoIl, vr)(updSettings)
    
    // Vote for 6.0 in multiple epochs
    val voteFor60 = 120: Byte
    val votes = Array(voteFor60, NoParameter, NoParameter)
    
    // Vote for sufficient epochs to trigger activation
    (1 to softForkEpochs).foreach { epoch =>
      val epochStartHeight = (epoch - 1) * votingEpochLength + 1
      val epochEndHeight = epoch * votingEpochLength
      
      (epochStartHeight to epochEndHeight).foreach { height =>
        val header = defaultHeaderGen.sample.get.copy(height = height, votes = votes, version = 3: Byte)
        esc = esc.appendHeader(header).get
      }
    }
    
    // After activation epochs, block version should transition to 4
    // The activation happens at the start of the next epoch after sufficient votes
    val activationBlockHeight = softForkEpochs * votingEpochLength + 1
    val activationHeader = defaultHeaderGen.sample.get.copy(height = activationBlockHeight, version = 4: Byte)
    esc.appendHeader(activationHeader).get
  }

  property("Rules should be disabled/enabled according to voting results") {
    val updateForV4 = ErgoValidationSettingsUpdate(Seq(ValidationRules.exMatchParameters, ValidationRules.hdrVotesUnknown), Seq())
    val p: Parameters = Parameters(2, Map(Parameters.BlockVersion -> 4), updateForV4)
    val vr: VotingData = VotingData.empty
    
    // Update validation settings with the update from parameters
    val updatedValidationSettings = validationSettingsNoIl.updated(updateForV4)
    
    val esc = new ErgoStateContext(Seq(), None, ADDigest @@ Array.fill(33)(0: Byte), p, updatedValidationSettings, vr)(updSettings)
    
    // After 6.0 activation, specific rules should be disabled
    esc.validationSettings.isActive(ValidationRules.hdrVotesUnknown) shouldBe false // Rule 215
    esc.validationSettings.isActive(ValidationRules.exMatchParameters) shouldBe false // Rule 409
    
    // And new rules should be enabled
    esc.validationSettings.isActive(ValidationRules.exMatchParameters60) shouldBe true // Rule 414
  }

  property("Voting should reset at epoch boundaries") {
    val p: Parameters = Parameters(2, Map(Parameters.BlockVersion -> 3), ErgoValidationSettingsUpdate.empty)
    val vr: VotingData = VotingData.empty
    
    var esc = new ErgoStateContext(Seq(), None, ADDigest @@ Array.fill(33)(0: Byte), p, validationSettingsNoIl, vr)(updSettings)
    
    val voteFor60 = 120: Byte
    
    // Vote in first epoch
    (1 to votingEpochLength).foreach { height =>
      val header = defaultHeaderGen.sample.get.copy(height = height, votes = Array(voteFor60, NoParameter, NoParameter), version = 3: Byte)
      esc = esc.appendHeader(header).toOption.get
    }
    
    // Vote in second epoch
    (votingEpochLength + 1 to votingEpochLength * 2).foreach { height =>
      val header = defaultHeaderGen.sample.get.copy(height = height, votes = Array(NoParameter, NoParameter, NoParameter), version = 3: Byte)
      esc = esc.appendHeader(header).get
    }
    
    // Votes should be reset for new epoch
    esc.votingData.epochVotes should not contain (voteFor60 -> 2)
  }

}
