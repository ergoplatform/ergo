package org.ergoplatform.settings

import org.ergoplatform.modifiers.history.Extension
import org.ergoplatform.nodeView.state.{ErgoStateContext, VotingData}
import org.ergoplatform.settings.Parameters._
import org.ergoplatform.utils.ErgoPropertyTest
import scorex.crypto.authds.ADDigest

class ErgoValidationSettingsSpec extends ErgoPropertyTest {

  private val headerId = scorex.util.bytesToId(Array.fill(32)(0: Byte))

  private val votingEpochLength = 2

  override implicit val votingSettings: VotingSettings =
    VotingSettings(votingEpochLength, softForkEpochs = 2, activationEpochs = 3)

  private def toExtension(s: ErgoValidationSettings): Extension = s.toExtensionCandidate().toExtension(headerId)
  private implicit def toExtension(p: Parameters): Extension = p.toExtensionCandidate(Seq.empty).toExtension(headerId)

  /**
    * A test which is ensuring that approved soft-fork activates properly.
    * For the test, we have:
    *   - epoch length is about 2 blocks
    *   - 2 epochs to vote
    *   - 3 epochs to activate the fork
    *
    * So the fork would be activated only if 4 votes out of 4 are for it.
    */
  property("soft fork - w. activation") {
    val s: ErgoValidationSettings = ErgoValidationSettings.empty
    val vr: VotingData = VotingData.empty
    val esc1 = new ErgoStateContext(Seq(), ADDigest @@ Array.fill(33)(0: Byte), parameters, s, vr)
    val forkVote = Array(SoftFork, NoParameter, NoParameter)
    val emptyVotes = Array(NoParameter, NoParameter, NoParameter)

    // Soft-fork vote is proposed @ height == 2 with deactivated rules Seq(ValidationRules.exEmpty, ValidationRules.bsBlockTransactionsSize)
    val h2 = defaultHeaderGen.sample.get.copy(votes = forkVote, version = 0: Byte, height = 2)
    val expectedParameters2 = Parameters(2, Map(SoftForkStartingHeight -> 2, SoftForkVotesCollected -> 0, BlockVersion -> 0))
    val proposedSoftForkFields = Seq(ValidationRules.exEmpty, ValidationRules.bsBlockTransactionsSize)

    val extension = expectedParameters2.toExtensionCandidate(Seq()).toExtension(headerId)
    val esc2 = esc1.process(h2, expectedParameters2).get
    esc2.currentParameters.softForkStartingHeight.get shouldBe 2

  }


}
