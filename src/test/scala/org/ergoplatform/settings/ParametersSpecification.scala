package org.ergoplatform.settings

import org.ergoplatform.modifiers.history.Extension
import org.ergoplatform.nodeView.state.{ErgoStateContext, VotingData, VotingResults}
import org.ergoplatform.utils.ErgoPropertyTest
import scorex.crypto.authds.ADDigest

import scala.language.implicitConversions

class ParametersSpecification extends ErgoPropertyTest {
  import Parameters._

  private val headerId = scorex.util.bytesToId(Array.fill(32)(0: Byte))

  private val votingEpochLength = 2

  override implicit val votingSettings = VotingSettings(votingEpochLength, 2, 2)

  private implicit def toExtension(p: Parameters): Extension = p.toExtensionCandidate().toExtension(headerId)

  property("simple voting - start - conditions") {
    val kInit = 1000000

    val p: Parameters = Parameters(2, Map(KIncrease -> kInit))
    val vr: VotingData = VotingData.empty
    val esc = new ErgoStateContext(Seq(), ADDigest @@ Array.fill(33)(0: Byte), p, vr)
    val votes = Array(KIncrease, NoParameter, NoParameter)
    val esc2 = esc.processExtension(p, votes, 2, votingSettings).get

    //double vote
    val wrongVotes1 = Array(KIncrease, KIncrease, NoParameter)
    esc.processExtension(p, wrongVotes1, 2, votingSettings).isSuccess shouldBe false

    //contradictory votes
    val wrongVotes2 = Array(KIncrease, KDecrease, NoParameter)
    esc.processExtension(p, wrongVotes2, 2, votingSettings).isSuccess shouldBe false

    //too many votes - only two ordinary changes allowed per epoch
    val wrongVotes3 = Array(KIncrease, MaxBlockCostIncrease, MaxBlockSizeDecrease)
    esc.processExtension(p, wrongVotes3, 2, votingSettings).isSuccess shouldBe false

    //too many votes - a vote proposed on non-existing parameter
    val wrongVotes4 = Array((-50).toByte, NoParameter, MaxBlockSizeDecrease)
    esc.processExtension(p, wrongVotes4, 2, votingSettings).isSuccess shouldBe false

    //no quorum gathered - no parameter change
    val esc30 = esc2.processExtension(p, Array.fill(3)(NoParameter), 3, votingSettings).get
    val esc40 = esc30.processExtension(p, Array.fill(3)(NoParameter), 4, votingSettings).get
    esc40.currentParameters.k shouldBe kInit

    //quorum gathered - parameter change
    val esc31 = esc2.processExtension(p, votes, 3, votingSettings).get
    val p4 = Parameters(4, Map(KIncrease -> (kInit + Parameters.Kstep)))

    esc31.currentVoting.results.filter(_._1 == KIncrease).head._2 shouldBe 2

    val esc41 = esc31.processExtension(p4, Array.fill(3)(NoParameter), 4, votingSettings).get
    esc41.currentParameters.k shouldBe (kInit + Parameters.Kstep)
  }
}
