package org.ergoplatform.settings

import org.ergoplatform.modifiers.history.Extension
import org.ergoplatform.nodeView.state.{ErgoStateContext, VotingData}
import org.ergoplatform.utils.ErgoPropertyTest
import scorex.crypto.authds.ADDigest

import scala.language.implicitConversions

class ParametersSpecification extends ErgoPropertyTest {

  import Parameters._

  private val headerId = scorex.util.bytesToId(Array.fill(32)(0: Byte))

  private val votingEpochLength = 2

  override implicit val votingSettings: VotingSettings =
    VotingSettings(votingEpochLength, softForkEpochs = 2, activationEpochs = 3)

  private implicit def toExtension(p: Parameters): Extension = p.toExtensionCandidate().toExtension(headerId)

  property("simple voting - start - conditions") {
    val kInit = 1000000

    val p: Parameters = Parameters(2, Map(KIncrease -> kInit, BlockVersion -> 0))
    val vr: VotingData = VotingData.empty
    val esc = new ErgoStateContext(Seq(), ADDigest @@ Array.fill(33)(0: Byte), p, vr)
    val votes = Array(KIncrease, NoParameter, NoParameter)
    val h = defaultHeaderGen.sample.get.copy(votes = votes, version = 0: Byte)
    val esc2 = esc.processExtension(p, h).get

    //double vote
    val wrongVotes1 = Array(KIncrease, KIncrease, NoParameter)
    val hw1 = defaultHeaderGen.sample.get.copy(votes = wrongVotes1, version = 0: Byte)
    esc.processExtension(p, hw1).isSuccess shouldBe false

    //contradictory votes
    val wrongVotes2 = Array(KIncrease, KDecrease, NoParameter)
    val hw2 = defaultHeaderGen.sample.get.copy(votes = wrongVotes2, version = 0: Byte)
    esc.processExtension(p, hw2).isSuccess shouldBe false

    //too many votes - only two ordinary changes allowed per epoch
    val wrongVotes3 = Array(KIncrease, MaxBlockCostIncrease, MaxBlockSizeDecrease)
    val hw3 = defaultHeaderGen.sample.get.copy(votes = wrongVotes3, version = 0: Byte)
    esc.processExtension(p, hw3).isSuccess shouldBe false

    //too many votes - a vote proposed on non-existing parameter
    val wrongVotes4 = Array((-50).toByte, NoParameter, MaxBlockSizeDecrease)
    val hw4 = defaultHeaderGen.sample.get.copy(votes = wrongVotes4, version = 0: Byte)
    esc.processExtension(p, hw4).isSuccess shouldBe false

    //no quorum gathered - no parameter change
    val he = defaultHeaderGen.sample.get.copy(votes = Array.fill(3)(NoParameter), version = 0: Byte)
    val esc30 = esc2.processExtension(p, he).get
    val esc40 = esc30.processExtension(p, he).get
    esc40.currentParameters.k shouldBe kInit

    //quorum gathered - parameter change
    val esc31 = esc2.processExtension(p, h.copy(height = 3)).get
    val p4 = Parameters(4, Map(KIncrease -> (kInit + Parameters.Kstep), BlockVersion -> 0))

    esc31.currentVoting.results.filter(_._1 == KIncrease).head._2 shouldBe 2

    val esc41 = esc31.processExtension(p4, he).get
    esc41.currentParameters.k shouldBe (kInit + Parameters.Kstep)
  }

  property("soft fork") {
    val p: Parameters = Parameters(2, Map(BlockVersion -> 0))
    val vr: VotingData = VotingData.empty
    val esc = new ErgoStateContext(Seq(), ADDigest @@ Array.fill(33)(0: Byte), p, vr)
    val votes = Array(SoftFork, NoParameter, NoParameter)
    val h = defaultHeaderGen.sample.get.copy(votes = votes, version = 0: Byte)
    val esc2 = esc.processExtension(p, h).get

  }

}
