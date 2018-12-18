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

    val p: Parameters = Parameters(2, Map(StorageFeeFactorIncrease -> kInit, BlockVersion -> 0))
    val vr: VotingData = VotingData.empty
    val esc = new ErgoStateContext(Seq(), ADDigest @@ Array.fill(33)(0: Byte), p, vr)
    val votes = Array(StorageFeeFactorIncrease, NoParameter, NoParameter)
    val h = defaultHeaderGen.sample.get.copy(height = 2, votes = votes, version = 0: Byte)
    val esc2 = esc.processExtension(p, h).get

    //double vote
    val wrongVotes1 = Array(StorageFeeFactorIncrease, StorageFeeFactorIncrease, NoParameter)
    val hw1 = defaultHeaderGen.sample.get.copy(votes = wrongVotes1, version = 0: Byte)
    esc.processExtension(p, hw1).isSuccess shouldBe false

    //contradictory votes
    val wrongVotes2 = Array(StorageFeeFactorIncrease, StorageFeeFactorDecrease, NoParameter)
    val hw2 = defaultHeaderGen.sample.get.copy(votes = wrongVotes2, version = 0: Byte)
    esc.processExtension(p, hw2).isSuccess shouldBe false

    //too many votes - only two ordinary changes allowed per epoch
    val wrongVotes3 = Array(StorageFeeFactorIncrease, MaxBlockCostIncrease, MaxBlockSizeDecrease)
    val hw3 = defaultHeaderGen.sample.get.copy(votes = wrongVotes3, version = 0: Byte)
    esc.processExtension(p, hw3).isSuccess shouldBe false

    //a vote proposed on non-existing parameter
    val wrongVotes4 = Array((-50).toByte, NoParameter, MaxBlockSizeDecrease)
    val hw4 = defaultHeaderGen.sample.get.copy(votes = wrongVotes4, version = 0: Byte, height = 2)
    esc.processExtension(p, hw4).isSuccess shouldBe false

    //no quorum gathered - no parameter change
    val he = defaultHeaderGen.sample.get.copy(votes = Array.fill(3)(NoParameter), version = 0: Byte)
    val esc30 = esc2.processExtension(p, he).get
    val esc40 = esc30.processExtension(p, he).get
    esc40.currentParameters.storageFeeFactor shouldBe kInit

    //quorum gathered - parameter change
    val esc31 = esc2.processExtension(p, h.copy(height = 3)).get
    esc31.currentVoting.results.find(_._1 == StorageFeeFactorIncrease).get._2 shouldBe 2

    val p4 = Parameters(4, Map(StorageFeeFactorIncrease -> (kInit + Parameters.StorageFeeFactorStep), BlockVersion -> 0))
    val esc41 = esc31.processExtension(p4, he.copy(height = 4)).get
    esc41.currentParameters.storageFeeFactor shouldBe (kInit + Parameters.StorageFeeFactorStep)
  }

  property("soft fork - w. activation") {
    val p: Parameters = Parameters(1, Map(BlockVersion -> 0))
    val vr: VotingData = VotingData.empty
    val esc1 = new ErgoStateContext(Seq(), ADDigest @@ Array.fill(33)(0: Byte), p, vr)
    val forkVote = Array(SoftFork, NoParameter, NoParameter)
    val emptyVotes = Array(NoParameter, NoParameter, NoParameter)
    val h2 = defaultHeaderGen.sample.get.copy(votes = forkVote, version = 0: Byte, height = 2)

    val expectedParameters2 = Parameters(2, Map(SoftForkStartingHeight -> 2, SoftForkVotesCollected -> 0, BlockVersion -> 0))
    val esc2 = esc1.processExtension(expectedParameters2, h2).get
    esc2.currentParameters.softForkStartingHeight.get shouldBe 2

    val h3 = h2.copy(height = 3)
    val esc3 = esc2.processExtension(expectedParameters2, h3).get
    esc3.currentParameters.softForkStartingHeight.get shouldBe 2

    val h4 = h3.copy(height = 4)
    val expectedParameters4 = Parameters(4, Map(SoftForkStartingHeight -> 2, SoftForkVotesCollected -> 2, BlockVersion -> 0))
    val esc4 = esc3.processExtension(expectedParameters4, h4).get
    esc4.currentParameters.softForkStartingHeight.get shouldBe 2
    esc4.currentParameters.softForkVotesCollected.get shouldBe 2

    val h5 = h4.copy(height = 5)
    val esc5 = esc4.processExtension(expectedParameters4, h5).get

    val h6 = h5.copy(height = 6, votes = emptyVotes)
    val expectedParameters6 = Parameters(6, Map(SoftForkStartingHeight -> 2, SoftForkVotesCollected -> 4, BlockVersion -> 0))
    val esc6 = esc5.processExtension(expectedParameters6, h6).get

    val h6w = h5.copy(height = 6)
    esc5.processExtension(expectedParameters6, h6w).isSuccess shouldBe false

    val esc11 = (7 to 11).foldLeft(esc6) { case (esc, i) =>
      val hw = h6.copy(height = i, votes = forkVote)
      esc.processExtension(expectedParameters6, hw).isFailure shouldBe true

      val h = h6.copy(height = i)
      esc.processExtension(expectedParameters6, h).get
    }

    val h12 = h6.copy(height = 12, version = 1: Byte)
    val expectedParameters12 = Parameters(12, Map(SoftForkStartingHeight -> 2, SoftForkVotesCollected -> 4, BlockVersion -> 1))
    val esc12 = esc11.processExtension(expectedParameters12, h12).get

    val h12w = h12.copy(votes = forkVote)
    esc11.processExtension(expectedParameters12, h12w).isFailure shouldBe true

    val h13 = h12.copy(height = 13)
    val esc13 = esc12.processExtension(expectedParameters12, h13).get

    val h13w = h13.copy(votes = forkVote)
    esc12.processExtension(expectedParameters12, h13w).isFailure shouldBe true


    val h14 = h13.copy(height = 14, votes = forkVote)
    val expectedParameters14 = Parameters(14, Map(SoftForkStartingHeight -> 14, SoftForkVotesCollected -> 0, BlockVersion -> 1))
    val esc14 = esc13.processExtension(expectedParameters14, h14).get

    val h14e = h13.copy(height = 14, votes = emptyVotes)
    val expectedParameters14e = Parameters(14, Map(BlockVersion -> 1))
    val esc14e = esc13.processExtension(expectedParameters14e, h14e).get
  }

  property("soft fork - unsuccessful voting") {
    val p: Parameters = Parameters(1, Map(BlockVersion -> 0))
    val vr: VotingData = VotingData.empty
    val esc1 = new ErgoStateContext(Seq(), ADDigest @@ Array.fill(33)(0: Byte), p, vr)
    val forkVote = Array(SoftFork, NoParameter, NoParameter)
    val emptyVotes = Array(NoParameter, NoParameter, NoParameter)
    val h2 = defaultHeaderGen.sample.get.copy(votes = forkVote, version = 0: Byte, height = 2)

    val expectedParameters2 = Parameters(2, Map(SoftForkStartingHeight -> 2, SoftForkVotesCollected -> 0, BlockVersion -> 0))
    val esc2 = esc1.processExtension(expectedParameters2, h2).get
    esc2.currentParameters.softForkStartingHeight.get shouldBe 2
    val h3 = h2.copy(height = 3)
    val esc3 = esc2.processExtension(expectedParameters2, h3).get
    esc3.currentParameters.softForkStartingHeight.get shouldBe 2
    val h4 = h3.copy(height = 4)
    val expectedParameters4 = Parameters(4, Map(SoftForkStartingHeight -> 2, SoftForkVotesCollected -> 2, BlockVersion -> 0))
    val esc4 = esc3.processExtension(expectedParameters4, h4).get
    esc4.currentParameters.softForkStartingHeight.get shouldBe 2
    esc4.currentParameters.softForkVotesCollected.get shouldBe 2
    val h5 = h4.copy(height = 5, votes = emptyVotes)
    val esc5 = esc4.processExtension(expectedParameters4, h5).get

    val h6 = h5.copy(height = 6)
    val expectedParameters6 = Parameters(6, Map(SoftForkStartingHeight -> 2, SoftForkVotesCollected -> 3, BlockVersion -> 0))
    val esc6 = esc5.processExtension(expectedParameters6, h6).get

    val h6w = h5.copy(height = 6, votes = forkVote)
    esc5.processExtension(expectedParameters6, h6w).isFailure shouldBe true

    val h7 = h6.copy(height = 7)
    val esc7 = esc6.processExtension(expectedParameters6, h7).get

    val h7w = h6.copy(height = 7, votes = forkVote)
    esc6.processExtension(expectedParameters6, h7w).isFailure shouldBe true

    val h8 = h7.copy(height = 8, votes = forkVote)
    val expectedParameters8 = Parameters(8, Map(SoftForkStartingHeight -> 8, SoftForkVotesCollected -> 0, BlockVersion -> 0))
    val esc8 = esc7.processExtension(expectedParameters8, h8).get

    val h8e = h7.copy(height = 8, votes = emptyVotes)
    val expectedParameters8e = Parameters(8, Map(BlockVersion -> 0))
    val esc8e = esc7.processExtension(expectedParameters8e, h8e).get
  }

}
