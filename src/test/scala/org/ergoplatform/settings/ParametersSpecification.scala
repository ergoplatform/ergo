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

  private implicit def toExtension(p: Parameters): Extension = p.toExtensionCandidate(Seq.empty).toExtension(headerId)

  property("extension processing") {
    val constants = stateConstants.copy(
      settings = settings.copy(
        chainSettings = settings.chainSettings.copy(voting = votingSettings)
      )
    )
    val ctx = ErgoStateContext.empty(constants)
    val chain = genChain(votingEpochLength * 4).map { b =>
      b.copy(extension = b.extension.copy(fields = LaunchParameters.toExtensionCandidate(Seq.empty).fields))
    }
    val validChain = chain.init
    val lastBlock = chain.last
    val invalidExtBlock1 = { // extension does not contain all required params
      lastBlock.copy(extension = lastBlock.extension.copy(
        fields = Seq(Array(0: Byte, 1: Byte) -> Array.fill(4)(2: Byte)))
      )
    }
    val invalidExtBlock2 = { // extension contains redundant parameter
      lastBlock.copy(extension = lastBlock.extension.copy(
        fields = LaunchParameters.toExtensionCandidate(Seq.empty).fields :+ Array(0: Byte, 99: Byte) -> Array.fill(4)(2: Byte))
      )
    }
    val invalidExtBlock3 = { // extension does not contain params at all
      lastBlock.copy(extension = lastBlock.extension.copy(fields = Seq()))
    }
    val validCtx = validChain.foldLeft(ctx)((acc, mod) => acc.appendFullBlock(mod, votingSettings).get)
    validCtx.appendFullBlock(invalidExtBlock1, votingSettings) shouldBe 'failure
    validCtx.appendFullBlock(invalidExtBlock2, votingSettings) shouldBe 'failure
    validCtx.appendFullBlock(invalidExtBlock3, votingSettings) shouldBe 'failure
    validCtx.appendFullBlock(lastBlock, votingSettings) shouldBe 'success
  }

  //Simple checks for votes in header could be found also in NonVerifyADHistorySpecification("Header votes")
  property("simple voting - start - conditions") {
    val kInit = 1000000

    val p: Parameters = Parameters(2, Map(StorageFeeFactorIncrease -> kInit, BlockVersion -> 0))
    val vr: VotingData = VotingData.empty
    val esc = new ErgoStateContext(Seq(), ADDigest @@ Array.fill(33)(0: Byte), p, vr)
    val votes = Array(StorageFeeFactorIncrease, NoParameter, NoParameter)
    val h = defaultHeaderGen.sample.get.copy(height = 2, votes = votes, version = 0: Byte)
    val esc2 = esc.process(h, p).get

    //no quorum gathered - no parameter change
    val he = defaultHeaderGen.sample.get.copy(votes = Array.fill(3)(NoParameter), version = 0: Byte)
    val esc30 = esc2.process(he, p).get
    val esc40 = esc30.process(he, p).get
    esc40.currentParameters.storageFeeFactor shouldBe kInit

    //quorum gathered - parameter change
    val esc31 = esc2.process(h.copy(height = 3), p).get
    esc31.votingData.epochVotes.find(_._1 == StorageFeeFactorIncrease).get._2 shouldBe 2

    val p4 = Parameters(4, Map(StorageFeeFactorIncrease -> (kInit + Parameters.StorageFeeFactorStep), BlockVersion -> 0))
    val esc41 = esc31.process(he.copy(height = 4), p4).get
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
    val esc2 = esc1.process(h2, expectedParameters2).get
    esc2.currentParameters.softForkStartingHeight.get shouldBe 2

    val h3 = h2.copy(height = 3)
    val esc3 = esc2.process(h3, expectedParameters2).get
    esc3.currentParameters.softForkStartingHeight.get shouldBe 2

    val h4 = h3.copy(height = 4)
    val expectedParameters4 = Parameters(4, Map(SoftForkStartingHeight -> 2, SoftForkVotesCollected -> 2, BlockVersion -> 0))
    val esc4 = esc3.process(h4, expectedParameters4).get
    esc4.currentParameters.softForkStartingHeight.get shouldBe 2
    esc4.currentParameters.softForkVotesCollected.get shouldBe 2

    val h5 = h4.copy(height = 5)
    val esc5 = esc4.process(h5, expectedParameters4).get

    val h6 = h5.copy(height = 6, votes = emptyVotes)
    val expectedParameters6 = Parameters(6, Map(SoftForkStartingHeight -> 2, SoftForkVotesCollected -> 4, BlockVersion -> 0))
    val esc6 = esc5.process(h6, expectedParameters6).get

    val h6w = h5.copy(height = 6)
    esc5.process(h6w, expectedParameters6).isSuccess shouldBe false

    val esc11 = (7 to 11).foldLeft(esc6) { case (esc, i) =>
      val hw = h6.copy(height = i, votes = forkVote)
      esc.process(hw, expectedParameters6).isFailure shouldBe true

      val h = h6.copy(height = i)
      esc.process(h, expectedParameters6).get
    }

    val h12 = h6.copy(height = 12, version = 1: Byte)
    val expectedParameters12 = Parameters(12, Map(SoftForkStartingHeight -> 2, SoftForkVotesCollected -> 4, BlockVersion -> 1))
    val esc12 = esc11.process(h12, expectedParameters12).get

    val h12w = h12.copy(votes = forkVote)
    esc11.process(h12w, expectedParameters12).isFailure shouldBe true

    val h13 = h12.copy(height = 13)
    val esc13 = esc12.process(h13, expectedParameters12).get

    val h13w = h13.copy(votes = forkVote)
    esc12.process(h13w, expectedParameters12).isFailure shouldBe true


    val h14 = h13.copy(height = 14, votes = forkVote)
    val expectedParameters14 = Parameters(14, Map(SoftForkStartingHeight -> 14, SoftForkVotesCollected -> 0, BlockVersion -> 1))
    val esc14 = esc13.process(h14, expectedParameters14).get

    val h14e = h13.copy(height = 14, votes = emptyVotes)
    val expectedParameters14e = Parameters(14, Map(BlockVersion -> 1))
    val esc14e = esc13.process(h14e, expectedParameters14e).get
  }

  property("soft fork - unsuccessful voting") {
    val p: Parameters = Parameters(1, Map(BlockVersion -> 0))
    val vr: VotingData = VotingData.empty
    val esc1 = new ErgoStateContext(Seq(), ADDigest @@ Array.fill(33)(0: Byte), p, vr)
    val forkVote = Array(SoftFork, NoParameter, NoParameter)
    val emptyVotes = Array(NoParameter, NoParameter, NoParameter)
    val h2 = defaultHeaderGen.sample.get.copy(votes = forkVote, version = 0: Byte, height = 2)

    val expectedParameters2 = Parameters(2, Map(SoftForkStartingHeight -> 2, SoftForkVotesCollected -> 0, BlockVersion -> 0))
    val esc2 = esc1.process(h2, expectedParameters2).get
    esc2.currentParameters.softForkStartingHeight.get shouldBe 2
    val h3 = h2.copy(height = 3)
    val esc3 = esc2.process(h3, expectedParameters2).get
    esc3.currentParameters.softForkStartingHeight.get shouldBe 2
    val h4 = h3.copy(height = 4)
    val expectedParameters4 = Parameters(4, Map(SoftForkStartingHeight -> 2, SoftForkVotesCollected -> 2, BlockVersion -> 0))
    val esc4 = esc3.process(h4, expectedParameters4).get
    esc4.currentParameters.softForkStartingHeight.get shouldBe 2
    esc4.currentParameters.softForkVotesCollected.get shouldBe 2
    val h5 = h4.copy(height = 5, votes = emptyVotes)
    val esc5 = esc4.process(h5, expectedParameters4).get

    val h6 = h5.copy(height = 6)
    val expectedParameters6 = Parameters(6, Map(SoftForkStartingHeight -> 2, SoftForkVotesCollected -> 3, BlockVersion -> 0))
    val esc6 = esc5.process(h6, expectedParameters6).get

    val h6w = h5.copy(height = 6, votes = forkVote)
    esc5.process(h6w, expectedParameters6).isFailure shouldBe true

    val h7 = h6.copy(height = 7)
    val esc7 = esc6.process(h7, expectedParameters6).get

    val h7w = h6.copy(height = 7, votes = forkVote)
    esc6.process(h7w, expectedParameters6).isFailure shouldBe true

    val h8 = h7.copy(height = 8, votes = forkVote)
    val expectedParameters8 = Parameters(8, Map(SoftForkStartingHeight -> 8, SoftForkVotesCollected -> 0, BlockVersion -> 0))
    val esc8 = esc7.process(h8, expectedParameters8).get

    val h8e = h7.copy(height = 8, votes = emptyVotes)
    val expectedParameters8e = Parameters(8, Map(BlockVersion -> 0))
    val esc8e = esc7.process(h8e, expectedParameters8e).get
  }

}
