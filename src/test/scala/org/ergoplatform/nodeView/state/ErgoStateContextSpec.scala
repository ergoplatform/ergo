package org.ergoplatform.nodeView.state

import org.ergoplatform.modifiers.ErgoFullBlock
import org.ergoplatform.settings.LaunchParameters
import org.ergoplatform.utils.{ErgoPropertyTest, HistoryTestHelpers}
import scorex.crypto.authds.ADDigest

class ErgoStateContextSpec extends ErgoPropertyTest with HistoryTestHelpers {

  property("Header votes") {
    import org.ergoplatform.settings.Parameters._
    val vr: VotingData = VotingData.empty
    val esc = new ErgoStateContext(Seq(), None, ADDigest @@ Array.fill(33)(0: Byte), LaunchParameters, validationSettingsNoIl, vr)

    val fb = genChain(1).head
    val header = fb.header

    def fbWithVotes(votes: Array[Byte], h: Int = 1): ErgoFullBlock = {
      val newHeader = header.copy(votes = votes, version = 0: Byte, height = h)
      fb.copy(header = newHeader)
    }


    //double vote
    val wrongVotes1 = Array(StorageFeeFactorIncrease, StorageFeeFactorIncrease, NoParameter)
    esc.appendFullBlock(fbWithVotes(wrongVotes1), votingSettings) shouldBe 'failure


    //contradictory votes
    val wrongVotes2 = Array(StorageFeeFactorIncrease, StorageFeeFactorDecrease, NoParameter)
    esc.appendFullBlock(fbWithVotes(wrongVotes2), votingSettings) shouldBe 'failure

    //too many votes - only two ordinary changes allowed per epoch
    val wrongVotes3 = Array(StorageFeeFactorIncrease, MaxBlockCostIncrease, MaxBlockSizeDecrease)
    esc.appendFullBlock(fbWithVotes(wrongVotes3), votingSettings) shouldBe 'failure

    //a vote proposed on non-existing parameter
    val wrongVotes4 = Array((-50).toByte, NoParameter, MaxBlockSizeDecrease)
    esc.appendFullBlock(fbWithVotes(wrongVotes4, 2), votingSettings) shouldBe 'failure

    //correct votes
    val correctVotes = Array(StorageFeeFactorIncrease, MaxBlockSizeDecrease, NoParameter)
    esc.appendFullBlock(fbWithVotes(correctVotes), votingSettings) shouldBe 'success
  }


}
