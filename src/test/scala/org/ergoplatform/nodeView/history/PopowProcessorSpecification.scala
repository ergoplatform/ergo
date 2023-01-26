package org.ergoplatform.nodeView.history

import org.ergoplatform.modifiers.ErgoFullBlock
import org.ergoplatform.modifiers.history.popow.{PoPowHeader, PoPowParams}
import org.ergoplatform.nodeView.state.StateType
import org.ergoplatform.utils.HistoryTestHelpers
import scorex.util.ModifierId

class PopowProcessorSpecification extends HistoryTestHelpers {
  private val poPowParams = PoPowParams(30, 30)

  private def genHistory(genesisIdOpt: Option[ModifierId], popowBootstrap: Boolean) =
    generateHistory(verifyTransactions = true, StateType.Utxo, PoPoWBootstrap = popowBootstrap, blocksToKeep = -1,
                    epochLength = 10000, useLastEpochs = 3, initialDiffOpt = None, genesisIdOpt)
      .ensuring(_.bestFullBlockOpt.isEmpty)

  val toPoPoWChain = (c: Seq[ErgoFullBlock]) => c.map(b => PoPowHeader.fromBlock(b).get)

  property("popow proof application") {
    val senderHistory = genHistory(None, popowBootstrap = false)
    val senderChain = genChain(5000, senderHistory)
    applyChain(senderHistory, senderChain)

    val popowProofBytes = senderHistory.popowProofBytes().get
    val popowProof = senderHistory.nipopowSerializer.parseBytes(popowProofBytes)

    val receiverHistory = genHistory(senderHistory.bestHeaderAtHeight(1).map(_.id), popowBootstrap = true)
    receiverHistory.headersHeight shouldBe 0
    receiverHistory.applyPopowProof(popowProof)
    receiverHistory.headersHeight shouldBe senderHistory.headersHeight
    receiverHistory.bestHeaderOpt.get shouldBe senderHistory.bestHeaderOpt.get
  }

}
