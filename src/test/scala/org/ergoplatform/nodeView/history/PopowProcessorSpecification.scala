package org.ergoplatform.nodeView.history

import org.ergoplatform.nodeView.state.StateType
import org.ergoplatform.utils.HistoryTestHelpers
import scorex.util.ModifierId

class PopowProcessorSpecification extends HistoryTestHelpers {
  private def genHistory(genesisIdOpt: Option[ModifierId]) =
    generateHistory(verifyTransactions = true, StateType.Utxo, PoPoWBootstrap = false, blocksToKeep = -1,
                    epochLength = 10000, useLastEpochs = 3, initialDiffOpt = None, genesisIdOpt)
      .ensuring(_.bestFullBlockOpt.isEmpty)


  property("popow proof application") {
    val senderHistory = genHistory(None)
    val senderChain = genChain(5000, senderHistory)
    applyChain(senderHistory, senderChain)

    val popowProofBytes = senderHistory.popowProofBytes().get
    println("proof size: " + popowProofBytes.length)
    val popowProof = senderHistory.popowProof().get // nipopowSerializer.parseBytes(popowProofBytes)

    println("valid: " + popowProof.isValid)
    println(popowProof.hasValidConnections)
    println(popowProof.hasValidHeights)
    println(popowProof.hasValidProofs)

    val receiverHistory = genHistory(senderHistory.bestHeaderAtHeight(1).map(_.id))
    receiverHistory.headersHeight shouldBe 0
    receiverHistory.applyPopowProof(popowProof)
    receiverHistory.headersHeight shouldBe senderHistory.headersHeight
  }

}
