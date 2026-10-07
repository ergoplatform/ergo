package org.ergoplatform.nodeView.history

import org.ergoplatform.mining.AutolykosPowScheme
import org.ergoplatform.modifiers.ErgoFullBlock
import org.ergoplatform.modifiers.history.popow.PoPowHeader
import org.ergoplatform.nodeView.state.StateType
import org.ergoplatform.settings.NipopowSettings
import org.ergoplatform.utils.ErgoCorePropertyTest
import org.ergoplatform.wallet.utils.FileUtils
import scorex.util.ModifierId

class PopowProcessorSpecification extends ErgoCorePropertyTest with FileUtils {
  import org.ergoplatform.utils.HistoryTestHelpers._
  import org.ergoplatform.utils.ErgoNodeTestConstants.{settings => baseSettings}
  import org.ergoplatform.utils.generators.ChainGenerator._

  private def genHistory(genesisIdOpt: Option[ModifierId], popowBootstrap: Boolean) =
    generateHistory(verifyTransactions = true, StateType.Utxo, PoPoWBootstrap = popowBootstrap, blocksToKeep = -1,
                    epochLength = 10000, useLastEpochs = 3, initialDiffOpt = None, genesisIdOpt)
      .ensuring(_.bestFullBlockOpt.isEmpty)

  private def genRealPowHistory(genesisIdOpt: Option[ModifierId],
                                realPowScheme: AutolykosPowScheme): ErgoHistory = {
    val realPowSettings = baseSettings.copy(
      directory = createTempDir.getAbsolutePath,
      chainSettings = baseSettings.chainSettings.copy(powScheme = realPowScheme, genesisId = genesisIdOpt),
      nodeSettings = baseSettings.nodeSettings.copy(
        stateType = StateType.Utxo,
        verifyTransactions = true,
        blocksToKeep = -1,
        nipopowSettings = NipopowSettings(nipopowBootstrap = true, p2pNipopows = 1)
      )
    )
    ErgoHistory.readOrGenerate(realPowSettings)(null).ensuring(_.bestFullBlockOpt.isEmpty)
  }

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

  property("popow proof application rejects headers failing real Autolykos validation") {
    val senderHistory = genHistory(None, popowBootstrap = false)
    val senderChain = genChain(80, senderHistory)
    applyChain(senderHistory, senderChain)

    val popowProofBytes = senderHistory.popowProofBytes().get
    val realPowScheme = new AutolykosPowScheme(baseSettings.chainSettings.powScheme.k, baseSettings.chainSettings.powScheme.n)
    val receiverHistory = genRealPowHistory(senderHistory.bestHeaderAtHeight(1).map(_.id), realPowScheme)
    val popowProof = receiverHistory.nipopowSerializer.parseBytes(popowProofBytes)

    popowProof.headersChain.exists(h => realPowScheme.validate(h).isFailure) shouldBe true

    receiverHistory.headersHeight shouldBe 0
    receiverHistory.applyPopowProof(popowProof)
    receiverHistory.headersHeight shouldBe 0
    receiverHistory.bestHeaderOpt shouldBe None
  }

}
