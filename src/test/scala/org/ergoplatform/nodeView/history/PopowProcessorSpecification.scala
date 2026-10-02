package org.ergoplatform.nodeView.history

import org.ergoplatform.mining.AutolykosPowScheme
import org.ergoplatform.modifiers.ErgoFullBlock
import org.ergoplatform.modifiers.history.HeaderChain
import org.ergoplatform.modifiers.history.popow.PoPowHeader
import org.ergoplatform.nodeView.history.ErgoHistoryUtils.GenesisHeight
import org.ergoplatform.nodeView.state.StateType
import org.ergoplatform.settings.{Constants, NipopowSettings}
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

  private def genPrunedDigestHistory(genesisId: ModifierId,
                                     blocksToKeep: Int,
                                     nipopowBootstrap: Boolean = true,
                                     votingLength: Option[Int] = None): ErgoHistory =
    ErgoHistory.readOrGenerate(prunedDigestSettings(genesisId, blocksToKeep, nipopowBootstrap, votingLength))(null)
      .ensuring(_.bestFullBlockOpt.isEmpty)

  private def prunedDigestSettings(genesisId: ModifierId,
                                   blocksToKeep: Int,
                                   nipopowBootstrap: Boolean = true,
                                   votingLength: Option[Int] = None) = {
    val voting = votingLength.fold(baseSettings.chainSettings.voting)(l => baseSettings.chainSettings.voting.copy(votingLength = l))
    val prunedSettings = baseSettings.copy(
      directory = createTempDir.getAbsolutePath,
      chainSettings = baseSettings.chainSettings.copy(epochLength = 10000, useLastEpochs = 3, genesisId = Some(genesisId), voting = voting),
      nodeSettings = baseSettings.nodeSettings.copy(
        stateType = StateType.Digest,
        verifyTransactions = true,
        blocksToKeep = blocksToKeep,
        nipopowSettings = NipopowSettings(nipopowBootstrap = nipopowBootstrap, p2pNipopows = 1)
      )
    )
    prunedSettings
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

  property("pruned digest node bootstrapped with popow proof does not download full blocks from a headers gap") {
    val senderHistory = genHistory(None, popowBootstrap = false)
    // all the blocks have fresh timestamps, so any header could be considered as the one headers chain is synced at
    val senderChain = genChain(110, senderHistory)
    applyChain(senderHistory, senderChain)

    val m = senderHistory.P2PNipopowProofM
    val k = senderHistory.P2PNipopowProofK
    val suffixHeadId = senderHistory.bestHeaderIdAtHeight(60).get
    val popowProofBytes = senderHistory.popowProofBytes(m, k, Some(suffixHeadId)).get

    val history = genPrunedDigestHistory(senderHistory.bestHeaderIdAtHeight(GenesisHeight).get, blocksToKeep = 20)
    history.applyPopowProof(history.nipopowSerializer.parseBytes(popowProofBytes))
    val proofTip = history.headersHeight
    proofTip shouldBe 60 + k - 1

    // headers chain is continuous from this height up to the proof tip only
    val continuousFrom = (proofTip to GenesisHeight by -1).takeWhile(h => history.bestHeaderIdAtHeight(h).nonEmpty).last
    continuousFrom should be > GenesisHeight

    def floorInGap: Boolean = history.isHeadersChainSynced && history.minimalFullBlockHeight < continuousFrom
    floorInGap shouldBe false

    // headers after the proof are coming as usual
    applyHeaderChain(history, HeaderChain(senderChain.drop(proofTip).map(_.header)))
    history.headersHeight shouldBe senderChain.last.height
    floorInGap shouldBe false

    // full blocks are to be downloaded from a height with enough headers before it to construct state context
    history.isHeadersChainSynced shouldBe true
    val firstContextHeight = history.minimalFullBlockHeight - Constants.LastHeadersInContext + 1
    (firstContextHeight to history.headersHeight).forall(h => history.bestHeaderIdAtHeight(h).nonEmpty) shouldBe true

    // and full blocks downloading is going on up to the tip
    val toDownload = history.nextModifiersToDownload(1000, (_, id) => !history.contains(id)).values.flatten.toSeq
    toDownload should contain(senderChain.last.header.transactionsId)
  }


  // a sender chain of 110 fresh blocks and a proof whose suffix starts at height 60
  private def senderChainAndProof(): (Seq[ErgoFullBlock], Array[Byte], ModifierId) = {
    val senderHistory = genHistory(None, popowBootstrap = false)
    val senderChain = genChain(110, senderHistory)
    applyChain(senderHistory, senderChain)
    val suffixHeadId = senderHistory.bestHeaderIdAtHeight(60).get
    val proofBytes = senderHistory.popowProofBytes(senderHistory.P2PNipopowProofM, senderHistory.P2PNipopowProofK, Some(suffixHeadId)).get
    (senderChain, proofBytes, senderHistory.bestHeaderIdAtHeight(GenesisHeight).get)
  }

  private def continuousFrom(history: ErgoHistory): Int =
    (history.headersHeight to GenesisHeight by -1).takeWhile(h => history.bestHeaderIdAtHeight(h).nonEmpty).last

  property("after a popow proof the full block floor is the first height with LastHeadersInContext connected headers") {
    val (_, proofBytes, genesisId) = senderChainAndProof()
    // blocksToKeep reaches below the proof's continuous suffix, so the floor is clamped, not tip - blocksToKeep + 1
    val history = genPrunedDigestHistory(genesisId, blocksToKeep = 60)
    history.applyPopowProof(history.nipopowSerializer.parseBytes(proofBytes))
    val from = continuousFrom(history)
    withClue(s"tip=${history.headersHeight} continuousFrom=$from floor=${history.minimalFullBlockHeight}: ") {
      (history.headersHeight - 60 + 1) should be < from
      history.isHeadersChainSynced shouldBe true
      history.minimalFullBlockHeight shouldBe from + Constants.LastHeadersInContext - 1
    }
  }

  property("a full block floor that would start mid-epoch after a popow proof moves to the next voting epoch start") {
    val (senderChain, proofBytes, genesisId) = senderChainAndProof()
    val votingLength = 20
    val history = genPrunedDigestHistory(genesisId, blocksToKeep = 60, votingLength = Some(votingLength))
    history.applyPopowProof(history.nipopowSerializer.parseBytes(proofBytes))
    val lowest = continuousFrom(history) + Constants.LastHeadersInContext - 1
    val expected = lowest - lowest % votingLength + votingLength
    withClue(s"tip=${history.headersHeight} lowest=$lowest floor=${history.minimalFullBlockHeight}: ") {
      lowest should be > votingLength
      lowest % votingLength should not be 0
      history.minimalFullBlockHeight shouldBe expected
    }
    // headers after the proof are coming as usual; nothing below the floor is ever scheduled, the tip is
    applyHeaderChain(history, HeaderChain(senderChain.drop(history.headersHeight).map(_.header)))
    history.minimalFullBlockHeight shouldBe expected
    val toDownload = history.nextModifiersToDownload(1000, (_, id) => !history.contains(id)).values.flatten.toSet
    senderChain.filter(_.header.height < expected).map(_.header.transactionsId).exists(toDownload.contains) shouldBe false
    toDownload should contain(senderChain.last.header.transactionsId)
  }

  property("after a restart, a full block floor stored inside a popow headers gap moves past the gap") {
    val (senderChain, proofBytes, genesisId) = senderChainAndProof()
    val settings = prunedDigestSettings(genesisId, blocksToKeep = 60)
    val history = ErgoHistory.readOrGenerate(settings)(null)
    history.applyPopowProof(history.nipopowSerializer.parseBytes(proofBytes))
    val lowest = continuousFrom(history) + Constants.LastHeadersInContext - 1
    // a database written before this change: the floor stored at height 1, inside the gap
    history.writeMinimalFullBlockHeight(GenesisHeight)
    history.closeStorage()

    val restarted = ErgoHistory.readOrGenerate(settings)(null)
    restarted.isHeadersChainSynced shouldBe false
    restarted.minimalFullBlockHeight shouldBe GenesisHeight
    // headers after the proof are coming as usual, so the node marks the headers chain synced again
    applyHeaderChain(restarted, HeaderChain(senderChain.drop(restarted.headersHeight).map(_.header)))
    withClue(s"tip=${restarted.headersHeight} lowest=$lowest floor=${restarted.minimalFullBlockHeight}: ") {
      restarted.isHeadersChainSynced shouldBe true
      restarted.minimalFullBlockHeight should be >= lowest
    }
  }

  property("a continuous headers chain keeps the full block floor it had without nipopowBootstrap") {
    val (senderChain, _, genesisId) = senderChainAndProof()
    val headers = HeaderChain(senderChain.map(_.header))
    val withNipopow = genPrunedDigestHistory(genesisId, blocksToKeep = 20)
    val without = genPrunedDigestHistory(genesisId, blocksToKeep = 20, nipopowBootstrap = false)
    applyHeaderChain(withNipopow, headers)
    applyHeaderChain(without, headers)
    withClue(s"with: synced=${withNipopow.isHeadersChainSynced} floor=${withNipopow.minimalFullBlockHeight} h=${withNipopow.headersHeight}; " +
      s"without: synced=${without.isHeadersChainSynced} floor=${without.minimalFullBlockHeight} h=${without.headersHeight}: ") {
      withNipopow.isHeadersChainSynced shouldBe without.isHeadersChainSynced
      withNipopow.minimalFullBlockHeight shouldBe without.minimalFullBlockHeight
    }
  }
}
