package org.ergoplatform.nodeView.history

import org.ergoplatform.mining.AutolykosPowScheme
import org.ergoplatform.modifiers.{ErgoFullBlock, SnapshotsInfoTypeId}
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
    ErgoHistory.readOrGenerate(bootstrapSettings(genesisId, blocksToKeep, nipopowBootstrap, votingLength))(null)
      .ensuring(_.bestFullBlockOpt.isEmpty)

  private def bootstrapSettings(genesisId: ModifierId,
                                blocksToKeep: Int,
                                nipopowBootstrap: Boolean = true,
                                votingLength: Option[Int] = None,
                                stateType: StateType = StateType.Digest,
                                utxoBootstrap: Boolean = false) = {
    val voting = votingLength.fold(baseSettings.chainSettings.voting)(l => baseSettings.chainSettings.voting.copy(votingLength = l))
    baseSettings.copy(
      directory = createTempDir.getAbsolutePath,
      chainSettings = baseSettings.chainSettings.copy(epochLength = 10000, useLastEpochs = 3, genesisId = Some(genesisId), voting = voting),
      nodeSettings = baseSettings.nodeSettings.copy(
        stateType = stateType,
        verifyTransactions = true,
        blocksToKeep = blocksToKeep,
        utxoSettings = baseSettings.nodeSettings.utxoSettings.copy(utxoBootstrap = utxoBootstrap),
        nipopowSettings = NipopowSettings(nipopowBootstrap = nipopowBootstrap, p2pNipopows = 1)
      )
    )
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

    // once headers chain is marked synced, the floor is not inside the gap
    def checkFloorNotInGap(): Unit =
      if (history.isHeadersChainSynced) history.minimalFullBlockHeight should be >= continuousFrom
    checkFloorNotInGap()

    // headers after the proof are coming as usual
    applyHeaderChain(history, HeaderChain(senderChain.drop(proofTip).map(_.header)))
    history.headersHeight shouldBe senderChain.last.height
    checkFloorNotInGap()

    // full blocks are to be downloaded from a height with enough headers before it to construct state context
    history.isHeadersChainSynced shouldBe true
    val firstContextHeight = history.minimalFullBlockHeight - Constants.LastHeadersInContext + 1
    (firstContextHeight to history.headersHeight).filter(h => history.bestHeaderIdAtHeight(h).isEmpty) shouldBe empty

    // and full blocks downloading is going on up to the tip
    val toDownload = history.nextModifiersToDownload(1000, (_, id) => !history.contains(id)).values.flatten.toSeq
    toDownload should contain(senderChain.last.header.transactionsId)
  }


  // a sender chain of `length` fresh blocks and a proof whose suffix starts at height 60
  private def senderChainAndProof(length: Int = 110): (Seq[ErgoFullBlock], Array[Byte], ModifierId) = {
    val senderHistory = genHistory(None, popowBootstrap = false)
    val senderChain = genChain(length, senderHistory)
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
    senderChain.filter(_.header.height < expected).map(_.header.transactionsId).filter(toDownload.contains) shouldBe empty
    toDownload should contain(senderChain.last.header.transactionsId)
  }

  property("near the tip, no block section below a full block floor raised to an epoch start after a popow proof is scheduled") {
    val (senderChain, proofBytes, genesisId) = senderChainAndProof(length = 250)
    val votingLength = 20
    // blocksToKeep is large enough for the floor to stay where the proof put it while full blocks are applied
    val history = genPrunedDigestHistory(genesisId, blocksToKeep = 200, votingLength = Some(votingLength))
    history.applyPopowProof(history.nipopowSerializer.parseBytes(proofBytes))
    applyHeaderChain(history, HeaderChain(senderChain.drop(history.headersHeight).map(_.header)))
    val from = continuousFrom(history)
    val floor = history.minimalFullBlockHeight

    // full blocks from the floor up, so that 100 blocks below the best full block is below the floor, but not in the gap
    val bestFullBlockHeight = floor + 90
    applyChain(history, senderChain.slice(floor - 1, bestFullBlockHeight))
    val scanStart = bestFullBlockHeight - 100
    withClue(s"tip=${history.headersHeight} continuousFrom=$from floor=$floor bestFullBlock=$bestFullBlockHeight scanStart=$scanStart: ") {
      floor should be > from + Constants.LastHeadersInContext - 1 // raised to the next epoch start
      floor % votingLength shouldBe 0
      history.minimalFullBlockHeight shouldBe floor
      history.bestFullBlockOpt.map(_.height) shouldBe Some(bestFullBlockHeight)
      (history.headersHeight - bestFullBlockHeight) should be < 128 // near the tip
      // headers below the floor are contiguous from the scan start
      scanStart should be >= from
      scanStart should be < floor

      val heightOf = senderChain.flatMap(b => history.requiredModifiersForHeader(b.header).map { case (_, id) => id -> b.height }).toMap
      val toDownload = history.nextModifiersToDownload(1000, (_, id) => !history.contains(id)).values.flatten.toSeq
      // every scheduled id is a section of a sender chain block, so none drops out of the height check below
      toDownload.filterNot(heightOf.contains) shouldBe empty
      // sections below the floor would fail `bsTooOld`, and their sender would be penalized
      toDownload.map(heightOf).filter(_ < floor).sorted shouldBe empty
      val nextHeader = senderChain(bestFullBlockHeight).header
      toDownload should contain allElementsOf history.requiredModifiersForHeader(nextHeader).map { case (_, id) => id }
    }
  }

  property("after a restart, a full block floor stored inside a popow headers gap moves past the gap, if no full block was applied") {
    val (senderChain, proofBytes, genesisId) = senderChainAndProof()
    val settings = bootstrapSettings(genesisId, blocksToKeep = 60)
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

  property("after a restart, a node which applied a full block inside a popow headers gap is not repaired") {
    val (senderChain, proofBytes, genesisId) = senderChainAndProof()
    val settings = bootstrapSettings(genesisId, blocksToKeep = 20)
    val history = ErgoHistory.readOrGenerate(settings)(null)
    history.applyPopowProof(history.nipopowSerializer.parseBytes(proofBytes))
    // a database written before this change, as in a devnet run: the floor stored at height 1, and the genesis block
    // applied as the first full block, with no header at height 2
    history.writeMinimalFullBlockHeight(GenesisHeight)
    applyBlock(history, senderChain.head)
    history.bestFullBlockOpt.map(_.height) shouldBe Some(GenesisHeight)
    history.bestHeaderIdAtHeight(GenesisHeight + 1) shouldBe None
    history.closeStorage()

    val restarted = ErgoHistory.readOrGenerate(settings)(null)
    applyHeaderChain(restarted, HeaderChain(senderChain.drop(restarted.headersHeight).map(_.header)))
    val lowest = continuousFrom(restarted) + Constants.LastHeadersInContext - 1
    val floor = restarted.minimalFullBlockHeight
    // even with every block from the floor up delivered, none of them can follow the best full block
    applyChain(restarted, senderChain.drop(floor - 1))
    withClue(s"tip=${restarted.headersHeight} lowest=$lowest floor=$floor: ") {
      restarted.isHeadersChainSynced shouldBe true
      floor should be >= lowest
      restarted.bestFullBlockOpt.map(_.height) shouldBe Some(GenesisHeight)
    }
  }

  property("a popow proof applied by a UTXO set snapshot bootstrapping node marks headers chain synced only") {
    val (_, proofBytes, genesisId) = senderChainAndProof()
    val settings = bootstrapSettings(genesisId, blocksToKeep = -1, stateType = StateType.Utxo, utxoBootstrap = true)
    val history = ErgoHistory.readOrGenerate(settings)(null)
    history.applyPopowProof(history.nipopowSerializer.parseBytes(proofBytes))
    withClue(s"tip=${history.headersHeight} floor=${history.minimalFullBlockHeight}: ") {
      history.isHeadersChainSynced shouldBe true
      // no full block floor is written, so the snapshot is still to be applied, and it is asked for
      history.minimalFullBlockHeight shouldBe GenesisHeight
      history.isUtxoSnapshotApplied shouldBe false
      history.nextModifiersToDownload(1000, (_, id) => !history.contains(id)) shouldBe Map(SnapshotsInfoTypeId.value -> Seq.empty)
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
