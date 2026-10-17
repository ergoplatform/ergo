package org.ergoplatform.nodeView.history

import org.ergoplatform.mining.AutolykosPowScheme
import org.ergoplatform.modifiers.{BlockSection, ErgoFullBlock}
import org.ergoplatform.modifiers.history.popow.PoPowHeader
import org.ergoplatform.nodeView.state.StateType
import org.ergoplatform.settings.NipopowSettings
import org.ergoplatform.utils.{ErgoCorePropertyTest, ErgoNodeTestConstants}
import org.ergoplatform.wallet.utils.FileUtils
import scorex.util.ModifierId

import java.nio.charset.StandardCharsets

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

  private val legacySentinel =
    "legacy-nipopow-proof-must-not-be-served".getBytes(StandardCharsets.UTF_8)

  property("legacy NiPoPoW cache bytes are ignored") {
    val history = genHistory(None, popowBootstrap = false)
    try {
      history.LegacyNipopowSnapshotKey.data.toSeq shouldBe Seq.fill(32)(50.toByte)
      history.NipopowProofV2Key.data.toSeq shouldBe Seq.fill(32)(51.toByte)
      history.LegacyNipopowSnapshotKey should not equal history.NipopowProofV2Key

      history.historyStorage.insert(
        Array(history.LegacyNipopowSnapshotKey -> legacySentinel),
        BlockSection.emptyArray
      ).get

      history.readPopowProofBytesFromDb() shouldBe None
      history.historyStorage
        .getIndex(history.LegacyNipopowSnapshotKey)
        .get
        .toSeq shouldBe legacySentinel.toSeq
    } finally {
      history.closeStorage()
    }
  }

  property("V2 NiPoPoW cache miss remains empty until a scheduled snapshot") {
    val history = genHistory(None, popowBootstrap = false)
    try {
      history.readPopowProofBytesFromDb() shouldBe None
      history.readPopowProofBytesFromDb() shouldBe None
    } finally {
      history.closeStorage()
    }
  }

  property("V2 NiPoPoW cache hit reuses byte-identical proof without generation") {
    val history = genHistory(None, popowBootstrap = false)
    val cachedBytes = Array[Byte](2, 4, 6, 8)
    try {
      history.historyStorage.insert(
        Array(history.NipopowProofV2Key -> cachedBytes),
        BlockSection.emptyArray
      ).get

      history.readPopowProofBytesFromDb().get.toSeq shouldBe cachedBytes.toSeq
      history.readPopowProofBytesFromDb().get.toSeq shouldBe cachedBytes.toSeq
    } finally {
      history.closeStorage()
    }
  }

  property("V2 NiPoPoW cache miss never falls back to legacy bytes") {
    val history = genHistory(None, popowBootstrap = false)
    try {
      history.historyStorage.insert(
        Array(history.LegacyNipopowSnapshotKey -> legacySentinel),
        BlockSection.emptyArray
      ).get

      history.readPopowProofBytesFromDb() shouldBe None
      history.historyStorage
        .getIndex(history.LegacyNipopowSnapshotKey)
        .get
        .toSeq shouldBe legacySentinel.toSeq
    } finally {
      history.closeStorage()
    }
  }

  property("V2 NiPoPoW cache bytes survive history restart") {
    val directory = createTempDir
    val baseSettings = ErgoNodeTestConstants.initSettings
    val historySettings = baseSettings.copy(
      directory = directory.getAbsolutePath,
      nodeSettings = baseSettings.nodeSettings.copy(extraIndex = false)
    )
    val cachedBytes = Array[Byte](10, 20, 30, 40)
    val firstHistory = ErgoHistory.readOrGenerate(historySettings)(null)
    try {
      firstHistory.historyStorage.insert(
        Array(firstHistory.NipopowProofV2Key -> cachedBytes),
        BlockSection.emptyArray
      ).get
    } finally {
      firstHistory.closeStorage()
    }

    val reopenedHistory = ErgoHistory.readOrGenerate(historySettings)(null)
    try {
      reopenedHistory.readPopowProofBytesFromDb().get.toSeq shouldBe cachedBytes.toSeq
      reopenedHistory.readPopowProofBytesFromDb().get.toSeq shouldBe cachedBytes.toSeq
    } finally {
      reopenedHistory.closeStorage()
    }
  }

  property("periodic NiPoPoW snapshots refresh V2 without rewriting legacy bytes") {
    val history = genHistory(None, popowBootstrap = false)
    try {
      history.historyStorage.insert(
        Array(history.LegacyNipopowSnapshotKey -> legacySentinel),
        BlockSection.emptyArray
      ).get

      val chain = blockStream(None)
        .take(ErgoNodeTestConstants.settings.chainSettings.makeSnapshotEvery)
      applyChain(history, chain)

      history.readPopowProofBytesFromDb().isDefined shouldBe true
      history.historyStorage
        .getIndex(history.LegacyNipopowSnapshotKey)
        .get
        .toSeq shouldBe legacySentinel.toSeq
    } finally {
      history.closeStorage()
    }
  }

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
