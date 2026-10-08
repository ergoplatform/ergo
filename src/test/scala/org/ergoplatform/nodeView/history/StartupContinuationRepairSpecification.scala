package org.ergoplatform.nodeView.history

import org.ergoplatform.consensus.{ModifierSemanticValidity, ProgressInfo}
import org.ergoplatform.mining.difficulty.DifficultySerializer
import org.ergoplatform.modifiers.{BlockSection, ErgoFullBlock}
import org.ergoplatform.modifiers.history.header.Header
import org.ergoplatform.nodeView.history.ErgoHistoryUtils._
import org.ergoplatform.nodeView.state.StateType
import org.ergoplatform.utils.{ErgoCorePropertyTest, ErgoNodeTestConstants}
import org.ergoplatform.utils.generators.ChainGenerator.{applyBlock, genChain, nextBlock}
import org.ergoplatform.wallet.utils.FileUtils
import scorex.util.encode.Base16

class StartupContinuationRepairSpecification extends ErgoCorePropertyTest with FileUtils {
  import org.ergoplatform.utils.ErgoCoreTestConstants._
  import ErgoNodeTestConstants.initSettings

  private def withFork(withSibling: Boolean)(check: (ErgoHistory, ErgoFullBlock, ErgoFullBlock,
      Option[ErgoFullBlock], org.ergoplatform.settings.ErgoSettings) => Unit): Unit = {
    val initDiff = BigInt(2)
    val settings = initSettings.copy(
      directory = createTempDir.getAbsolutePath,
      chainSettings = initSettings.chainSettings.copy(
        epochLength = 3,
        useLastEpochs = 3,
        initialDifficultyHex = Base16.encode(initDiff.toByteArray)
      ),
      nodeSettings = initSettings.nodeSettings.copy(
        stateType = StateType.Digest,
        verifyTransactions = true,
        blocksToKeep = 100,
        extraIndex = false
      )
    )
    var history = ErgoHistory.readOrGenerate(settings)(null)
    history.writeMinimalFullBlockHeight(GenesisHeight)
    history.isHeadersChainSyncedVar = true

    val interval = history.difficultyCalculator.desiredInterval.toMillis
    val baseTime = System.currentTimeMillis() - 5 * interval
    val transactions = genChain(1).head.blockTransactions.txs

    def appendChild(parent: Option[ErgoFullBlock], halfIntervals: Long): ErgoFullBlock = {
      val seed = nextBlock(parent, transactions, defaultExtension)
      val nBits = parent match {
        case Some(p) => DifficultySerializer.encodeCompactBits(history.requiredDifficultyAfter(p.header))
        case None => DifficultySerializer.encodeCompactBits(initDiff)
      }
      val block = powScheme.proveBlock(
        parent.map(_.header),
        Header.InitialVersion,
        nBits,
        seed.header.stateRoot,
        seed.adProofs.get.proofBytes,
        seed.blockTransactions.txs,
        baseTime + halfIntervals * interval / 2,
        org.ergoplatform.modifiers.history.extension.ExtensionCandidate(seed.extension.fields),
        seed.header.votes,
        defaultMinerSecretNumber
      ).get
      history = applyBlock(history, block)
      block
    }

    val genesis = appendChild(None, 0)
    val a1 = appendChild(Some(genesis), 2)
    val a2 = appendChild(Some(a1), 4)
    val a3 = appendChild(Some(a2), 6)
    val a4 = appendChild(Some(a3), 8)
    val a5 = appendChild(Some(a4), 12)
    val a6 = appendChild(Some(a5), 14)
    val a7 = appendChild(Some(a6), 16)
    val a8 = appendChild(Some(a7), 18)

    val b1 = appendChild(Some(genesis), 1)
    val b2 = appendChild(Some(b1), 2)
    val b3 = appendChild(Some(b2), 3)
    val b4 = appendChild(Some(b3), 4)
    val b5 = appendChild(Some(b4), 5)
    val b6 = appendChild(Some(b5), 6)
    val b7 = appendChild(Some(b6), 7)
    val sibling = if (withSibling) Some(appendChild(Some(a7), 19)) else None

    b7.height shouldBe a8.height - 1
    history.scoreOf(b7.id).get should be > history.scoreOf(a8.id).get
    history.bestHeaderIdOpt shouldBe Some(b7.id)
    history.bestFullBlockIdOpt shouldBe Some(b7.id)
    try check(history, a8, b7, sibling, settings)
    finally history.closeStorage()
  }

  private def invalidate(history: ErgoHistory, block: ErgoFullBlock): Unit = {
    history.reportModifierIsInvalid(
      block,
      ProgressInfo[BlockSection](None, Seq.empty, Seq.empty, Seq.empty)(
        org.ergoplatform.utils.ScorexEncoder.default
      )
    ).get
  }

  property("startup removes only an explicitly invalid continuation") {
    withFork(withSibling = false) { (history, a8, b7, _, settings) =>
      invalidate(history, a8)
      history.isSemanticallyValid(a8.id) shouldBe ModifierSemanticValidity.Invalid
      history.bestFullBlockIdOpt shouldBe Some(b7.id)
      history.closeStorage()
      val reopened = ErgoHistory.readOrGenerate(settings)(null)
      try {
        reopened.headerIdsAtHeight(a8.height) should not contain a8.id
        reopened.historyStorage.modifierTypeAndBytesById(a8.id) shouldBe None
        reopened.isSemanticallyValid(a8.id) shouldBe ModifierSemanticValidity.Absent
        reopened.bestFullBlockIdOpt shouldBe Some(b7.id)
      } finally reopened.closeStorage()
    }
  }

  property("startup retains an unknown continuation") {
    withFork(withSibling = false) { (history, a8, b7, _, settings) =>
      history.isSemanticallyValid(a8.id) shouldBe ModifierSemanticValidity.Unknown
      history.closeStorage()
      val reopened = ErgoHistory.readOrGenerate(settings)(null)
      try {
        reopened.headerIdsAtHeight(a8.height) should contain(a8.id)
        reopened.historyStorage.modifierTypeAndBytesById(a8.id).map(_._1) shouldBe Some(Header.modifierTypeId)
        reopened.isSemanticallyValid(a8.id) shouldBe ModifierSemanticValidity.Unknown
        reopened.bestFullBlockIdOpt shouldBe Some(b7.id)
      } finally reopened.closeStorage()
    }
  }

  property("startup retains a mixed continuation row with a valid sibling") {
    withFork(withSibling = true) { (history, a8, b7, siblingOpt, settings) =>
      val sibling = siblingOpt.get
      history.reportModifierIsValid(a8).get
      invalidate(history, sibling)
      history.isSemanticallyValid(a8.id) shouldBe ModifierSemanticValidity.Valid
      history.isSemanticallyValid(sibling.id) shouldBe ModifierSemanticValidity.Invalid
      history.bestFullBlockIdOpt shouldBe Some(b7.id)
      history.closeStorage()
      val reopened = ErgoHistory.readOrGenerate(settings)(null)
      try {
        reopened.headerIdsAtHeight(a8.height) should contain allOf (a8.id, sibling.id)
        reopened.historyStorage.modifierTypeAndBytesById(a8.id).map(_._1) shouldBe Some(Header.modifierTypeId)
        reopened.isSemanticallyValid(a8.id) shouldBe ModifierSemanticValidity.Valid
        reopened.isSemanticallyValid(sibling.id) shouldBe ModifierSemanticValidity.Invalid
        reopened.bestFullBlockIdOpt shouldBe Some(b7.id)
      } finally reopened.closeStorage()
    }
  }
}
