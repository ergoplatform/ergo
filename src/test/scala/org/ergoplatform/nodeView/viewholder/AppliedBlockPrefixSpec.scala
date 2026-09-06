package org.ergoplatform.nodeView.viewholder

import akka.actor.{ActorRef, ActorSystem}
import akka.testkit.{TestActorRef, TestProbe}
import org.ergoplatform.consensus.ProgressInfo
import org.ergoplatform.core.{VersionTag, idToVersion, versionToId}
import org.ergoplatform.mining.AutolykosPowScheme
import org.ergoplatform.modifiers.BlockSection
import org.ergoplatform.modifiers.mempool.UnconfirmedTransaction
import org.ergoplatform.network.ErgoNodeViewSynchronizerMessages.{FullBlockApplied, LocalBlockApplied, RemoteBlockApplied}
import org.ergoplatform.nodeView.{ErgoNodeViewHolder, LocallyGeneratedModifier}
import org.ergoplatform.nodeView.ErgoNodeViewHolder.BlockAppliedTransactions
import org.ergoplatform.nodeView.history.ErgoHistory
import org.ergoplatform.nodeView.history.storage.HistoryStorage
import org.ergoplatform.nodeView.history.storage.modifierprocessors.EmptyBlockSectionProcessor
import org.ergoplatform.nodeView.mempool.ErgoMemPool
import org.ergoplatform.nodeView.state.{ErgoState, ErgoStateContext}
import org.ergoplatform.nodeView.wallet.ErgoWallet
import org.ergoplatform.nodeView.wallet.ErgoWalletActorMessages.ScanOnChain
import org.ergoplatform.settings.ErgoSettings
import org.ergoplatform.utils.{ErgoCorePropertyTest, ScorexEncoding}
import org.ergoplatform.utils.ErgoNodeTestConstants.initSettings
import org.ergoplatform.utils.generators.ErgoCoreTransactionGenerators.invalidErgoFullBlockGen
import org.ergoplatform.wallet.utils.FileUtils
import scorex.crypto.authds.ADDigest
import scorex.db.LDBVersionedStore
import scorex.util.ModifierId

import scala.collection.mutable.ArrayBuffer
import scala.concurrent.Await
import scala.concurrent.duration._
import scala.util.{Failure, Success, Try}

class AppliedBlockPrefixSpec extends ErgoCorePropertyTest with FileUtils with ScorexEncoding {

  // State validation and history selection are scripted; the holder's recursive
  // update and its mempool/wallet consumers run unchanged.
  private class ScriptedState(override val version: VersionTag,
                              rejected: ModifierId,
                              rollbacks: ArrayBuffer[VersionTag],
                              override protected val ergoSettings: ErgoSettings)
    extends ErgoState[ScriptedState] {
    override val store: LDBVersionedStore = null
    override def rootDigest: ADDigest = ergoSettings.chainSettings.genesisStateDigest
    override def stateContext: ErgoStateContext =
      ErgoStateContext.empty(ergoSettings.chainSettings, ergoSettings.launchParameters)
    override def rollbackVersions: Iterable[VersionTag] = rollbacks.toVector
    override def closeStorage(): Unit = ()
    override def rollbackTo(to: VersionTag): Try[ScriptedState] = {
      rollbacks += to
      Success(new ScriptedState(to, rejected, rollbacks, ergoSettings))
    }
    override def applyModifier(mod: BlockSection, estimatedTip: Option[Int])
                              (generate: LocallyGeneratedModifier => Unit): Try[ScriptedState] = {
      if (mod.id == rejected) Failure(new IllegalArgumentException("scripted state rejection"))
      else Success(new ScriptedState(idToVersion(mod.id), rejected, rollbacks, ergoSettings))
    }
  }

  private class ScriptedHistory(initial: ProgressInfo[BlockSection],
                                fallback: ProgressInfo[BlockSection],
                                override protected val settings: ErgoSettings)
    extends ErgoHistory with EmptyBlockSectionProcessor {
    override val historyStorage: HistoryStorage = null
    override val powScheme: AutolykosPowScheme = settings.chainSettings.powScheme
    override def contains(id: ModifierId): Boolean = false
    override def estimatedTip(): Option[Int] = None
    override def headersHeight: Int = 3
    override def fullBlockHeight: Int = 3
    override def bestHeaderIdOpt: Option[ModifierId] = None
    override def closeStorage(): Unit = ()
    override def append(mod: BlockSection): Try[(ErgoHistory, ProgressInfo[BlockSection])] =
      Success(this -> initial)
    override def reportModifierIsValid(mod: BlockSection): Try[ErgoHistory] = Success(this)
    override def reportModifierIsInvalid(mod: BlockSection, progress: ProgressInfo[BlockSection])
      : Try[(ErgoHistory, ProgressInfo[BlockSection])] = Success(this -> fallback)
  }

  private class Holder(settings: ErgoSettings, initialView: (ErgoHistory, ScriptedState, ErgoWallet, ErgoMemPool))
    extends ErgoNodeViewHolder[ScriptedState](settings) {
    override def restoreState(): Option[NodeView] = Some(initialView)
    def applySection(section: BlockSection, local: Boolean): Unit = pmodModify(section, local)
    def pool: ErgoMemPool = memoryPool()
    def stateVersion: VersionTag = minimalState().version
  }

  private def checkFallback(prefixLength: Int, continue: Boolean, rollback: Boolean, local: Boolean): Unit = {
    implicit val system: ActorSystem = ActorSystem("applied-block-prefix")
    val settings = initSettings.copy(directory = createTempDir.getAbsolutePath)
    try {
      val blocks = Vector.fill(5)(invalidErgoFullBlockGen.sample.get)
      val prefix = blocks.take(prefixLength)
      val rejected = blocks(2)
      val continuation = if (continue) Vector(blocks(3)) else Vector.empty
      val initialVersion = ErgoState.genesisStateVersion
      val currentVersion = prefix.lastOption.map(b => idToVersion(b.id)).getOrElse(initialVersion)
      val branchPoint = if (rollback) initialVersion else currentVersion
      val initial = ProgressInfo[BlockSection](None, Seq.empty, prefix :+ rejected, Seq.empty)
      val fallback = ProgressInfo[BlockSection](Some(versionToId(branchPoint)), Seq(rejected), continuation, Seq.empty)
      val history = new ScriptedHistory(initial, fallback, settings)
      val rollbacks = ArrayBuffer.empty[VersionTag]
      val state = new ScriptedState(initialVersion, rejected.id, rollbacks, settings)
      val scans = TestProbe()
      val wallet = new ErgoWallet(history, settings, settings.launchParameters) {
        override val walletActor: ActorRef = scans.ref
      }
      val allTxs = blocks.flatMap(_.transactions)
      val pool = ErgoMemPool.empty(settings).put(allTxs.map(tx => UnconfirmedTransaction(tx, None)))
      val holder = TestActorRef(new Holder(settings, (history, state, wallet, pool)))
      val events = TestProbe()
      system.eventStream.subscribe(events.ref, classOf[FullBlockApplied])
      system.eventStream.subscribe(events.ref, classOf[BlockAppliedTransactions])

      holder.underlyingActor.applySection(rejected.header, local)

      val retained = (if (rollback) Vector.empty else prefix) ++ continuation
      val appliedTxs = retained.flatMap(_.transactions)
      val immediate = prefix ++ continuation
      immediate.foreach { block =>
        val event = events.expectMsgType[FullBlockApplied]
        event.header.id shouldBe block.id
        event.txIds shouldBe block.transactions.map(_.id)
        if (local) event shouldBe a[LocalBlockApplied] else event shouldBe a[RemoteBlockApplied]
      }
      events.expectMsg(BlockAppliedTransactions(appliedTxs.map(_.id)))
      events.expectNoMessage(100.millis)
      retained.foreach(block => scans.expectMsg(ScanOnChain(block)))
      scans.expectNoMessage(100.millis)
      appliedTxs.foreach(tx => holder.underlyingActor.pool.contains(tx.id) shouldBe false)
      blocks(4).transactions.foreach(tx => holder.underlyingActor.pool.contains(tx.id) shouldBe true)
      if (rollback) prefix.flatMap(_.transactions).foreach { tx =>
        holder.underlyingActor.pool.contains(tx.id) shouldBe true
      }
      holder.underlyingActor.stateVersion shouldBe continuation.lastOption
        .map(b => idToVersion(b.id)).getOrElse(branchPoint)
      rollbacks.toVector shouldBe (if (rollback) Vector(initialVersion) else Vector.empty)
      system.stop(holder)
    } finally {
      Await.result(system.terminate(), 20.seconds)
    }
  }

  property("retain the applied prefix when recursive fallback needs no rollback and has no continuation") {
    checkFallback(prefixLength = 2, continue = false, rollback = false, local = false)
  }

  property("append fallback blocks to the retained prefix without duplicate scans or events") {
    checkFallback(prefixLength = 2, continue = true, rollback = false, local = true)
  }

  property("a first-block rejection does not invent an applied prefix") {
    checkFallback(prefixLength = 0, continue = false, rollback = false, local = false)
  }

  property("an actual rollback before the accumulated prefix discards it for the consumers") {
    checkFallback(prefixLength = 2, continue = true, rollback = true, local = false)
  }
}
