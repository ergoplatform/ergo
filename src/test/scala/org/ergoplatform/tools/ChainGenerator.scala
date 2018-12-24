package org.ergoplatform.tools

import java.io.File

import akka.actor.{ActorRef, ActorSystem}
import akka.testkit.TestKit
import org.ergoplatform.local.ErgoMiner.StartMining
import org.ergoplatform.local.TransactionGenerator.StartGeneration
import org.ergoplatform.local.{ErgoMinerRef, TransactionGeneratorRef}
import org.ergoplatform.mining.AutolykosPowScheme
import org.ergoplatform.modifiers.{ErgoFullBlock, ErgoPersistentModifier}
import org.ergoplatform.nodeView.history.ErgoHistory
import org.ergoplatform.nodeView.history.storage.modifierprocessors.{FullBlockPruningProcessor, ToDownloadProcessor}
import org.ergoplatform.nodeView.mempool.ErgoMemPool
import org.ergoplatform.nodeView.state._
import org.ergoplatform.nodeView.wallet.ErgoWallet
import org.ergoplatform.nodeView.{ErgoNodeViewRef, ErgoReadersHolderRef}
import org.ergoplatform.settings._
import org.ergoplatform.utils.ErgoTestHelpers
import scorex.core.NodeViewHolder.ReceivableMessages.GetDataFromCurrentView
import scorex.core.network.NodeViewSynchronizer.ReceivableMessages.SemanticallySuccessfulModifier

import scala.concurrent.duration._

/**
  * Application object for chain generation.
  * Takes 2 parameters: start timestamp and path to history folder.
  * Generate blocks starting from start timestamp and until current time with expected block interval
  * between them, to ensure that difficulty does not change.
  */
object ChainGenerator extends TestKit(ActorSystem()) with App with ErgoTestHelpers {

  val pow = new AutolykosPowScheme(powScheme.k, powScheme.n)
  val blockInterval = 2.minute

  val dir = if (args.length < 2) new File("/tmp/ergo/node1/data") else new File(args(1))
  val txsSize: Int = if (args.length < 3) 100 * 1024 else args(2).toInt

  val miningDelay = 1.second
  val minimalSuffix = 2
  val nodeSettings: NodeConfigurationSettings = NodeConfigurationSettings(StateType.Utxo, verifyTransactions = true,
    -1, PoPoWBootstrap = false, minimalSuffix, mining = false, miningDelay, offlineGeneration = false, 200)
  val chainSettings = ChainSettings(0: Byte, blockInterval, 256, 8, pow, settings.chainSettings.monetary)
  val fullHistorySettings: ErgoSettings = ErgoSettings(dir.getAbsolutePath, chainSettings, settings.testingSettings,
    nodeSettings, settings.scorexSettings, settings.walletSettings, CacheSettings.default)

  lazy val ergoSettings: ErgoSettings = ErgoSettings.read(args.headOption)

  val nodeViewHolderRef: ActorRef = ErgoNodeViewRef(ergoSettings, timeProvider)

  val readersHolderRef: ActorRef = ErgoReadersHolderRef(nodeViewHolderRef)

  val minerRef: ActorRef = ErgoMinerRef(ergoSettings, nodeViewHolderRef, readersHolderRef, timeProvider)

  val txGenRef = TransactionGeneratorRef(nodeViewHolderRef, ergoSettings)

  val startTime = args.headOption.map(_.toLong).getOrElse(timeProvider.time - (blockInterval * 10).toMillis)

  Thread.sleep(5000)

  system.eventStream.subscribe(testActor, classOf[SemanticallySuccessfulModifier[ErgoPersistentModifier]])

  nodeViewHolderRef ! GetDataFromCurrentView[ErgoHistory, DigestState, ErgoWallet, ErgoMemPool, Unit] { v =>
    allowToApplyOldBlocks(v.history)
  }

  minerRef ! StartMining
  txGenRef ! StartGeneration

  receiveWhile() {
    case SemanticallySuccessfulModifier(block: ErgoFullBlock)
      if block.header.timestamp >= timeProvider.time =>
      nodeViewHolderRef ! GetDataFromCurrentView[ErgoHistory, DigestState, ErgoWallet, ErgoMemPool, Unit] { v =>
        log.info(s"Chain of length ${v.history.bestHeaderOpt.map(_.height).getOrElse(0)} generated")
        v.history.bestHeaderOpt shouldBe v.history.bestFullBlockOpt.map(_.header)
        log.info("History was generated successfully")
        System.exit(0)
      }
  }

  /**
    * Use reflection to set `minimalFullBlockHeightVar` to 0 to change regular synchronization rule, that we
    * first apply headers chain, and apply full blocks only after that
    */
  private def allowToApplyOldBlocks(history: ErgoHistory): Unit = {
    import scala.reflect.runtime.{universe => ru}
    val runtimeMirror = ru.runtimeMirror(getClass.getClassLoader)
    val procInstance = runtimeMirror.reflect(history.asInstanceOf[ToDownloadProcessor])
    val ppM = ru.typeOf[ToDownloadProcessor].member(ru.TermName("pruningProcessor")).asMethod
    val pp = procInstance.reflectMethod(ppM).apply().asInstanceOf[FullBlockPruningProcessor]
    val f = ru.typeOf[FullBlockPruningProcessor].member(ru.TermName("minimalFullBlockHeightVar")).asTerm.accessed.asTerm
    runtimeMirror.reflect(pp).reflectField(f).set(ErgoHistory.GenesisHeight)
    val f2 = ru.typeOf[FullBlockPruningProcessor].member(ru.TermName("isHeadersChainSyncedVar")).asTerm.accessed.asTerm
    runtimeMirror.reflect(pp).reflectField(f2).set(true)
  }

}
