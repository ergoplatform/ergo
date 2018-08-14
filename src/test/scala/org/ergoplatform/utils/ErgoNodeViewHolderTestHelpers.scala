package org.ergoplatform.utils

import java.io.File

import akka.actor.{ActorRef, ActorSystem}
import akka.testkit.TestProbe
import org.ergoplatform.mining.DefaultFakePowScheme
import org.ergoplatform.modifiers.history.Header
import org.ergoplatform.modifiers.{ErgoFullBlock, ErgoPersistentModifier}
import org.ergoplatform.nodeView.ErgoNodeViewRef
import org.ergoplatform.nodeView.history.ErgoHistory
import org.ergoplatform.nodeView.mempool.ErgoMemPool
import org.ergoplatform.nodeView.state.{DigestState, ErgoState, StateType, UtxoState}
import org.ergoplatform.nodeView.wallet.ErgoWallet
import org.ergoplatform.settings.{Algos, ErgoSettings}
import org.scalatest.BeforeAndAfterAll
import scorex.core.ModifierId
import scorex.core.NodeViewHolder.ReceivableMessages.GetDataFromCurrentView

import scala.concurrent.ExecutionContext
import scala.concurrent.duration.FiniteDuration
import scala.reflect.ClassTag

trait ErgoNodeViewHolderTestHelpers extends ErgoPropertyTest with BeforeAndAfterAll with NoShrink {
  implicit val system: ActorSystem = ActorSystem("WithIsoFix")
  implicit val executionContext: ExecutionContext = system.dispatchers.lookup("scorex.executionContext")

  case class NodeViewHolderConfig(stateType: StateType,
                                  verifyTransactions: Boolean,
                                  popowBootstrap: Boolean,
                                  genesisId: Option[String] = None) {
    override def toString: String = {
      s"State: $stateType, Verify Transactions: $verifyTransactions, PoPoW Bootstrap: $popowBootstrap"
    }
  }

  override def afterAll(): Unit = {
    system.terminate()
  }

  protected def actorRef(c: NodeViewHolderConfig, dirOpt: Option[File] = None): ActorRef = {
    val dir: File = dirOpt.getOrElse(createTempDir)
    val defaultSettings: ErgoSettings = ErgoSettings.read(None).copy(directory = dir.getAbsolutePath)
    val settings = defaultSettings.copy(
      nodeSettings = defaultSettings.nodeSettings.copy(
        stateType = c.stateType,
        verifyTransactions = c.verifyTransactions,
        PoPoWBootstrap = c.popowBootstrap,
        genesisId = c.genesisId
      ),
      chainSettings = defaultSettings.chainSettings.copy(powScheme = DefaultFakePowScheme)
    )
    ErgoNodeViewRef(settings, timeProvider, emission)
  }


  type H = ErgoHistory
  type S = ErgoState[_]
  type D = DigestState
  type U = UtxoState
  type W = ErgoWallet
  type P = ErgoMemPool
  type C = NodeViewHolderConfig

  val allConfigs: List[NodeViewHolderConfig] = List(
    NodeViewHolderConfig(StateType.Digest, verifyTransactions = true, popowBootstrap = true),
    NodeViewHolderConfig(StateType.Digest, verifyTransactions = false, popowBootstrap = true),
    NodeViewHolderConfig(StateType.Digest, verifyTransactions = false, popowBootstrap = false),
    NodeViewHolderConfig(StateType.Digest, verifyTransactions = true, popowBootstrap = false),
    NodeViewHolderConfig(StateType.Utxo, verifyTransactions = true, popowBootstrap = true),
    NodeViewHolderConfig(StateType.Utxo, verifyTransactions = true, popowBootstrap = false),
  )

  protected def checkAfterGenesisState(c: C) = GetDataFromCurrentView[H, S, W, P, Boolean] { v =>
    java.util.Arrays.equals(v.state.rootHash, settings.chainSettings.monetary.afterGenesisStateDigest)
  }

  protected def bestHeaderOpt(c: C) = GetDataFromCurrentView[H, S, W, P, Option[Header]](v => v.history.bestHeaderOpt)

  protected def historyHeight(c: C) = GetDataFromCurrentView[H, S, W, P, Int](v => v.history.headersHeight)

  protected def heightOf(id: ModifierId, c: C) = GetDataFromCurrentView[H, S, W, P, Option[Int]](v => v.history.heightOf(id))

  protected def lastHeadersLength(count: Int, c: C) =
    GetDataFromCurrentView[H, S, W, P, Int](v => v.history.lastHeaders(count).size)


  protected def openSurfaces(c: C) = GetDataFromCurrentView[H, S, W, P, Seq[ModifierId]] { v =>
    v.history.openSurfaceIds()
  }

  protected def bestFullBlock(c: C) = GetDataFromCurrentView[H, S, W, P, Option[ErgoFullBlock]] { v =>
    v.history.bestFullBlockOpt
  }

  protected def modifierById(id: ModifierId) = GetDataFromCurrentView[H, S, W, P, Option[ErgoPersistentModifier]] { v =>
    v.history.modifierById(id)
  }

  protected def bestFullBlockEncodedId(c: C) = GetDataFromCurrentView[H, S, W, P, Option[String]] { v =>
    v.history.bestFullBlockOpt.map(_.header.encodedId)
  }

  protected def poolSize(c: C) = GetDataFromCurrentView[H, S, W, P, Int](v => v.pool.size)

  protected def rootHash(c: C) = GetDataFromCurrentView[H, S, W, P, String](v => Algos.encode(v.state.rootHash))

  case class TestCase(name: String)(test: NodeViewFixture => Unit) {
    def run(c: NodeViewHolderConfig): Unit = new NodeViewFixture(c).run(test)
  }

  /** This uses TestProbe to receive messages from actor.
    * To make TestProbe work `defaultSender` implicit should be imported
    */
  class NodeViewFixture(val nodeViewConfig: NodeViewHolderConfig) {
    val nodeViewDir: java.io.File = createTempDir
    val nodeViewRef: ActorRef = actorRef(nodeViewConfig, Option(nodeViewDir))
    val testProbe = new TestProbe(system)

    /** This sender should be imported to make TestProbe work! */
    implicit val defaultSender: ActorRef = testProbe.testActor

    @inline def send(msg: Any): Unit = testProbe.send(nodeViewRef, msg)

    @inline def defaultTimeout: FiniteDuration = testProbe.remainingOrDefault

    @inline def expectMsg[T](obj: T): T = testProbe.expectMsg(obj)

    @inline def expectMsgType[T](implicit t: ClassTag[T]): T = testProbe.expectMsgType

    @inline def expectNoMsg(): Unit = testProbe.expectNoMessage(defaultTimeout)

    def subscribeEvents(eventClass: Class[_]): Boolean = system.eventStream.subscribe(testProbe.ref, eventClass)

    def withRecoveredNodeViewRef(test: ActorRef => Unit): Unit = {
      val a = actorRef(nodeViewConfig, Option(nodeViewDir))
      try test(a) finally system.stop(a)
    }

    def run(test: NodeViewFixture => Unit): Unit = try test(this) finally stop()

    def stop(): Unit = {
      system.stop(nodeViewRef)
      system.stop(testProbe.testActor)
    }
  }
}
