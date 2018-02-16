package org.ergoplatform.nodeView

import java.io.File

import akka.actor.{ActorRef, ActorSystem, Props}
import akka.testkit.TestProbe
import io.iohk.iodb.ByteArrayWrapper
import org.ergoplatform.mining.DefaultFakePowScheme
import org.ergoplatform.modifiers.history.{ADProofs, BlockTransactions, Header}
import org.ergoplatform.modifiers.mempool.AnyoneCanSpendTransaction
import org.ergoplatform.modifiers.mempool.proposition.AnyoneCanSpendProposition
import org.ergoplatform.modifiers.{ErgoFullBlock, ErgoPersistentModifier}
import org.ergoplatform.nodeView.history.ErgoHistory
import org.ergoplatform.nodeView.mempool.ErgoMemPool
import org.ergoplatform.nodeView.state.{DigestState, ErgoState, UtxoState}
import org.ergoplatform.nodeView.wallet.ErgoWallet
import org.ergoplatform.settings.{Algos, ErgoSettings}
import org.ergoplatform.utils.ErgoGenerators
import org.scalatest.{BeforeAndAfterAll, Matchers, PropSpec}
import scorex.core.LocalInterface.{LocallyGeneratedModifier, LocallyGeneratedTransaction}
import scorex.core.NodeViewHolder.EventType._
import scorex.core.NodeViewHolder.{GetDataFromCurrentView, SyntacticallySuccessfulModifier}
import scorex.core.utils.NetworkTimeProvider
import scorex.core.{ModifierId, NodeViewHolder}
import scorex.testkit.utils.FileUtils

import scala.reflect.ClassTag

class ErgoNodeViewHolderSpecification extends PropSpec
  with ErgoGenerators
  with Matchers
  with FileUtils
  with BeforeAndAfterAll {

  implicit val system = ActorSystem("WithIsoFix")

  case class NodeViewHolderConfig(adState: Boolean, verifyTransactions: Boolean, popowBootstrap: Boolean) {
    override def toString: String = {
      val state = if (adState) {
        "DigestState"
      } else {
        "UtxoState"
      }

      s"State: $state, Verify Transactions: $verifyTransactions, PoPoW Bootstrap: $popowBootstrap"
    }
  }

  override def afterAll() = {
    system.terminate()
  }

  type H = ErgoHistory
  type S = ErgoState[_]
  type D = DigestState
  type U = UtxoState
  type W = ErgoWallet
  type P = ErgoMemPool
  type C = NodeViewHolderConfig

  val allConfigs: List[NodeViewHolderConfig] = List(
    NodeViewHolderConfig(adState = true, verifyTransactions = true, popowBootstrap = true),
    NodeViewHolderConfig(adState = true, verifyTransactions = false, popowBootstrap = true),
    NodeViewHolderConfig(adState = true, verifyTransactions = false, popowBootstrap = false),
    NodeViewHolderConfig(adState = true, verifyTransactions = true, popowBootstrap = false),
    NodeViewHolderConfig(adState = false, verifyTransactions = true, popowBootstrap = true),
    NodeViewHolderConfig(adState = false, verifyTransactions = true, popowBootstrap = false),
    //TODO     NodeViewHolderConfig(false, false, ???),
  )

  def actorRef(c: NodeViewHolderConfig, dirOpt: Option[File] = None): ActorRef = {
    val dir: File = dirOpt.getOrElse(createTempDir)
    val defaultSettings: ErgoSettings = ErgoSettings.read(None).copy(directory = dir.getAbsolutePath)
    val settings = defaultSettings.copy(
      nodeSettings = defaultSettings.nodeSettings.copy(
        ADState = c.adState,
        verifyTransactions = c.verifyTransactions,
        PoPoWBootstrap = c.popowBootstrap
      ),
      chainSettings = defaultSettings.chainSettings.copy(poWScheme = DefaultFakePowScheme)
    )
    val timeProvider: NetworkTimeProvider = new NetworkTimeProvider(settings.scorexSettings.ntp)
    if (c.adState) {
      system.actorOf(Props(classOf[DigestErgoNodeViewHolder], settings, timeProvider))
    } else {
      system.actorOf(Props(classOf[UtxoErgoNodeViewHolder], settings, timeProvider))
    }
  }

  def checkAfterGenesisState(c: C) = GetDataFromCurrentView[H, S, W, P, Boolean] { v =>
    v.state.rootHash.sameElements(ErgoState.afterGenesisStateDigest)
  }

  def bestHeaderOpt(c: C) = GetDataFromCurrentView[H, S, W, P, Option[Header]](v => v.history.bestHeaderOpt)

  def historyHeight(c: C) = GetDataFromCurrentView[H, S, W, P, Int](v => v.history.headersHeight)

  def heightOf(id: ModifierId, c: C) = GetDataFromCurrentView[H, S, W, P, Option[Int]](v => v.history.heightOf(id))

  def lastHeadersLength(count: Int, c: C) =
    GetDataFromCurrentView[H, S, W, P, Int](v => v.history.lastHeaders(count).size)


  def openSurfaces(c: C) = GetDataFromCurrentView[H, S, W, P, Seq[ByteArrayWrapper]] { v =>
    v.history.openSurfaceIds().map(ByteArrayWrapper.apply)
  }

  def bestFullBlock(c: C) = GetDataFromCurrentView[H, S, W, P, Option[ErgoFullBlock]] { v =>
    v.history.bestFullBlockOpt
  }

  def modifierById(id: ModifierId) = GetDataFromCurrentView[H, S, W, P, Option[ErgoPersistentModifier]] { v =>
    v.history.modifierById(id)
  }

  def bestFullBlockEncodedId(c: C) = GetDataFromCurrentView[H, S, W, P, Option[String]] { v =>
    v.history.bestFullBlockOpt.map(_.header.encodedId)
  }

  def poolSize(c: C) = GetDataFromCurrentView[H, S, W, P, Int](v => v.pool.size)

  def rootHash(c: C) = GetDataFromCurrentView[H, S, W, P, String](v => Algos.encode(v.state.rootHash))

  case class TestCase(name: String)(test: NodeViewFixture => Unit) {
    def run(c: NodeViewHolderConfig): Unit = new NodeViewFixture(c).run(test)
  }

  /** This uses TestProbe to receive messages from actor.
    * To make TestProbe work `defaultSender` implicit should be imported
    */
  class NodeViewFixture(val nodeViewConfig: NodeViewHolderConfig) {
    val nodeViewDir = createTempDir
    val nodeViewRef: ActorRef = actorRef(nodeViewConfig, Option(nodeViewDir))
    val testProbe = new TestProbe(system)

    /** This sender should be imported to make TestProbe work! */
    implicit val defaultSender: ActorRef = testProbe.testActor

    @inline def send(msg: Any): Unit = testProbe.send(nodeViewRef, msg)
    @inline def defaultTimeout = testProbe.remainingOrDefault
    @inline def expectMsg[T](obj: T) = testProbe.expectMsg(obj)
    @inline def expectMsgType[T](implicit t: ClassTag[T]) = testProbe.expectMsgType
    @inline def expectNoMsg = testProbe.expectNoMessage(defaultTimeout)

    def withRecoveredNodeViewRef(test: ActorRef => Unit) = {
      val a = actorRef(nodeViewConfig, Option(nodeViewDir))
      try test(a) finally system.stop(a)
    }

    def run(test: NodeViewFixture => Unit) = try test(this) finally stop()
    def stop() = { system.stop(nodeViewRef); system.stop(testProbe.ref) }
  }

  val t1 = TestCase("check genesis state") { fixture =>
    import fixture._
    nodeViewRef ! checkAfterGenesisState(nodeViewConfig)
    expectMsg(true)
  }

  val t2 = TestCase("check history after genesis") { fixture =>
    import fixture._
    nodeViewRef ! bestHeaderOpt(nodeViewConfig)
    expectMsg(None)
  }

  val t3 = TestCase("apply valid block header") { fixture =>
    import fixture._
    val dir = createTempDir
    val (us, bh) = ErgoState.generateGenesisUtxoState(dir, Some(nodeViewRef))
    val block = validFullBlock(None, us, bh)

    nodeViewRef ! bestHeaderOpt(nodeViewConfig)
    expectMsg(None)

    nodeViewRef ! historyHeight(nodeViewConfig)
    expectMsg(-1)

    nodeViewRef ! NodeViewHolder.Subscribe(Seq(SuccessfulSyntacticallyValidModifier))

    //sending header
    nodeViewRef ! LocallyGeneratedModifier[Header](block.header)
    expectMsgType[SyntacticallySuccessfulModifier[Header]]

    nodeViewRef ! historyHeight(nodeViewConfig)
    expectMsg(0)

    nodeViewRef ! heightOf(block.header.id, nodeViewConfig)
    expectMsg(Some(0))

    nodeViewRef ! lastHeadersLength(10, nodeViewConfig)
    expectMsg(1)

    nodeViewRef ! openSurfaces(nodeViewConfig)
    expectMsg(Seq(ByteArrayWrapper(block.header.id)))

    nodeViewRef ! bestHeaderOpt(nodeViewConfig)
    expectMsg(Some(block.header))
  }

  val t4 = TestCase("apply valid block as genesis") { fixture =>
    import fixture._
    val dir = createTempDir
    val (us, bh) = ErgoState.generateGenesisUtxoState(dir, Some(nodeViewRef))
    val genesis = validFullBlock(parentOpt = None, us, bh)

    nodeViewRef ! NodeViewHolder.Subscribe(Seq(SuccessfulSyntacticallyValidModifier))

    nodeViewRef ! LocallyGeneratedModifier(genesis.header)
    expectMsgType[SyntacticallySuccessfulModifier[Header]]

    if (nodeViewConfig.verifyTransactions) {
      nodeViewRef ! LocallyGeneratedModifier(genesis.blockTransactions)
      nodeViewRef ! LocallyGeneratedModifier(genesis.aDProofs.get)

      if (nodeViewConfig.adState) {
        expectMsgType[SyntacticallySuccessfulModifier[ADProofs]]
        expectMsgType[SyntacticallySuccessfulModifier[ADProofs]]
      }
      else {
        expectMsgType[SyntacticallySuccessfulModifier[BlockTransactions]]
        expectMsgType[SyntacticallySuccessfulModifier[BlockTransactions]]
      }

      nodeViewRef ! bestFullBlock(nodeViewConfig)
      expectMsg(Some(genesis))
    }
  }

  val t5 = TestCase("apply full blocks after genesis") { fixture =>
    import fixture._
    val dir = createTempDir
    val (us, bh) = ErgoState.generateGenesisUtxoState(dir, Some(nodeViewRef))
    val genesis = validFullBlock(parentOpt = None, us, bh)
    val wusAfterGenesis = WrappedUtxoState(us, bh, None).applyModifier(genesis).get

    nodeViewRef ! LocallyGeneratedModifier(genesis.header)
    if (nodeViewConfig.verifyTransactions) {
      nodeViewRef ! LocallyGeneratedModifier(genesis.blockTransactions)
      nodeViewRef ! LocallyGeneratedModifier(genesis.aDProofs.get)
    }

    val block = validFullBlock(Some(genesis.header), wusAfterGenesis)

    nodeViewRef ! LocallyGeneratedModifier(block.header)
    if (nodeViewConfig.verifyTransactions) {
      nodeViewRef ! LocallyGeneratedModifier(block.blockTransactions)
      nodeViewRef ! LocallyGeneratedModifier(block.aDProofs.get)
      nodeViewRef ! bestFullBlock(nodeViewConfig)
      expectMsg(Some(block))
    }

    nodeViewRef ! bestHeaderOpt(nodeViewConfig)
    expectMsg(Some(block.header))

    nodeViewRef ! historyHeight(nodeViewConfig)
    expectMsg(1)

    nodeViewRef ! lastHeadersLength(10, nodeViewConfig)
    expectMsg(2)
  }

  val t6 = TestCase("add transaction to memory pool") { fixture =>
    import fixture._
    val tx = AnyoneCanSpendTransaction(IndexedSeq.empty[Long], IndexedSeq.empty[Long])

    nodeViewRef ! NodeViewHolder.Subscribe(Seq(FailedTransaction))
    nodeViewRef ! LocallyGeneratedTransaction[AnyoneCanSpendProposition.type, AnyoneCanSpendTransaction](tx)
    expectNoMsg
    nodeViewRef ! poolSize(nodeViewConfig)
    expectMsg(1)
  }

  val t7 = TestCase("apply invalid full block") { fixture =>
    import fixture._
    val dir = createTempDir
    val (us, bh) = ErgoState.generateGenesisUtxoState(dir, Some(nodeViewRef))
    val genesis = validFullBlock(parentOpt = None, us, bh)
    val wusAfterGenesis = WrappedUtxoState(us, bh, None).applyModifier(genesis).get

    nodeViewRef ! LocallyGeneratedModifier(genesis.header)
    if (nodeViewConfig.verifyTransactions) {
      nodeViewRef ! LocallyGeneratedModifier(genesis.blockTransactions)
      nodeViewRef ! LocallyGeneratedModifier(genesis.aDProofs.get)
    }

    val block = validFullBlock(Some(genesis.header), wusAfterGenesis)
    val wusAfterBlock = wusAfterGenesis.applyModifier(block).get

    nodeViewRef ! LocallyGeneratedModifier(block.header)
    if (nodeViewConfig.verifyTransactions) {
      nodeViewRef ! LocallyGeneratedModifier(block.blockTransactions)
      nodeViewRef ! LocallyGeneratedModifier(block.aDProofs.get)
      nodeViewRef ! rootHash(nodeViewConfig)
      expectMsg(Algos.encode(wusAfterBlock.rootHash))
    }

    nodeViewRef ! bestHeaderOpt(nodeViewConfig)
    expectMsg(Some(block.header))

    val brokenBlock = validFullBlock(Some(block.header), wusAfterBlock)

    nodeViewRef ! LocallyGeneratedModifier(brokenBlock.header)

    val brokenTransactions = brokenBlock.blockTransactions.copy(txs = brokenBlock.blockTransactions.txs.tail)
    if (nodeViewConfig.verifyTransactions) {
      nodeViewRef ! LocallyGeneratedModifier(brokenTransactions)
      nodeViewRef ! LocallyGeneratedModifier(brokenBlock.aDProofs.get)
      nodeViewRef ! bestFullBlock(nodeViewConfig)
      expectMsg(Some(block))
    }

    nodeViewRef ! bestHeaderOpt(nodeViewConfig)
    //TODO Note and verify!
    expectMsg(Some(brokenBlock.header))
  }

  val t8 = TestCase("switching for a better chain") { fixture =>
    import fixture._
    val dir = createTempDir
    val (us, bh) = ErgoState.generateGenesisUtxoState(dir, Some(nodeViewRef))
    val genesis = validFullBlock(parentOpt = None, us, bh)
    val wusAfterGenesis = WrappedUtxoState(us, bh, None).applyModifier(genesis).get

    nodeViewRef ! LocallyGeneratedModifier(genesis.header)
    if (nodeViewConfig.verifyTransactions) {
      nodeViewRef ! LocallyGeneratedModifier(genesis.blockTransactions)
      nodeViewRef ! LocallyGeneratedModifier(genesis.aDProofs.get)
    }

    nodeViewRef ! rootHash(nodeViewConfig)
    expectMsg(Algos.encode(wusAfterGenesis.rootHash))

    val chain1block1 = validFullBlock(Some(genesis.header), wusAfterGenesis)

    nodeViewRef ! LocallyGeneratedModifier(chain1block1.header)
    if (nodeViewConfig.verifyTransactions) {
      nodeViewRef ! LocallyGeneratedModifier(chain1block1.blockTransactions)
      nodeViewRef ! LocallyGeneratedModifier(chain1block1.aDProofs.get)
    }

    nodeViewRef ! GetDataFromCurrentView[ErgoHistory, DigestState, ErgoWallet, ErgoMemPool, Option[ErgoFullBlock]] { v =>
      v.history.bestFullBlockOpt
    }
    if (nodeViewConfig.verifyTransactions) expectMsg(Some(chain1block1)) else expectMsg(None)
    nodeViewRef ! bestHeaderOpt(nodeViewConfig)
    expectMsg(Some(chain1block1.header))

    val chain2block1 = validFullBlock(Some(genesis.header), wusAfterGenesis)

    nodeViewRef ! LocallyGeneratedModifier(chain2block1.header)
    if (nodeViewConfig.verifyTransactions) {
      nodeViewRef ! LocallyGeneratedModifier(chain2block1.blockTransactions)
      nodeViewRef ! LocallyGeneratedModifier(chain2block1.aDProofs.get)
    }

    nodeViewRef ! GetDataFromCurrentView[ErgoHistory, DigestState, ErgoWallet, ErgoMemPool, Option[ErgoFullBlock]] { v =>
      v.history.bestFullBlockOpt
    }
    if (nodeViewConfig.verifyTransactions) expectMsg(Some(chain1block1)) else expectMsg(None)
    nodeViewRef ! bestHeaderOpt(nodeViewConfig)
    expectMsg(Some(chain1block1.header))

    val wusChain2Block1 = wusAfterGenesis.applyModifier(chain2block1).get
    val chain2block2 = validFullBlock(Some(chain2block1.header), wusChain2Block1)

    chain2block1.header.stateRoot shouldEqual wusChain2Block1.rootHash

    nodeViewRef ! LocallyGeneratedModifier(chain2block2.header)
    if (nodeViewConfig.verifyTransactions) {
      nodeViewRef ! LocallyGeneratedModifier(chain2block2.blockTransactions)
      nodeViewRef ! LocallyGeneratedModifier(chain2block2.aDProofs.get)
      nodeViewRef ! bestFullBlockEncodedId(nodeViewConfig)
      expectMsg(Some(chain2block2.header.encodedId))
    }

    nodeViewRef ! bestHeaderOpt(nodeViewConfig)
    expectMsg(Some(chain2block2.header))

    nodeViewRef ! rootHash(nodeViewConfig)
    expectMsg(Algos.encode(chain2block2.header.stateRoot))

  }

  val t9 = TestCase("UTXO state should generate ADProofs and put them in history") { fixture =>
    import fixture._
    if (!nodeViewConfig.adState) {
      val dir = createTempDir
      val (us, bh) = ErgoState.generateGenesisUtxoState(dir, Some(nodeViewRef))
      val genesis = validFullBlock(parentOpt = None, us, bh)

      nodeViewRef ! LocallyGeneratedModifier(genesis.header)

      nodeViewRef ! LocallyGeneratedModifier(genesis.blockTransactions)

      nodeViewRef ! bestFullBlock(nodeViewConfig)
      expectMsg(Some(genesis))

      nodeViewRef ! modifierById(genesis.aDProofs.get.id)
      expectMsg(genesis.aDProofs)
    }
  }

  val t10 = TestCase("NodeViewHolder start from inconsistent state") { fixture =>
    import fixture._
    if (nodeViewConfig.verifyTransactions) {

      val dir = createTempDir
      val (us, bh) = ErgoState.generateGenesisUtxoState(dir, Some(nodeViewRef))
      val genesis = validFullBlock(parentOpt = None, us, bh)
      val wusAfterGenesis = WrappedUtxoState(us, bh, None).applyModifier(genesis).get

      nodeViewRef ! LocallyGeneratedModifier(genesis.header)
      nodeViewRef ! LocallyGeneratedModifier(genesis.blockTransactions)
      nodeViewRef ! LocallyGeneratedModifier(genesis.aDProofs.get)

      val block1 = validFullBlock(Some(genesis.header), wusAfterGenesis)

      nodeViewRef ! LocallyGeneratedModifier(block1.header)
      nodeViewRef ! LocallyGeneratedModifier(block1.aDProofs.get)
      nodeViewRef ! LocallyGeneratedModifier(block1.blockTransactions)
      nodeViewRef ! bestFullBlock(nodeViewConfig)
      expectMsg(Some(block1))

      nodeViewRef ! rootHash(nodeViewConfig)
      expectMsg(Algos.encode(block1.header.stateRoot))

      system.stop(nodeViewRef)
      val stateDir = new File(s"${nodeViewDir.getAbsolutePath}/state")
      for (file <- stateDir.listFiles) file.delete

      withRecoveredNodeViewRef { nodeViewRef2 =>
        nodeViewRef2 ! rootHash(nodeViewConfig)
        expectMsg(Algos.encode(block1.header.stateRoot))
      }
    }
  }

  val cases: List[TestCase] = List(t1, t2, t3, t4, t5, t6, t7, t8, t9, t10)

  allConfigs.foreach { c =>
    cases.foreach { t =>
      property(s"${t.name} - $c") {
        t.run(c)
      }
    }
  }
}