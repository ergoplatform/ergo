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

  case class TestCase(name: String)(test: ActorFixture => Unit) {
    def run(c: NodeViewHolderConfig): Unit = {
      val a = new ActorFixture(c)
      try test(a) finally a.stop()
    }
  }

  class ActorFixture(val config: NodeViewHolderConfig) {
    val probe = new TestProbe(system)
    val ref = actorRef(config)
    implicit val defaultSender: ActorRef = probe.testActor
    @inline def c = config
    @inline def send(msg: Any): Unit = probe.send(ref, msg)
    @inline def !(msg: Any) = send(msg)
    @inline def defaultTimeout = probe.remainingOrDefault
    @inline def expectMsg[T](obj: T) = probe.expectMsg(obj)
    @inline def expectMsgType[T](implicit t: ClassTag[T]) = probe.expectMsgType
    @inline def expectNoMsg = probe.expectNoMessage(defaultTimeout)
    def stop() = { system.stop(ref); system.stop(probe.ref) }
  }

  val t1 = TestCase("check genesis state") { a =>
    import a._
    a ! checkAfterGenesisState(c)
    expectMsg(true)
  }

  val t2 = TestCase("check history after genesis") { a =>
    import a._
    a ! bestHeaderOpt(c)
    expectMsg(None)
  }

  val t3 = TestCase("apply valid block header") { a =>
    import a._
    val dir = createTempDir
    val (us, bh) = ErgoState.generateGenesisUtxoState(dir, Some(ref))
    val block = validFullBlock(None, us, bh)

    a ! bestHeaderOpt(c)
    expectMsg(None)

    a ! historyHeight(c)
    expectMsg(-1)

    a ! NodeViewHolder.Subscribe(Seq(SuccessfulSyntacticallyValidModifier))

    //sending header
    a ! LocallyGeneratedModifier[Header](block.header)
    expectMsgType[SyntacticallySuccessfulModifier[Header]]

    a ! historyHeight(c)
    expectMsg(0)

    a ! heightOf(block.header.id, c)
    expectMsg(Some(0))

    a ! lastHeadersLength(10, c)
    expectMsg(1)

    a ! openSurfaces(c)
    expectMsg(Seq(ByteArrayWrapper(block.header.id)))

    a ! bestHeaderOpt(c)
    expectMsg(Some(block.header))
  }

  val t4 = TestCase("apply valid block as genesis") { a =>
    import a._
    val dir = createTempDir
    val (us, bh) = ErgoState.generateGenesisUtxoState(dir, Some(ref))
    val genesis = validFullBlock(parentOpt = None, us, bh)

    a ! NodeViewHolder.Subscribe(Seq(SuccessfulSyntacticallyValidModifier))

    a ! LocallyGeneratedModifier(genesis.header)
    expectMsgType[SyntacticallySuccessfulModifier[Header]]

    if (c.verifyTransactions) {
      a ! LocallyGeneratedModifier(genesis.blockTransactions)
      a ! LocallyGeneratedModifier(genesis.aDProofs.get)

      if (c.adState) {
        expectMsgType[SyntacticallySuccessfulModifier[ADProofs]]
        expectMsgType[SyntacticallySuccessfulModifier[ADProofs]]
      }
      else {
        expectMsgType[SyntacticallySuccessfulModifier[BlockTransactions]]
        expectMsgType[SyntacticallySuccessfulModifier[BlockTransactions]]
      }

      a ! bestFullBlock(c)
      expectMsg(Some(genesis))
    }
  }

  val t5 = TestCase("apply full blocks after genesis") { a =>
    import a._
    val dir = createTempDir
    val (us, bh) = ErgoState.generateGenesisUtxoState(dir, Some(ref))
    val genesis = validFullBlock(parentOpt = None, us, bh)
    val wusAfterGenesis = WrappedUtxoState(us, bh, None).applyModifier(genesis).get

    a ! LocallyGeneratedModifier(genesis.header)
    if (c.verifyTransactions) {
      a ! LocallyGeneratedModifier(genesis.blockTransactions)
      a ! LocallyGeneratedModifier(genesis.aDProofs.get)
    }

    val block = validFullBlock(Some(genesis.header), wusAfterGenesis)

    a ! LocallyGeneratedModifier(block.header)
    if (c.verifyTransactions) {
      a ! LocallyGeneratedModifier(block.blockTransactions)
      a ! LocallyGeneratedModifier(block.aDProofs.get)
      a ! bestFullBlock(c)
      expectMsg(Some(block))
    }

    a ! bestHeaderOpt(c)
    expectMsg(Some(block.header))

    a ! historyHeight(c)
    expectMsg(1)

    a ! lastHeadersLength(10, c)
    expectMsg(2)
  }

  val t6 = TestCase("add transaction to memory pool") { a =>
    import a._
    val tx = AnyoneCanSpendTransaction(IndexedSeq.empty[Long], IndexedSeq.empty[Long])

    a ! NodeViewHolder.Subscribe(Seq(FailedTransaction))
    a ! LocallyGeneratedTransaction[AnyoneCanSpendProposition.type, AnyoneCanSpendTransaction](tx)
    expectNoMsg
    a ! poolSize(c)
    expectMsg(1)
  }

  val t7 = TestCase("apply invalid full block") { a =>
    import a._
    val dir = createTempDir
    val (us, bh) = ErgoState.generateGenesisUtxoState(dir, Some(ref))
    val genesis = validFullBlock(parentOpt = None, us, bh)
    val wusAfterGenesis = WrappedUtxoState(us, bh, None).applyModifier(genesis).get

    a ! LocallyGeneratedModifier(genesis.header)
    if (c.verifyTransactions) {
      a ! LocallyGeneratedModifier(genesis.blockTransactions)
      a ! LocallyGeneratedModifier(genesis.aDProofs.get)
    }

    val block = validFullBlock(Some(genesis.header), wusAfterGenesis)
    val wusAfterBlock = wusAfterGenesis.applyModifier(block).get

    a ! LocallyGeneratedModifier(block.header)
    if (c.verifyTransactions) {
      a ! LocallyGeneratedModifier(block.blockTransactions)
      a ! LocallyGeneratedModifier(block.aDProofs.get)
      a ! rootHash(c)
      expectMsg(Algos.encode(wusAfterBlock.rootHash))
    }

    a ! bestHeaderOpt(c)
    expectMsg(Some(block.header))

    val brokenBlock = validFullBlock(Some(block.header), wusAfterBlock)

    a ! LocallyGeneratedModifier(brokenBlock.header)

    val brokenTransactions = brokenBlock.blockTransactions.copy(txs = brokenBlock.blockTransactions.txs.tail)
    if (c.verifyTransactions) {
      a ! LocallyGeneratedModifier(brokenTransactions)
      a ! LocallyGeneratedModifier(brokenBlock.aDProofs.get)
      a ! bestFullBlock(c)
      expectMsg(Some(block))
    }

    a ! bestHeaderOpt(c)
    //TODO Note and verify!
    expectMsg(Some(brokenBlock.header))
  }

  val t8 = TestCase("switching for a better chain") { a =>
    import a._
    val dir = createTempDir
    val (us, bh) = ErgoState.generateGenesisUtxoState(dir, Some(ref))
    val genesis = validFullBlock(parentOpt = None, us, bh)
    val wusAfterGenesis = WrappedUtxoState(us, bh, None).applyModifier(genesis).get

    a ! LocallyGeneratedModifier(genesis.header)
    if (c.verifyTransactions) {
      a ! LocallyGeneratedModifier(genesis.blockTransactions)
      a ! LocallyGeneratedModifier(genesis.aDProofs.get)
    }

    a ! rootHash(c)
    expectMsg(Algos.encode(wusAfterGenesis.rootHash))

    val chain1block1 = validFullBlock(Some(genesis.header), wusAfterGenesis)

    a ! LocallyGeneratedModifier(chain1block1.header)
    if (c.verifyTransactions) {
      a ! LocallyGeneratedModifier(chain1block1.blockTransactions)
      a ! LocallyGeneratedModifier(chain1block1.aDProofs.get)
    }

    a ! GetDataFromCurrentView[ErgoHistory, DigestState, ErgoWallet, ErgoMemPool, Option[ErgoFullBlock]] { v =>
      v.history.bestFullBlockOpt
    }
    if (c.verifyTransactions) expectMsg(Some(chain1block1)) else expectMsg(None)
    a ! bestHeaderOpt(c)
    expectMsg(Some(chain1block1.header))

    val chain2block1 = validFullBlock(Some(genesis.header), wusAfterGenesis)

    a ! LocallyGeneratedModifier(chain2block1.header)
    if (c.verifyTransactions) {
      a ! LocallyGeneratedModifier(chain2block1.blockTransactions)
      a ! LocallyGeneratedModifier(chain2block1.aDProofs.get)
    }

    a ! GetDataFromCurrentView[ErgoHistory, DigestState, ErgoWallet, ErgoMemPool, Option[ErgoFullBlock]] { v =>
      v.history.bestFullBlockOpt
    }
    if (c.verifyTransactions) expectMsg(Some(chain1block1)) else expectMsg(None)
    a ! bestHeaderOpt(c)
    expectMsg(Some(chain1block1.header))

    val wusChain2Block1 = wusAfterGenesis.applyModifier(chain2block1).get
    val chain2block2 = validFullBlock(Some(chain2block1.header), wusChain2Block1)

    chain2block1.header.stateRoot shouldEqual wusChain2Block1.rootHash

    a ! LocallyGeneratedModifier(chain2block2.header)
    if (c.verifyTransactions) {
      a ! LocallyGeneratedModifier(chain2block2.blockTransactions)
      a ! LocallyGeneratedModifier(chain2block2.aDProofs.get)
      a ! bestFullBlockEncodedId(c)
      expectMsg(Some(chain2block2.header.encodedId))
    }

    a ! bestHeaderOpt(c)
    expectMsg(Some(chain2block2.header))

    a ! rootHash(c)
    expectMsg(Algos.encode(chain2block2.header.stateRoot))

  }

  val t9 = TestCase("UTXO state should generate ADProofs and put them in history") { a =>
    import a._
    if (!c.adState) {
      val dir = createTempDir
      val (us, bh) = ErgoState.generateGenesisUtxoState(dir, Some(ref))
      val genesis = validFullBlock(parentOpt = None, us, bh)

      a ! LocallyGeneratedModifier(genesis.header)

      a ! LocallyGeneratedModifier(genesis.blockTransactions)

      a ! bestFullBlock(c)
      expectMsg(Some(genesis))

      a ! modifierById(genesis.aDProofs.get.id)
      expectMsg(genesis.aDProofs)
    }
  }

  val t10 = TestCase("NodeViewHolder start from inconsistent state") { a =>
    import a._
    if (c.verifyTransactions) {

      val nodeViewDir = Some(createTempDir)
      val a = actorRef(c, nodeViewDir)

      val dir = createTempDir
      val (us, bh) = ErgoState.generateGenesisUtxoState(dir, Some(a))
      val genesis = validFullBlock(parentOpt = None, us, bh)
      val wusAfterGenesis = WrappedUtxoState(us, bh, None).applyModifier(genesis).get

      a ! LocallyGeneratedModifier(genesis.header)
      a ! LocallyGeneratedModifier(genesis.blockTransactions)
      a ! LocallyGeneratedModifier(genesis.aDProofs.get)

      val block1 = validFullBlock(Some(genesis.header), wusAfterGenesis)

      a ! LocallyGeneratedModifier(block1.header)
      a ! LocallyGeneratedModifier(block1.aDProofs.get)
      a ! LocallyGeneratedModifier(block1.blockTransactions)
      a ! bestFullBlock(c)
      expectMsg(Some(block1))

      a ! rootHash(c)
      expectMsg(Algos.encode(block1.header.stateRoot))

      system.stop(a)
      val stateDir = new File(s"${nodeViewDir.get.getAbsolutePath}/state")
      for (file <- stateDir.listFiles) file.delete

      val a2 = actorRef(c, nodeViewDir)
      a2 ! rootHash(c)
      expectMsg(Algos.encode(block1.header.stateRoot))
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