package org.ergoplatform.nodeView

import java.io.File

import akka.actor.{ActorRef, ActorSystem, Props}
import akka.testkit.{ImplicitSender, TestKit}
import io.iohk.iodb.ByteArrayWrapper
import org.ergoplatform.modifiers.ErgoFullBlock
import org.ergoplatform.modifiers.history.{ADProofs, BlockTransactions, DefaultFakePowScheme, Header}
import org.ergoplatform.modifiers.mempool.AnyoneCanSpendTransaction
import org.ergoplatform.modifiers.mempool.proposition.AnyoneCanSpendProposition
import org.ergoplatform.nodeView.history.ErgoHistory
import org.ergoplatform.nodeView.mempool.ErgoMemPool
import org.ergoplatform.nodeView.state.{DigestState, ErgoState, UtxoState}
import org.ergoplatform.nodeView.wallet.ErgoWallet
import org.ergoplatform.settings.ErgoSettings
import org.ergoplatform.utils.ErgoGenerators
import org.scalatest.{BeforeAndAfterAll, Matchers, PropSpecLike}
import scorex.core.LocalInterface.{LocallyGeneratedModifier, LocallyGeneratedTransaction}
import scorex.core.NodeViewHolder.EventType._
import scorex.core.NodeViewHolder.{GetDataFromCurrentView, SyntacticallySuccessfulModifier}
import scorex.core.{ModifierId, NodeViewHolder}
import scorex.testkit.utils.FileUtils

class ErgoNodeViewHolderSpecification extends TestKit(ActorSystem("WithIsoFix")) with ImplicitSender with PropSpecLike with ErgoGenerators with Matchers with FileUtils with BeforeAndAfterAll {

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
  type D = DigestState
  type U = UtxoState
  type W = ErgoWallet
  type P = ErgoMemPool
  type C = NodeViewHolderConfig

  val allConfigs: List[NodeViewHolderConfig] = List(
    NodeViewHolderConfig(true, true, true),
    NodeViewHolderConfig(true, false, true),
    NodeViewHolderConfig(true, false, false),
    NodeViewHolderConfig(true, true, false),
    NodeViewHolderConfig(false, true, true),
    NodeViewHolderConfig(false, true, false),
  )

  def actorRef(c: NodeViewHolderConfig): ActorRef = {
    val dir: File = createTempDir
    val defaultSettings: ErgoSettings = ErgoSettings.read(None).copy(directory = dir.getAbsolutePath)
    val settings = defaultSettings.copy(
      nodeSettings = defaultSettings.nodeSettings.copy(
        ADState = c.adState,
        verifyTransactions = c.verifyTransactions,
        PoPoWBootstrap = c.popowBootstrap
      ),
      chainSettings = defaultSettings.chainSettings.copy(poWScheme = DefaultFakePowScheme)
    )
    if (c.adState) {
      system.actorOf(Props(classOf[DigestErgoNodeViewHolder], settings))
    } else {
      system.actorOf(Props(classOf[UtxoErgoNodeViewHolder], settings))
    }
  }

  def checkAfterGenesisState(c: C) = if (c.adState) {
    GetDataFromCurrentView[H, D, W, P, Boolean] { v =>
      v.state.rootHash.sameElements(ErgoState.afterGenesisStateDigest)
    }
  } else {
    GetDataFromCurrentView[H, U, W, P, Boolean] { v =>
      v.state.rootHash.sameElements(ErgoState.afterGenesisStateDigest)
    }
  }

  def bestHeaderOpt(c: C) = if (c.adState) {
    GetDataFromCurrentView[H, D, W, P, Option[Header]] { v =>
      v.history.bestHeaderOpt
    }
  } else {
    GetDataFromCurrentView[H, U, W, P, Option[Header]] { v =>
      v.history.bestHeaderOpt
    }
  }

  def historyHeight(c: C) = if (c.adState) {
    GetDataFromCurrentView[H, D, W, P, Int] { v =>
      v.history.height
    }
  } else {
    GetDataFromCurrentView[H, U, W, P, Int] { v =>
      v.history.height
    }
  }

  def heightOf(id: ModifierId, c: C) = if (c.adState) {
    GetDataFromCurrentView[H, D, W, P, Option[Int]] { v =>
      v.history.heightOf(id)
    }
  } else {
    GetDataFromCurrentView[H, U, W, P, Option[Int]] { v =>
      v.history.heightOf(id)
    }
  }

  def lastHeadersLength(count: Int, c: C) = if (c.adState) {
    GetDataFromCurrentView[H, D, W, P, Int] { v =>
      v.history.lastHeaders(count).size
    }
  } else {
    GetDataFromCurrentView[H, U, W, P, Int] { v =>
      v.history.lastHeaders(count).size
    }
  }

  def openSurfaces(c: C) = if (c.adState) {
    GetDataFromCurrentView[H, D, W, P, Seq[ByteArrayWrapper]] { v =>
      v.history.openSurfaceIds().map(ByteArrayWrapper.apply)
    }
  } else {
    GetDataFromCurrentView[H, U, W, P, Seq[ByteArrayWrapper]] { v =>
      v.history.openSurfaceIds().map(ByteArrayWrapper.apply)
    }
  }

  def bestFullBlock(c: C) = if (c.adState) {
    GetDataFromCurrentView[H, D, W, P, Option[ErgoFullBlock]] { v =>
      v.history.bestFullBlockOpt
    }
  } else {
    GetDataFromCurrentView[H, U, W, P, Option[ErgoFullBlock]] { v =>
      v.history.bestFullBlockOpt
    }
  }

  def bestFullBlockEncodedId(c: C) = if (c.adState) {
    GetDataFromCurrentView[H, D, W, P, Option[String]] { v =>
      v.history.bestFullBlockOpt.map(_.header.encodedId)
    }
  } else {
    GetDataFromCurrentView[H, U, W, P, Option[String]] { v =>
      v.history.bestFullBlockOpt.map(_.header.encodedId)
    }
  }

  def poolSize(c: C) = if (c.adState) {
    GetDataFromCurrentView[H, D, W, P, Int] { v =>
      v.pool.size
    }
  } else {
    GetDataFromCurrentView[H, U, W, P, Int] { v =>
      v.pool.size
    }
  }

  def rootHash(c: C) = if (c.adState) {
    GetDataFromCurrentView[H, D, W, P, ByteArrayWrapper] { v =>
      ByteArrayWrapper(v.state.rootHash)
    }
  } else {
    GetDataFromCurrentView[H, U, W, P, ByteArrayWrapper] { v =>
      ByteArrayWrapper(v.state.rootHash)
    }
  }


  class TestCase(val name: String, val test: (NodeViewHolderConfig, ActorRef) => Unit)

  val t1 = new TestCase("check genesis state", (c, a) => {
    val msg = checkAfterGenesisState(c)
    a ! msg
    expectMsg(true)
  })

  val t2 = new TestCase("check history after genesis", (c, a) => {
    a ! bestHeaderOpt(c)
    expectMsg(None)
  })

  val t3 = new TestCase("apply valid block header", (c, a) => {
    val dir = createTempDir
    val (us, bh) = ErgoState.generateGenesisUtxoState(dir)
    val block = validFullBlock(None, us, bh)

    a ! bestHeaderOpt(c)
    expectMsg(None)

    a ! historyHeight(c)
    expectMsg(-1)

        a ! NodeViewHolder.Subscribe(Seq(SuccessfulPersistentModifier, FailedPersistentModifier))

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
  })

  val t4 = new TestCase("apply valid block as genesis", (c, a) => {
    val dir = createTempDir
    val (us, bh) = ErgoState.generateGenesisUtxoState(dir)
    val genesis = validFullBlock(parentOpt = None, us, bh)

    a ! NodeViewHolder.Subscribe(Seq(SemanticallyFailedPersistentModifier, SuccessfulSemanticallyValidModifier,
      SyntacticallyFailedPersistentModifier, SuccessfulSyntacticallyValidModifier))

    a ! LocallyGeneratedModifier(genesis.header)
    expectMsgType[SyntacticallySuccessfulModifier[Header]]

    if (c.verifyTransactions) {
      a ! LocallyGeneratedModifier(genesis.blockTransactions)
      a ! LocallyGeneratedModifier(genesis.aDProofs.get)

      if (c.adState) {
        expectMsgType[SyntacticallySuccessfulModifier[ADProofs]]
      }
      else {
        expectMsgType[SyntacticallySuccessfulModifier[BlockTransactions]]
      }

      a ! bestFullBlock(c)
      expectMsg(Some(genesis))
    }
  })

  val t5 = new TestCase("apply full blocks after genesis", (c, a) => {
    val dir = createTempDir
    val (us, bh) = ErgoState.generateGenesisUtxoState(dir)
    val genesis = validFullBlock(parentOpt = None, us, bh)
    val wusAfterGenesis = WrappedUtxoState(us, bh).applyModifier(genesis).get

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
  })

  val t6 = new TestCase("add transaction to memory pool", (c, a) => {
    val dir = createTempDir
    val (us, bh) = ErgoState.generateGenesisUtxoState(dir)
    val genesis = validFullBlock(parentOpt = None, us, bh)
    val wusAfterGenesis = WrappedUtxoState(us, bh).applyModifier(genesis).get
    val toSpend = wusAfterGenesis.takeBoxes(1).head
    val tx = AnyoneCanSpendTransaction(IndexedSeq(toSpend.nonce), IndexedSeq(toSpend.value))

    a ! LocallyGeneratedTransaction[AnyoneCanSpendProposition.type, AnyoneCanSpendTransaction](tx)
    a ! poolSize(c)
    expectMsg(1)
  })

  val t7 = new TestCase("apply invalid full block", (c, a) => {
    val dir = createTempDir
    val (us, bh) = ErgoState.generateGenesisUtxoState(dir)
    val genesis = validFullBlock(parentOpt = None, us, bh)
    val wusAfterGenesis = WrappedUtxoState(us, bh).applyModifier(genesis).get

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
      expectMsg(ByteArrayWrapper(wusAfterBlock.rootHash))
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
  })

  val t8 = new TestCase("switching for a better chain", (c, a) => {
    val dir = createTempDir
    val (us, bh) = ErgoState.generateGenesisUtxoState(dir)
    val genesis = validFullBlock(parentOpt = None, us, bh)
    val wusAfterGenesis = WrappedUtxoState(us, bh).applyModifier(genesis).get

    a ! LocallyGeneratedModifier(genesis.header)
    if (c.verifyTransactions) {
      a ! LocallyGeneratedModifier(genesis.blockTransactions)
      a ! LocallyGeneratedModifier(genesis.aDProofs.get)
    }

    val chain1block1 = validFullBlock(Some(genesis.header), wusAfterGenesis)

    a ! LocallyGeneratedModifier(chain1block1.header)
    if (c.verifyTransactions) {
      a ! LocallyGeneratedModifier(chain1block1.blockTransactions)
      a ! LocallyGeneratedModifier(chain1block1.aDProofs.get)
    }

    a ! GetDataFromCurrentView[ErgoHistory, DigestState, ErgoWallet, ErgoMemPool, Option[ErgoFullBlock]] { v =>
      v.history.bestFullBlockOpt
    }
    expectMsg(Some(chain1block1))

    val chain2block1 = validFullBlock(Some(genesis.header), wusAfterGenesis)

    a ! LocallyGeneratedModifier(chain2block1.header)
    if (c.verifyTransactions) {
      a ! LocallyGeneratedModifier(chain2block1.blockTransactions)
      a ! LocallyGeneratedModifier(chain2block1.aDProofs.get)
    }

    a ! GetDataFromCurrentView[ErgoHistory, DigestState, ErgoWallet, ErgoMemPool, Option[ErgoFullBlock]] { v =>
      v.history.bestFullBlockOpt
    }
    expectMsg(Some(chain1block1))

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
  })

  //TODO: fix switcing for a better chain cases for all configs
  val cases = List(t1, t2, t3, t4, t5, t6, t7 /*, t8*/)

  allConfigs.foreach { c =>
    cases.foreach { t =>
      property(s"$c - ${t.name}") {
        val a = actorRef(c)
        t.test(c, a)
        system.stop(a)
      }
    }
  }
}