package org.ergoplatform.local

import akka.actor.{ActorRef, ActorSystem}
import akka.testkit.{TestActorRef, TestProbe}
import org.ergoplatform.{ErgoAddressEncoder, ErgoBox}
import org.ergoplatform.modifiers.mempool.{ErgoTransaction, UnconfirmedTransaction}
import org.ergoplatform.network.ErgoNodeViewSynchronizerMessages.{FailedTransaction, RecheckMempool, SuccessfulTransaction}
import org.ergoplatform.nodeView.ErgoNodeViewHolder.ReceivableMessages.{LocallyGeneratedTransaction, RecheckedTransactions}
import org.ergoplatform.nodeView.mempool.ErgoMemPoolUtils.ProcessingOutcome
import org.ergoplatform.nodeView.state.{ErgoState, UtxoState}
import org.ergoplatform.nodeView.state.wrapped.WrappedUtxoState
import org.ergoplatform.settings.{Algos, ErgoSettings}
import org.ergoplatform.utils.ErgoCoreTestConstants.parameters
import org.ergoplatform.utils.fixtures.NodeViewFixture
import org.ergoplatform.utils.{ErgoTestHelpers, MempoolTestHelpers, NodeViewTestOps, RandomWrapper}
import org.ergoplatform.network.message.InvData
import org.scalatest.flatspec.AnyFlatSpec
import scorex.core.network.NetworkController.ReceivableMessages.SendToNetwork
import sigma.ast.ErgoTree
import sigma.ast.syntax.ValueOps
import sigma.compiler.SigmaCompiler
import sigma.compiler.ir.CompiletimeIRContext
import sigma.serialization.ErgoTreeSerializer
import sigmastate.interpreter.Interpreter.emptyEnv
import org.ergoplatform.settings.Constants.TrueTree
import scorex.util.ModifierId

import scala.concurrent.duration._

class MempoolAuditorSpec extends AnyFlatSpec with NodeViewTestOps with ErgoTestHelpers with MempoolTestHelpers {
  import org.ergoplatform.utils.ErgoNodeTestConstants._
  import org.ergoplatform.utils.generators.ErgoNodeTransactionGenerators._
  import org.ergoplatform.utils.generators.ErgoCoreGenerators._
  import org.ergoplatform.utils.generators.ValidBlocksGenerators._

  val cleanupDuration: FiniteDuration = 200.millis
  val settingsToTest: ErgoSettings = settings.copy(
    nodeSettings = settings.nodeSettings.copy(
      mempoolCleanupDuration = cleanupDuration,
      rebroadcastCount = 1
    ))
  val fixture = new NodeViewFixture(settingsToTest, parameters)
  val newTx: Class[SuccessfulTransaction] = classOf[SuccessfulTransaction]

  it should "remove transactions which become invalid" in {
    import fixture._

    val testProbe = new TestProbe(actorSystem)
    actorSystem.eventStream.subscribe(testProbe.ref, newTx)

    val (us, bh) = createUtxoState(settingsToTest)
    val genesis = validFullBlock(parentOpt = None, us, bh)
    val wusAfterGenesis =
      WrappedUtxoState(us, bh, settingsToTest).applyModifier(genesis) { mod =>
        nodeViewHolderRef ! mod
      } .get

    applyBlock(genesis) shouldBe 'success
    getRootHash shouldBe Algos.encode(wusAfterGenesis.rootDigest)

    val boxes = ErgoState.newBoxes(genesis.transactions).find(_.ergoTree == TrueTree)
    boxes.nonEmpty shouldBe true

    val script = s"{sigmaProp(HEIGHT == ${genesis.height} + 1)}"
    val compiler = new SigmaCompiler(ErgoAddressEncoder.MainnetNetworkPrefix)
    val prop = compiler.compile(emptyEnv, script)(new CompiletimeIRContext).buildTree
    val tree = ErgoTree.fromProposition(prop.asSigmaProp)

    val bs = ErgoTreeSerializer.DefaultSerializer.serializeErgoTree(tree)
    ErgoTreeSerializer.DefaultSerializer.deserializeErgoTree(bs) shouldBe tree

    val validTx = validTransactionFromBoxes(boxes.toIndexedSeq, outputsProposition = tree)

    val temporarilyValidTx = validTransactionFromBoxes(validTx.outputs, outputsProposition = ErgoTree.fromProposition(proveDlogGen.sample.get))

    subscribeEvents(classOf[FailedTransaction])
    nodeViewHolderRef ! LocallyGeneratedTransaction(UnconfirmedTransaction(validTx, None))
    testProbe.expectMsgClass(cleanupDuration, newTx)
    expectMsgType[ProcessingOutcome.Accepted]

    nodeViewHolderRef ! LocallyGeneratedTransaction(UnconfirmedTransaction(temporarilyValidTx, None))
    testProbe.expectMsgClass(cleanupDuration, newTx)
    expectMsgType[ProcessingOutcome.Accepted]

    getPoolSize shouldBe 2

    val _: ActorRef = MempoolAuditorRef(nodeViewHolderRef, nodeViewHolderRef, settingsToTest)

    Thread.sleep(200) // give transactions in the pool enough time to become candidates for re-checking

    // include first transaction in the block
    val block = validFullBlock(Some(genesis), wusAfterGenesis, Seq(validTx))

    applyBlock(block) shouldBe 'success

    org.ergoplatform.utils.untilTimeout(cleanupDuration * 4, 100.millis) {
      // first tx removed from pool during node view update
      // another tx invalidated by `MempoolAuditor`
      getPoolSize shouldBe 0
    }
  }

  it should "rebroadcast transactions correctly" in {

    val (us0, bh0) = createUtxoState(settingsToTest)
    val (txs0, bh1) = validTransactionsFromBoxHolder(bh0)
    val b1 = validFullBlock(None, us0, txs0)

    val us = us0.applyModifier(b1, None)(_ => ()).get

    val bxs = bh1.boxes.values.toList.filter(_.proposition != genesisEmissionBox.proposition)
    val txs = validTransactionsFromBoxes(200000, bxs, new RandomWrapper)._1
      .map(tx => UnconfirmedTransaction(tx, None))

    implicit val system = ActorSystem()
    val probe = TestProbe()

    val auditor: ActorRef = TestActorRef(new MempoolAuditor(probe.ref, probe.ref, settingsToTest))


    auditor ! RecheckMempool(us, new FakeMempool(txs))

    probe.fishForMessage(3.seconds) {
      case _: SendToNetwork => true
      case _: RecheckedTransactions => false
    }.isInstanceOf[SendToNetwork] shouldBe true
  }

  // the UTXO state after one block, and its boxes other than the emission box
  private def stateAfterOneBlock(): (UtxoState, Seq[ErgoBox]) = {
    val (us0, bh0) = createUtxoState(settingsToTest)
    val (txs0, bh1) = validTransactionsFromBoxHolder(bh0)
    val b1 = validFullBlock(None, us0, txs0)
    val us = us0.applyModifier(b1, None)(_ => ()).get
    val bxs = bh1.boxes.values.toList.filter(_.proposition != genesisEmissionBox.proposition)
    bxs.filter(b => us.boxById(b.id).isEmpty) shouldBe empty
    us -> bxs
  }

  // a transaction spending all of `inputs` and reading `dataInputs`, with no other data inputs
  private def spending(inputs: Seq[ErgoBox], dataInputs: Seq[ErgoBox] = Seq.empty): ErgoTransaction =
    validTransactionFromBoxes(inputs.toIndexedSeq, dataBoxes = dataInputs.toIndexedSeq)

  // a chain of `length` transactions, each spending all the outputs of the one before, starting from `inputs`
  private def chainFrom(inputs: Seq[ErgoBox], length: Int): Seq[ErgoTransaction] =
    (1 until length).scanLeft(spending(inputs))((prev, _) => spending(prev.outputs))

  // runs one cleanup and rebroadcast round over `pool` and returns the announced ids in order (one `Inv` each)
  private def announcedIds(us: UtxoState, pool: Seq[ErgoTransaction], rebroadcastCount: Int): Seq[ModifierId] = {
    implicit val system: ActorSystem = ActorSystem()
    try {
      val probe = TestProbe()
      val roundSettings =
        settingsToTest.copy(nodeSettings = settingsToTest.nodeSettings.copy(rebroadcastCount = rebroadcastCount))
      val auditor: ActorRef = TestActorRef(new MempoolAuditor(probe.ref, probe.ref, roundSettings))
      auditor ! RecheckMempool(us, new FakeMempool(pool.map(UnconfirmedTransaction(_, None))))
      val announcements: PartialFunction[Any, Seq[ModifierId]] = {
        case SendToNetwork(msg, _) => msg.input.toOption.toSeq.flatMap {
          case inv: InvData => inv.ids
          case _ => Seq.empty
        }
        case _ => Seq.empty
      }
      // a round announces all its ids in one go, after the cleanup run, however long that takes: wait for the first
      // announcement, then take the rest
      val first = probe.fishForMessage(30.seconds) {
        case _: SendToNetwork => true
        case _ => false
      }
      announcements(first) ++ probe.receiveWhile(max = 10.seconds, idle = 1.second)(announcements).flatten
    } finally {
      system.terminate()
    }
  }

  it should "rebroadcast a pooled child together with its in-pool parent, parent first" in {
    val (us, bxs) = stateAfterOneBlock()
    val Seq(parent, child) = chainFrom(bxs.take(1), 2)

    // FakeMempool.random takes from the front: only the child is selected; its input is in neither the state nor
    // the selection, so without its parent it is not announced at all
    announcedIds(us, Seq(child, parent), rebroadcastCount = 1) shouldBe Seq(parent.id, child.id)
  }

  it should "not rebroadcast the descendants of an in-pool ancestor that is not announced" in {
    val (us, bxs) = stateAfterOneBlock()
    // a chain whose root's inputs are in neither the state nor the pool: missing -> root -> mid -> leaf
    val Seq(_, root, mid, leaf) = chainFrom(bxs.slice(2, 3), 4)
    root.inputs.filter(in => us.boxById(in.boxId).isDefined) shouldBe empty
    // a control chain whose root's inputs are in the state: parent -> child
    val Seq(parent, child) = chainFrom(bxs.take(2), 2)

    // FakeMempool.random takes from the front: the leaf and the child are selected
    announcedIds(us, Seq(leaf, child, mid, root, parent), rebroadcastCount = 2) shouldBe Seq(parent.id, child.id)
  }

  it should "stop a rebroadcast round after MaxAnnouncedPerRound announcements, keeping the chain's front" in {
    val (us, bxs) = stateAfterOneBlock()
    val chain = chainFrom(bxs.take(1), MempoolAuditor.MaxAnnouncedPerRound + 5)

    // only the leaf is selected; with its ancestors the round holds the whole chain, parents first
    announcedIds(us, chain.reverse, rebroadcastCount = 1) shouldBe chain.take(MempoolAuditor.MaxAnnouncedPerRound).map(_.id)
  }

  it should "not stop a rebroadcast round below the configured rebroadcastCount" in {
    val (us, bxs) = stateAfterOneBlock()
    val chain = chainFrom(bxs.take(1), MempoolAuditor.MaxAnnouncedPerRound + 5)

    // the whole chain is selected
    announcedIds(us, chain.reverse, rebroadcastCount = chain.size) shouldBe chain.map(_.id)
  }

  it should "rebroadcast a pooled transaction together with the in-pool transaction that creates its data input" in {
    val (us, bxs) = stateAfterOneBlock()
    val creator = spending(bxs.take(1))
    val reader = spending(bxs.slice(1, 2), dataInputs = creator.outputs.take(1))
    reader.inputs.filter(in => us.boxById(in.boxId).isEmpty) shouldBe empty

    // only the reader is selected; its inputs are in the state, its data input is created by the pooled creator
    announcedIds(us, Seq(reader, creator), rebroadcastCount = 1) shouldBe Seq(creator.id, reader.id)
  }

  it should "not rebroadcast a transaction whose data input's in-pool creator is not announced" in {
    val (us, bxs) = stateAfterOneBlock()
    // missing -> creator, where creator's inputs are in neither the state nor the pool; reader reads creator's output
    val Seq(_, creator) = chainFrom(bxs.slice(2, 3), 2)
    val reader = spending(bxs.slice(3, 4), dataInputs = creator.outputs.take(1))
    // a control chain whose root's inputs are in the state: parent -> child
    val Seq(parent, child) = chainFrom(bxs.take(2), 2)

    // the reader and the child are selected
    announcedIds(us, Seq(reader, child, creator, parent), rebroadcastCount = 2) shouldBe Seq(parent.id, child.id)
  }

  it should "add in-pool ancestors root first and keep a selection without in-pool parents as it is" in {
    val (_, bxs) = stateAfterOneBlock()
    val Seq(g, p, c) = chainFrom(bxs.take(1), 3).map(UnconfirmedTransaction(_, None))
    val pool = Seq(c, p, g)

    MempoolAuditor.withAncestorsParentsFirst(Seq(c), pool).map(_.id) shouldBe Seq(g.id, p.id, c.id)
    MempoolAuditor.withAncestorsParentsFirst(Seq(p, c), pool).map(_.id) shouldBe Seq(g.id, p.id, c.id)
    // no in-pool parent among the pool: unchanged
    MempoolAuditor.withAncestorsParentsFirst(Seq(g), Seq(g)).map(_.id) shouldBe Seq(g.id)
    MempoolAuditor.withAncestorsParentsFirst(Seq(c, g), Seq(c, g)).map(_.id) shouldBe Seq(c.id, g.id)
  }

}
