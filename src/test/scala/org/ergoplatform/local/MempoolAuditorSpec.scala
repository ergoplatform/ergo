package org.ergoplatform.local

import akka.actor.{ActorRef, ActorSystem}
import akka.testkit.{TestActorRef, TestProbe}
import org.ergoplatform.ErgoAddressEncoder
import org.ergoplatform.modifiers.mempool.UnconfirmedTransaction
import org.ergoplatform.network.ErgoNodeViewSynchronizerMessages.{FailedTransaction, RecheckMempool, SuccessfulTransaction}
import org.ergoplatform.nodeView.ErgoNodeViewHolder.ReceivableMessages.{LocallyGeneratedTransaction, RecheckedTransactions}
import org.ergoplatform.nodeView.mempool.ErgoMemPoolUtils.ProcessingOutcome
import org.ergoplatform.nodeView.state.ErgoState
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

  it should "rebroadcast a pooled child together with its in-pool parent, parent first" in {

    val (us0, bh0) = createUtxoState(settingsToTest)
    val (txs0, bh1) = validTransactionsFromBoxHolder(bh0)
    val b1 = validFullBlock(None, us0, txs0)

    val us = us0.applyModifier(b1, None)(_ => ()).get

    val bxs = bh1.boxes.values.toList.filter(_.proposition != genesisEmissionBox.proposition)
    val parent = validTransactionsFromBoxes(200000, bxs, new RandomWrapper)._1.head
    val child = validTransactionsFromBoxes(200000, parent.outputs, new RandomWrapper)._1.head
    child.inputs.exists(in => parent.outputs.exists(_.id sameElements in.boxId)) shouldBe true

    implicit val system = ActorSystem()
    val probe = TestProbe()

    val auditor: ActorRef = TestActorRef(new MempoolAuditor(probe.ref, probe.ref, settingsToTest))

    // rebroadcastCount = 1 and FakeMempool.random takes from the front: only the child is sampled; its input is
    // in neither the state nor the selection, so without its parent it is not announced at all
    val pool = new FakeMempool(Seq(UnconfirmedTransaction(child, None), UnconfirmedTransaction(parent, None)))
    auditor ! RecheckMempool(us, pool)

    val announced = probe.receiveWhile(max = 5.seconds, idle = 2.seconds) {
      case SendToNetwork(msg, _) => msg.input.toOption.toSeq.flatMap {
        case inv: InvData => inv.ids
        case _ => Seq.empty
      }
      case _ => Seq.empty
    }.flatten

    announced shouldBe Seq(parent.id, child.id)
  }

  it should "add in-pool ancestors root first and keep a selection without in-pool parents as it is" in {
    val (_, bh0) = createUtxoState(settingsToTest)
    val bxs = bh0.boxes.values.toList.filter(_.proposition != genesisEmissionBox.proposition)
    val grand = validTransactionsFromBoxes(200000, bxs, new RandomWrapper)._1.head
    val parent = validTransactionsFromBoxes(200000, grand.outputs, new RandomWrapper)._1.head
    val child = validTransactionsFromBoxes(200000, parent.outputs, new RandomWrapper)._1.head
    val g = UnconfirmedTransaction(grand, None)
    val p = UnconfirmedTransaction(parent, None)
    val c = UnconfirmedTransaction(child, None)
    val pool = Seq(c, p, g)

    MempoolAuditor.withAncestorsParentsFirst(Seq(c), pool).map(_.id) shouldBe Seq(g.id, p.id, c.id)
    MempoolAuditor.withAncestorsParentsFirst(Seq(p, c), pool).map(_.id) shouldBe Seq(g.id, p.id, c.id)
    // no in-pool parent among the pool: unchanged
    MempoolAuditor.withAncestorsParentsFirst(Seq(g), Seq(g)).map(_.id) shouldBe Seq(g.id)
    MempoolAuditor.withAncestorsParentsFirst(Seq(c, g), Seq(c, g)).map(_.id) shouldBe Seq(c.id, g.id)
  }

}
