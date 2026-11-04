package org.ergoplatform.nodeView.viewholder

import akka.actor.{ActorRef, Props}
import akka.testkit.TestProbe
import org.ergoplatform.modifiers.{BlockSection, ErgoFullBlock}
import org.ergoplatform.modifiers.mempool.{ErgoTransaction, UnconfirmedTransaction}
import org.ergoplatform.nodeView.UtxoNodeViewHolder
import org.ergoplatform.nodeView.ErgoNodeViewHolder.ReceivableMessages.LocallyGeneratedTransaction
import org.ergoplatform.nodeView.mempool.ErgoMemPoolUtils.ProcessingOutcome
import org.ergoplatform.nodeView.state.{ErgoState, StateType}
import org.ergoplatform.nodeView.state.wrapped.WrappedUtxoState
import org.ergoplatform.settings.ErgoSettings
import org.ergoplatform.settings.Constants.TrueTree
import org.ergoplatform.utils.{ErgoCorePropertyTest, NodeViewTestConfig, NodeViewTestOps, TestCase}
import org.ergoplatform.utils.fixtures.NodeViewFixture
import scorex.util.ModifierId

import scala.collection.mutable
import scala.concurrent.duration._
import scala.util.Try

object AsyncMempoolUpdateOrderSpec {

  private val updateClassName = "org.ergoplatform.nodeView.ErgoNodeViewHolder$ReceivableMessages$MempoolUpdated"

  /** Whether the holder updates the pool off the block-application path (a `MempoolUpdated` message exists). */
  val asyncUpdate: Boolean = Try(Class.forName(updateClassName)).isSuccess

  case class Held(lastAppliedBlock: ModifierId)
  case class Release(lastAppliedBlock: ModifierId)
  case object Released

  /** The id of the last block a pool-update message carries as applied, if it is one and it applies a block. */
  private def heldKey(msg: Any): Option[ModifierId] = msg match {
    case p: Product if p.getClass.getName == updateClassName =>
      p.productElement(3) match {
        case applied: Seq[_] => applied.lastOption.collect { case b: BlockSection => b.id }
        case _ => None
      }
    case _ => None
  }

  /**
    * A UTXO holder that keeps each pool-update message (one per block application) until the test releases it, so
    * the order in which the updates reach the holder is chosen by the test, not by the dispatcher.
    */
  private[nodeView] class HoldingHolder(settings: ErgoSettings, monitor: ActorRef)
    extends UtxoNodeViewHolder(settings) {
    private val held = mutable.Map[ModifierId, Any]()

    override def receive: Receive = {
      val base = super.receive
      val control: Receive = {
        case msg if heldKey(msg).isDefined =>
          val key = heldKey(msg).get
          held(key) = msg
          monitor ! Held(key)
        case Release(key) =>
          held.remove(key).foreach(base)
          sender() ! Released
      }
      control orElse base
    }
  }
}

/**
  * A block's pool update runs after the block is applied. Several updates can be in flight at once; the pool must
  * end up the same whatever order they reach the holder in: no transaction of an applied block left in the pool,
  * transactions of rolled-back blocks back in it, unconfirmed transactions that arrived meanwhile kept.
  *
  * Each case applies blocks while their updates are held, then releases the updates in a given order. With a
  * synchronous update (no update message) nothing is held and the order is moot.
  */
class AsyncMempoolUpdateOrderSpec extends ErgoCorePropertyTest with NodeViewTestOps {
  import AsyncMempoolUpdateOrderSpec._
  import org.ergoplatform.utils.ErgoCoreTestConstants._
  import org.ergoplatform.utils.generators.ErgoNodeTransactionGenerators._
  import org.ergoplatform.utils.generators.ValidBlocksGenerators._

  private val utxoConfig = NodeViewTestConfig(StateType.Utxo, verifyTransactions = true, popowBootstrap = false)
  private val patience = 30.seconds

  private class Driver(fixture: NodeViewFixture) {
    import fixture._
    // the replacement opens the same directory: wait until the fixture's holder has stopped and closed its stores
    private val previous = fixture.nodeViewHolderRef
    private val watcher = TestProbe()(actorSystem)
    watcher.watch(previous)
    stopNodeViewHolder()
    watcher.expectTerminated(previous, patience)
    val monitor = TestProbe()(actorSystem)
    val control = TestProbe()(actorSystem)
    fixture.nodeViewHolderRef = actorSystem.actorOf(
      Props(classOf[HoldingHolder], fixture.settings, monitor.ref).withDispatcher("critical-dispatcher"))

    /** After a block application: its update is held (asynchronous holder) under the last applied block's id. */
    def expectHeld(lastApplied: ErgoFullBlock): Unit =
      if (asyncUpdate) monitor.expectMsg(patience, Held(lastApplied.id))

    def release(lastApplied: ErgoFullBlock): Unit =
      if (asyncUpdate) {
        control.send(nodeViewHolderRef, Release(lastApplied.id))
        control.expectMsg(patience, Released)
      }

    def submit(tx: ErgoTransaction): Unit = {
      control.send(nodeViewHolderRef, LocallyGeneratedTransaction(UnconfirmedTransaction(tx, None)))
      control.expectMsgType[ProcessingOutcome](patience) shouldBe a[ProcessingOutcome.Accepted]
    }

    def poolIds: Seq[ModifierId] = getCurrentView.pool.getAll.map(_.id)
  }

  private def genesisAndState(fixture: NodeViewFixture) = {
    val (us, bh) = createUtxoState(fixture.settings)
    val genesis = validFullBlock(parentOpt = None, us, bh)
    val wus = WrappedUtxoState(us, bh, fixture.settings).applyModifier(genesis)(_ => ()).get
    (us, bh, genesis, wus)
  }

  private def trueBoxes(wus: WrappedUtxoState) = wus.takeBoxes(100).filter(_.ergoTree == TrueTree)

  /** Two blocks in a row, B1 carrying t1 and B2 carrying t2 (both submitted first), u submitted after both. */
  private def twoBlocks(releaseOrder: String) =
    TestCase(s"two blocks applied before either update is delivered, delivered in order $releaseOrder") { fixture =>
      import fixture._
      val d = new Driver(fixture)
      val (_, _, genesis, wus0) = genesisAndState(fixture)
      applyBlock(genesis) shouldBe 'success
      d.expectHeld(genesis); d.release(genesis)

      val inputs = trueBoxes(wus0).take(3)
      inputs.size shouldBe 3
      val t1 = validTransactionFromBoxes(IndexedSeq(inputs(0)))
      val t2 = validTransactionFromBoxes(IndexedSeq(inputs(1)))
      val u = validTransactionFromBoxes(IndexedSeq(inputs(2)))
      d.submit(t1); d.submit(t2)

      val b1 = validFullBlock(Some(genesis), wus0, Seq(t1))
      val wus1 = wus0.applyModifier(b1)(_ => ()).get
      val b2 = validFullBlock(Some(b1), wus1, Seq(t2))
      applyBlock(b1) shouldBe 'success; d.expectHeld(b1)
      applyBlock(b2) shouldBe 'success; d.expectHeld(b2)
      getBestFullBlockOpt shouldBe Some(b2)
      d.submit(u)

      val order = if (releaseOrder == "B1,B2") Seq(b1, b2) else Seq(b2, b1)
      order.foreach(d.release)
      val ids = d.poolIds
      withClue(s"pool after delivery in order $releaseOrder: ") {
        ids should contain(u.id)
        ids should not contain t1.id
        ids should not contain t2.id
      }
    }

  /**
    * Chain 1 = A1 carrying t (t submitted first). Chain 2 forks at genesis: C1, C2 spend other boxes and outweigh
    * chain 1 when C2's body is applied (a switch: A1 rolled back, t valid on chain 2). With `confirmOnChain2`,
    * C3 on chain 2 then carries t. Updates: A1's, the switch's (applied C1, C2), and C3's.
    */
  private def chainSwitch(confirmOnChain2: Boolean, releaseOrder: String) = {
    val kind = if (confirmOnChain2) "t confirmed again in C3" else "t rolled back"
    TestCase(s"chain switch while A1's update is in flight ($kind), delivered in order $releaseOrder") { fixture =>
      import fixture._
      val d = new Driver(fixture)
      val (us1, bh1) = createUtxoState(fixture.settings)
      val (us2, bh2) = createUtxoState(fixture.settings)
      val genesis = validFullBlock(parentOpt = None, us1, bh1)
      val wus1 = WrappedUtxoState(us1, bh1, fixture.settings).applyModifier(genesis)(_ => ()).get
      var wus2 = WrappedUtxoState(us2, bh2, fixture.settings).applyModifier(genesis)(_ => ()).get
      applyBlock(genesis) shouldBe 'success
      d.expectHeld(genesis); d.release(genesis)

      val tInput = ErgoState.newBoxes(genesis.transactions).find(_.ergoTree == TrueTree).get
      val t: ErgoTransaction = validTransactionFromBoxes(IndexedSeq(tInput))
      d.submit(t)
      val a1 = validFullBlock(Some(genesis), wus1, Seq(t))
      applyBlock(a1) shouldBe 'success; d.expectHeld(a1)
      getBestFullBlockOpt shouldBe Some(a1)

      val chain2 = (1 to 2).foldLeft(Vector.empty[ErgoFullBlock]) { (acc, _) =>
        val input = trueBoxes(wus2).filterNot(_.id.sameElements(tInput.id)).head
        val block = validFullBlock(Some(acc.lastOption.getOrElse(genesis)), wus2, Seq(validTransactionFromBoxes(IndexedSeq(input))))
        wus2 = wus2.applyModifier(block)(_ => ()).get
        acc :+ block
      }
      val (c1, c2) = (chain2(0), chain2(1))
      chain2.foreach(b => applyHeader(b.header) shouldBe 'success)
      applyPayload(c1) shouldBe 'success
      applyPayload(c2) shouldBe 'success; d.expectHeld(c2)
      getBestFullBlockOpt shouldBe Some(c2)

      val c3Opt = if (confirmOnChain2) {
        val c3 = validFullBlock(Some(c2), wus2, Seq(t))
        applyBlock(c3) shouldBe 'success; d.expectHeld(c3)
        getBestFullBlockOpt shouldBe Some(c3)
        Some(c3)
      } else None

      val byName = Map("A1" -> a1, "C2" -> c2) ++ c3Opt.map(c3 => "C3" -> c3)
      releaseOrder.split(",").foreach(n => d.release(byName(n)))
      val ids = d.poolIds
      withClue(s"pool after delivery in order $releaseOrder ($kind): ") {
        if (confirmOnChain2) ids should not contain t.id else ids should contain(t.id)
      }
    }
  }

  /** The window itself: after a block's outcome is announced, its transaction stays in the pool until delivery. */
  private val window = TestCase("between a block's application and its update, the pool holds its transaction") {
    fixture =>
      import fixture._
      assume(asyncUpdate, "synchronous pool update: no window")
      val d = new Driver(fixture)
      val (_, _, genesis, wus0) = genesisAndState(fixture)
      applyBlock(genesis) shouldBe 'success
      d.expectHeld(genesis); d.release(genesis)
      val t1 = validTransactionFromBoxes(IndexedSeq(trueBoxes(wus0).head))
      d.submit(t1)
      val b1 = validFullBlock(Some(genesis), wus0, Seq(t1))
      applyBlock(b1) shouldBe 'success; d.expectHeld(b1)
      getBestFullBlockOpt shouldBe Some(b1)
      d.poolIds should contain(t1.id)
      d.release(b1)
      d.poolIds should not contain t1.id
  }

  private val cases = Seq(
    window,
    twoBlocks("B1,B2"),
    twoBlocks("B2,B1"),
    chainSwitch(confirmOnChain2 = false, "A1,C2"),
    chainSwitch(confirmOnChain2 = false, "C2,A1"),
    chainSwitch(confirmOnChain2 = true, "A1,C2,C3"),
    chainSwitch(confirmOnChain2 = true, "A1,C3,C2")
  )

  cases.foreach { tc =>
    property(s"${tc.name} - $utxoConfig") {
      tc.run(parameters, utxoConfig)
    }
  }
}
