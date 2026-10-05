package org.ergoplatform.network

import akka.actor.{ActorRef, ActorSystem, Props}
import akka.testkit.TestProbe
import org.ergoplatform.modifiers.mempool.ErgoTransaction
import org.ergoplatform.modifiers.mempool.UnconfirmedTransaction
import org.ergoplatform.network.ErgoNodeViewSynchronizerMessages._
import org.ergoplatform.network.message.{
  InvData,
  InvSpec,
  Message,
  ModifiersData,
  ModifiersSpec,
  RequestModifierSpec
}
import org.ergoplatform.network.peer.PeerInfo
import org.ergoplatform.network.peer.PenaltyType
import org.ergoplatform.nodeView.ErgoNodeViewHolder.ReceivableMessages.{
  ChainIsStuck,
  GetNodeViewChanges,
  TransactionFromRemote
}
import org.ergoplatform.nodeView.history.ErgoSyncInfoMessageSpec
import org.ergoplatform.nodeView.mempool.ErgoMemPool
import org.ergoplatform.nodeView.state.StateType
import org.ergoplatform.utils.ErgoNodeTestConstants._
import org.ergoplatform.utils.HistoryTestHelpers.generateHistory
import org.ergoplatform.utils.generators.ChainGenerator._
import org.ergoplatform.utils.generators.ErgoNodeTransactionGenerators._
import org.scalatest.matchers.should.Matchers
import org.scalatest.propspec.AnyPropSpec
import scorex.core.network.NetworkController.ReceivableMessages.{PenalizePeer, SendToNetwork}
import scorex.core.network.{ConnectedPeer, ConnectionId, DeliveryTracker, ModifiersStatus, Outgoing, SendToPeer}
import scorex.testkit.utils.AkkaFixture
import scorex.util.ModifierId

import java.net.InetSocketAddress
import scala.concurrent.duration._
import scala.concurrent.{Await, ExecutionContextExecutor}

class MempoolInflightBudgetSpec extends AnyPropSpec with Matchers {

  private def peer(port: Int, handler: ActorRef): ConnectedPeer =
    ConnectedPeer(
      ConnectionId(
        new InetSocketAddress("127.0.0.1", port),
        new InetSocketAddress("127.0.0.1", 19000),
        Outgoing
      ),
      handler,
      Some(PeerInfo(defaultPeerSpec, System.currentTimeMillis()))
    )

  private def sendInv(sender: TestProbe,
                      synchronizer: ActorRef,
                      from: ConnectedPeer,
                      ids: Seq[ModifierId]): Unit = {
    val inv = InvData(ErgoTransaction.modifierTypeId, ids)
    sender.send(synchronizer, Message(InvSpec, Left(InvSpec.toBytes(inv)), Some(from)))
  }

  private def expectRequest(networkProbe: TestProbe, ids: Seq[ModifierId]): Unit = {
    val request = networkProbe.fishForMessage(3.seconds) {
      case send: SendToNetwork =>
        send.message.spec.messageCode == RequestModifierSpec.messageCode
      case _ => false
    }.asInstanceOf[SendToNetwork]
    request.message.data.get.asInstanceOf[InvData].ids.toSet shouldBe ids.toSet
  }

  property("in-flight transactions respect the interblock cost budget") {
    val fixture = new AkkaFixture
    implicit val system: ActorSystem = fixture.system
    implicit val ec: ExecutionContextExecutor = system.dispatcher

    try {
      val history = generateHistory(
        verifyTransactions = true,
        StateType.Utxo,
        PoPoWBootstrap = false,
        blocksToKeep = -1
      )
      val pool = ErgoMemPool.empty(settings)
      val syncTracker = ErgoSyncTracker(settings.scorexSettings.network)
      val deliveryTracker = DeliveryTracker.empty(settings)
      val networkProbe = TestProbe()
      val viewHolderProbe = TestProbe()
      val peerHandlerProbe = TestProbe()
      val peers = (1 to 3).map(i => peer(20000 + i, peerHandlerProbe.ref))
      val txGen = validErgoTransactionGenTemplate(0, 0, maxInputs = 1)
      val transactions = Vector.fill(18)(txGen.sample.get._2)

      history.headersHeight shouldBe 0
      history.fullBlockHeight shouldBe 0
      syncTracker.maxHeight() shouldBe None
      transactions.map(_.id).distinct.size shouldBe 18
      transactions.forall { tx =>
        tx.bytes.length <= settings.nodeSettings.maxTransactionSize
      } shouldBe true

      val synchronizer = system.actorOf(Props(new ErgoNodeViewSynchronizer(
        networkProbe.ref,
        viewHolderProbe.ref,
        ErgoSyncInfoMessageSpec,
        settings,
        syncTracker,
        deliveryTracker
      )))
      viewHolderProbe.expectMsgType[GetNodeViewChanges]
      viewHolderProbe.send(synchronizer, ChangedHistory(history))
      viewHolderProbe.send(synchronizer, ChangedMempool(pool))

      val groups = transactions.grouped(6).toVector
      sendInv(viewHolderProbe, synchronizer, peers(0), groups(0).map(_.id))
      expectRequest(networkProbe, groups(0).take(5).map(_.id))
      sendInv(viewHolderProbe, synchronizer, peers(1), groups(1).map(_.id))
      expectRequest(networkProbe, groups(1).take(1).map(_.id))
      sendInv(viewHolderProbe, synchronizer, peers(2), groups(2).map(_.id))
      networkProbe.expectNoMessage(150.millis)

      viewHolderProbe.expectNoMessage(100.millis)
      val requested = groups(0).take(5).map(_ -> peers(0)) ++
        groups(1).take(1).map(_ -> peers(1))
      requested.foreach { case (tx, from) =>
        val data = ModifiersData(ErgoTransaction.modifierTypeId, Map(tx.id -> tx.bytes))
        viewHolderProbe.send(
          synchronizer,
          Message(ModifiersSpec, Left(ModifiersSpec.toBytes(data)), Some(from))
        )
      }

      val forwarded = viewHolderProbe.receiveN(6, 5.seconds).collect {
        case TransactionFromRemote(unconfirmedTx) => unconfirmedTx
      }
      forwarded.map(_.id).toSet shouldBe requested.map(_._1.id).toSet
      forwarded.size shouldBe 6
      viewHolderProbe.expectNoMessage(100.millis)

      // Completed work frees capacity without waiting for a new block.
      forwarded.take(2).foreach { utx =>
        viewHolderProbe.send(synchronizer, DeclinedTransaction(utx.withCost(1000)))
      }
      expectRequest(networkProbe, Seq(groups(0)(5).id))
      sendInv(viewHolderProbe, synchronizer, peers(2), groups(2).map(_.id))
      networkProbe.expectNoMessage(150.millis)
    } finally {
      Await.result(system.terminate(), Duration.Inf)
    }
  }

  property("a stale timeout cannot release a retry or a cross-peer validation") {
    val fixture = new AkkaFixture
    implicit val system: ActorSystem = fixture.system
    implicit val ec: ExecutionContextExecutor = system.dispatcher

    try {
      val history = generateHistory(
        verifyTransactions = true,
        StateType.Utxo,
        PoPoWBootstrap = false,
        blocksToKeep = -1
      )
      val pool = ErgoMemPool.empty(settings)
      val tracker = DeliveryTracker.empty(settings)
      val networkProbe = TestProbe()
      val viewHolderProbe = TestProbe()
      val handlerProbe = TestProbe()
      val peers = (1 to 3).map(i => peer(21000 + i, handlerProbe.ref))
      val txGen = validErgoTransactionGenTemplate(0, 0, maxInputs = 1)
      val transactions = Vector.fill(12)(txGen.sample.get._2)
      transactions.map(_.id).distinct.size shouldBe transactions.size

      val synchronizer = system.actorOf(Props(new ErgoNodeViewSynchronizer(
        networkProbe.ref,
        viewHolderProbe.ref,
        ErgoSyncInfoMessageSpec,
        settings,
        ErgoSyncTracker(settings.scorexSettings.network),
        tracker
      )))
      viewHolderProbe.expectMsgType[GetNodeViewChanges]
      viewHolderProbe.send(synchronizer, ChangedHistory(history))
      viewHolderProbe.send(synchronizer, ChangedMempool(pool))

      val txType = ErgoTransaction.modifierTypeId
      val target = transactions.head
      sendInv(viewHolderProbe, synchronizer, peers(0), Seq(target.id))
      expectRequest(networkProbe, Seq(target.id))
      sendInv(viewHolderProbe, synchronizer, peers(1), transactions.slice(1, 6).map(_.id))
      expectRequest(networkProbe, transactions.slice(1, 6).map(_.id))
      // A block clears completed costs, but six pending reservations must remain.
      val header = genHeaderChain(1, history, None, false).last
      viewHolderProbe.send(synchronizer, LocalBlockApplied(header, Seq.empty))
      val firstAttempt = tracker.getRequestedInfo(txType, target.id).get.requestId
      val otherAttempt = tracker.getRequestedInfo(txType, transactions(1).id).get.requestId

      viewHolderProbe.send(synchronizer, CheckDelivery(peers(0), txType, target.id, firstAttempt))
      sendInv(viewHolderProbe, synchronizer, peers(0), Seq(target.id))
      expectRequest(networkProbe, Seq(target.id))
      sendInv(viewHolderProbe, synchronizer, peers(2), transactions.drop(6).map(_.id))
      networkProbe.expectNoMessage(150.millis)

      viewHolderProbe.send(synchronizer, CheckDelivery(peers(0), txType, target.id, firstAttempt))
      val targetData = ModifiersData(txType, Map(target.id -> target.bytes))
      viewHolderProbe.send(synchronizer,
        Message(ModifiersSpec, Left(ModifiersSpec.toBytes(targetData)), Some(peers(1))))
      // The responder already owns five reservations, so this transfer is deferred.
      sendInv(viewHolderProbe, synchronizer, peers(2), transactions.drop(6).map(_.id))
      networkProbe.expectNoMessage(150.millis)
      viewHolderProbe.expectNoMessage(100.millis)
      val secondAttempt = tracker.getRequestedInfo(txType, target.id).get.requestId
      secondAttempt should not be firstAttempt
      tracker.getRequestedInfo(txType, target.id).get.requestId shouldBe secondAttempt

      viewHolderProbe.send(synchronizer,
        CheckDelivery(peers(1), txType, transactions(1).id, otherAttempt))
      expectRequest(networkProbe, Seq(transactions(6).id))
      viewHolderProbe.send(synchronizer,
        Message(ModifiersSpec, Left(ModifiersSpec.toBytes(targetData)), Some(peers(1))))
      val forwarded = viewHolderProbe.expectMsgType[TransactionFromRemote]
      forwarded.unconfirmedTx.id shouldBe target.id
      forwarded.unconfirmedTx.source shouldBe Some(peers(1))
      tracker.status(target.id, txType, Seq.empty) shouldBe ModifiersStatus.Received

      // The active delivery timer cannot release a transaction already under validation.
      viewHolderProbe.send(synchronizer, CheckDelivery(peers(0), txType, target.id, secondAttempt))
      sendInv(viewHolderProbe, synchronizer, peers(2), transactions.drop(6).map(_.id))
      networkProbe.expectNoMessage(150.millis)
    } finally {
      Await.result(system.terminate(), Duration.Inf)
    }
  }

  property("a stuck-chain reset releases requests but keeps received validation through local acceptance") {
    val fixture = new AkkaFixture
    implicit val system: ActorSystem = fixture.system
    implicit val ec: ExecutionContextExecutor = system.dispatcher

    try {
      val emptyHistory = generateHistory(
        verifyTransactions = true,
        StateType.Utxo,
        PoPoWBootstrap = false,
        blocksToKeep = -1
      )
      val history = applyChain(emptyHistory, genChain(1))
      history.fullBlockHeight shouldBe 1
      val tracker = DeliveryTracker.empty(settings)
      val networkProbe = TestProbe()
      val viewHolderProbe = TestProbe()
      val handlerProbe = TestProbe()
      val peers = (1 to 3).map(i => peer(22000 + i, handlerProbe.ref))
      val txGen = validErgoTransactionGenTemplate(0, 0, maxInputs = 1)
      val transactions = Vector.fill(8)(txGen.sample.get._2)
      transactions.map(_.id).distinct.size shouldBe transactions.size

      val synchronizer = system.actorOf(Props(new ErgoNodeViewSynchronizer(
        networkProbe.ref,
        viewHolderProbe.ref,
        ErgoSyncInfoMessageSpec,
        settings,
        ErgoSyncTracker(settings.scorexSettings.network),
        tracker
      )))
      viewHolderProbe.expectMsgType[GetNodeViewChanges]
      viewHolderProbe.send(synchronizer, ChangedHistory(history))
      viewHolderProbe.send(synchronizer, ChangedMempool(ErgoMemPool.empty(settings)))

      val received = transactions.head
      val requested = transactions(1)
      sendInv(viewHolderProbe, synchronizer, peers(0), Seq(received.id))
      expectRequest(networkProbe, Seq(received.id))
      sendInv(viewHolderProbe, synchronizer, peers(1), Seq(requested.id))
      expectRequest(networkProbe, Seq(requested.id))
      val data = ModifiersData(ErgoTransaction.modifierTypeId, Map(received.id -> received.bytes))
      viewHolderProbe.send(synchronizer,
        Message(ModifiersSpec, Left(ModifiersSpec.toBytes(data)), Some(peers(0))))
      val pending = viewHolderProbe.expectMsgType[TransactionFromRemote].unconfirmedTx

      viewHolderProbe.send(synchronizer, ChainIsStuck("test reset"))
      // An independent local acceptance of the same ID cannot complete the remote validation.
      viewHolderProbe.send(synchronizer,
        SuccessfulTransaction(UnconfirmedTransaction(received, None).withCost(1000)))
      sendInv(viewHolderProbe, synchronizer, peers(2), transactions.drop(2).map(_.id))
      expectRequest(networkProbe, transactions.slice(2, 6).map(_.id))
      tracker.status(requested.id, ErgoTransaction.modifierTypeId, Seq.empty) shouldBe
        ModifiersStatus.Unknown

      viewHolderProbe.send(synchronizer, DeclinedTransaction(pending.withCost(1000)))
      sendInv(viewHolderProbe, synchronizer, peers(2), transactions.drop(6).map(_.id))
      expectRequest(networkProbe, Seq(transactions(6).id))
    } finally {
      Await.result(system.terminate(), Duration.Inf)
    }
  }

  property("a configured transaction cost above the peer budget still permits one in-flight request") {
    val fixture = new AkkaFixture
    implicit val system: ActorSystem = fixture.system
    implicit val ec: ExecutionContextExecutor = system.dispatcher

    try {
      val costlySettings = settings.copy(
        nodeSettings = settings.nodeSettings.copy(maxTransactionCost = 11000000)
      )
      val history = generateHistory(
        verifyTransactions = true,
        StateType.Utxo,
        PoPoWBootstrap = false,
        blocksToKeep = -1
      )
      val networkProbe = TestProbe()
      val viewHolderProbe = TestProbe()
      val handlerProbe = TestProbe()
      val peers = (1 to 2).map(i => peer(23000 + i, handlerProbe.ref))
      val txGen = validErgoTransactionGenTemplate(0, 0, maxInputs = 1)
      val transactions = Vector.fill(5)(txGen.sample.get._2)
      transactions.map(_.id).distinct.size shouldBe transactions.size

      val synchronizer = system.actorOf(Props(new ErgoNodeViewSynchronizer(
        networkProbe.ref,
        viewHolderProbe.ref,
        ErgoSyncInfoMessageSpec,
        costlySettings,
        ErgoSyncTracker(costlySettings.scorexSettings.network),
        DeliveryTracker.empty(costlySettings)
      )))
      viewHolderProbe.expectMsgType[GetNodeViewChanges]
      viewHolderProbe.send(synchronizer, ChangedHistory(history))
      viewHolderProbe.send(synchronizer, ChangedMempool(ErgoMemPool.empty(costlySettings)))

      sendInv(viewHolderProbe, synchronizer, peers(0), transactions.take(3).map(_.id))
      expectRequest(networkProbe, Seq(transactions.head.id))
      sendInv(viewHolderProbe, synchronizer, peers(1), transactions.drop(3).map(_.id))
      networkProbe.expectNoMessage(150.millis)

      val data = ModifiersData(ErgoTransaction.modifierTypeId,
        Map(transactions.head.id -> transactions.head.bytes))
      viewHolderProbe.send(synchronizer,
        Message(ModifiersSpec, Left(ModifiersSpec.toBytes(data)), Some(peers(0))))
      val pending = viewHolderProbe.expectMsgType[TransactionFromRemote].unconfirmedTx
      viewHolderProbe.send(synchronizer, DeclinedTransaction(pending.withCost(1000)))
      sendInv(viewHolderProbe, synchronizer, peers(1), transactions.drop(3).map(_.id))
      expectRequest(networkProbe, Seq(transactions(3).id))
    } finally {
      Await.result(system.terminate(), Duration.Inf)
    }
  }

  property("a silent mainnet-cost request releases capacity for another peer on its active timeout") {
    val fixture = new AkkaFixture
    implicit val system: ActorSystem = fixture.system
    implicit val ec: ExecutionContextExecutor = system.dispatcher

    try {
      val mainnetCostSettings = settings.copy(
        nodeSettings = settings.nodeSettings.copy(maxTransactionCost = 4900000)
      )
      val history = generateHistory(
        verifyTransactions = true,
        StateType.Utxo,
        PoPoWBootstrap = false,
        blocksToKeep = -1
      )
      val tracker = DeliveryTracker.empty(mainnetCostSettings)
      val networkProbe = TestProbe()
      val viewHolderProbe = TestProbe()
      val handlerProbe = TestProbe()
      val silentPeer = peer(23501, handlerProbe.ref)
      val responsivePeer = peer(23502, handlerProbe.ref)
      val txGen = validErgoTransactionGenTemplate(0, 0, maxInputs = 1)
      val transactions = Vector.fill(3)(txGen.sample.get._2)
      transactions.map(_.id).distinct.size shouldBe 3

      val synchronizer = system.actorOf(Props(new ErgoNodeViewSynchronizer(
        networkProbe.ref,
        viewHolderProbe.ref,
        ErgoSyncInfoMessageSpec,
        mainnetCostSettings,
        ErgoSyncTracker(mainnetCostSettings.scorexSettings.network),
        tracker
      )))
      viewHolderProbe.expectMsgType[GetNodeViewChanges]
      viewHolderProbe.send(synchronizer, ChangedHistory(history))
      viewHolderProbe.send(synchronizer, ChangedMempool(ErgoMemPool.empty(mainnetCostSettings)))

      val txType = ErgoTransaction.modifierTypeId
      sendInv(viewHolderProbe, synchronizer, silentPeer, transactions.take(2).map(_.id))
      expectRequest(networkProbe, transactions.take(2).map(_.id))
      sendInv(viewHolderProbe, synchronizer, responsivePeer, Seq(transactions(2).id))
      networkProbe.expectNoMessage(150.millis)

      val activeAttempt = tracker.getRequestedInfo(txType, transactions.head.id).get.requestId
      viewHolderProbe.send(synchronizer,
        CheckDelivery(silentPeer, txType, transactions.head.id, activeAttempt))
      sendInv(viewHolderProbe, synchronizer, responsivePeer, Seq(transactions(2).id))
      expectRequest(networkProbe, Seq(transactions(2).id))
    } finally {
      Await.result(system.terminate(), Duration.Inf)
    }
  }

  property("a mainnet-cost inventory tail resumes when a request slot becomes free") {
    val fixture = new AkkaFixture
    implicit val system: ActorSystem = fixture.system
    implicit val ec: ExecutionContextExecutor = system.dispatcher

    try {
      val mainnetCostSettings = settings.copy(
        nodeSettings = settings.nodeSettings.copy(maxTransactionCost = 4900000)
      )
      val history = generateHistory(
        verifyTransactions = true,
        StateType.Utxo,
        PoPoWBootstrap = false,
        blocksToKeep = -1
      )
      val tracker = DeliveryTracker.empty(mainnetCostSettings)
      val networkProbe = TestProbe()
      val viewHolderProbe = TestProbe()
      val handlerProbe = TestProbe()
      val sender = peer(23511, handlerProbe.ref)
      val txGen = validErgoTransactionGenTemplate(0, 0, maxInputs = 1)
      val transactions = Vector.fill(3)(txGen.sample.get._2)
      transactions.map(_.id).distinct.size shouldBe 3

      val synchronizer = system.actorOf(Props(new ErgoNodeViewSynchronizer(
        networkProbe.ref,
        viewHolderProbe.ref,
        ErgoSyncInfoMessageSpec,
        mainnetCostSettings,
        ErgoSyncTracker(mainnetCostSettings.scorexSettings.network),
        tracker
      )))
      viewHolderProbe.expectMsgType[GetNodeViewChanges]
      viewHolderProbe.send(synchronizer, ChangedHistory(history))
      viewHolderProbe.send(synchronizer, ChangedMempool(ErgoMemPool.empty(mainnetCostSettings)))

      val txType = ErgoTransaction.modifierTypeId
      sendInv(viewHolderProbe, synchronizer, sender, transactions.map(_.id))
      expectRequest(networkProbe, transactions.take(2).map(_.id))
      tracker.status(transactions(2).id, txType, Seq.empty) shouldBe ModifiersStatus.Unknown

      viewHolderProbe.awaitCond(tracker.getRequestedInfo(txType, transactions.head.id).isDefined,
        3.seconds)
      val activeAttempt = tracker.getRequestedInfo(txType, transactions.head.id).get.requestId
      viewHolderProbe.send(synchronizer,
        CheckDelivery(sender, txType, transactions.head.id, activeAttempt))
      networkProbe.fishForMessage(3.seconds) {
        case send: SendToNetwork if send.message.spec.messageCode == RequestModifierSpec.messageCode =>
          send.message.data.get.asInstanceOf[InvData].ids.contains(transactions(2).id)
        case _ => false
      }
    } finally {
      Await.result(system.terminate(), Duration.Inf)
    }
  }

  property("a disconnected inventory owner cannot receive a deferred transaction request") {
    val fixture = new AkkaFixture
    implicit val system: ActorSystem = fixture.system
    implicit val ec: ExecutionContextExecutor = system.dispatcher

    try {
      val mainnetCostSettings = settings.copy(
        nodeSettings = settings.nodeSettings.copy(maxTransactionCost = 4900000)
      )
      val history = generateHistory(
        verifyTransactions = true,
        StateType.Utxo,
        PoPoWBootstrap = false,
        blocksToKeep = -1
      )
      val tracker = DeliveryTracker.empty(mainnetCostSettings)
      val networkProbe = TestProbe()
      val viewHolderProbe = TestProbe()
      val oldHandler = TestProbe()
      val newHandler = TestProbe()
      val oldPeer = peer(23512, oldHandler.ref)
      val newPeer = peer(23512, newHandler.ref)
      val txGen = validErgoTransactionGenTemplate(0, 0, maxInputs = 1)
      val transactions = Vector.fill(3)(txGen.sample.get._2)
      transactions.map(_.id).distinct.size shouldBe 3

      val synchronizer = system.actorOf(Props(new ErgoNodeViewSynchronizer(
        networkProbe.ref,
        viewHolderProbe.ref,
        ErgoSyncInfoMessageSpec,
        mainnetCostSettings,
        ErgoSyncTracker(mainnetCostSettings.scorexSettings.network),
        tracker
      )))
      viewHolderProbe.expectMsgType[GetNodeViewChanges]
      viewHolderProbe.send(synchronizer, ChangedHistory(history))
      viewHolderProbe.send(synchronizer, ChangedMempool(ErgoMemPool.empty(mainnetCostSettings)))

      val txType = ErgoTransaction.modifierTypeId
      sendInv(viewHolderProbe, synchronizer, oldPeer, transactions.map(_.id))
      expectRequest(networkProbe, transactions.take(2).map(_.id))
      viewHolderProbe.awaitCond(tracker.getRequestedInfo(txType, transactions.head.id).isDefined,
        3.seconds)
      val activeAttempt = tracker.getRequestedInfo(txType, transactions.head.id).get.requestId
      viewHolderProbe.send(synchronizer, DisconnectedPeer(oldPeer))
      viewHolderProbe.send(synchronizer,
        CheckDelivery(oldPeer, txType, transactions.head.id, activeAttempt))
      networkProbe.expectNoMessage(200.millis)

      sendInv(viewHolderProbe, synchronizer, newPeer, Seq(transactions(2).id))
      val request = networkProbe.fishForMessage(3.seconds) {
        case send: SendToNetwork =>
          send.message.spec.messageCode == RequestModifierSpec.messageCode
        case _ => false
      }.asInstanceOf[SendToNetwork]
      request.message.data.get.asInstanceOf[InvData].ids shouldBe Seq(transactions(2).id)
      request.sendingStrategy.asInstanceOf[SendToPeer].chosenPeer.handlerRef shouldBe newHandler.ref
    } finally {
      Await.result(system.terminate(), Duration.Inf)
    }
  }

  property("a late disconnect does not discard a replacement connection's deferred inventory") {
    val fixture = new AkkaFixture
    implicit val system: ActorSystem = fixture.system
    implicit val ec: ExecutionContextExecutor = system.dispatcher

    try {
      val mainnetCostSettings = settings.copy(
        nodeSettings = settings.nodeSettings.copy(maxTransactionCost = 4900000)
      )
      val history = generateHistory(
        verifyTransactions = true,
        StateType.Utxo,
        PoPoWBootstrap = false,
        blocksToKeep = -1
      )
      val tracker = DeliveryTracker.empty(mainnetCostSettings)
      val networkProbe = TestProbe()
      val viewHolderProbe = TestProbe()
      val oldHandler = TestProbe()
      val newHandler = TestProbe()
      val oldPeer = peer(23515, oldHandler.ref)
      val newPeer = peer(23515, newHandler.ref)
      val txGen = validErgoTransactionGenTemplate(0, 0, maxInputs = 1)
      val transactions = Vector.fill(3)(txGen.sample.get._2)
      transactions.map(_.id).distinct.size shouldBe 3

      val synchronizer = system.actorOf(Props(new ErgoNodeViewSynchronizer(
        networkProbe.ref,
        viewHolderProbe.ref,
        ErgoSyncInfoMessageSpec,
        mainnetCostSettings,
        ErgoSyncTracker(mainnetCostSettings.scorexSettings.network),
        tracker
      )))
      viewHolderProbe.expectMsgType[GetNodeViewChanges]
      viewHolderProbe.send(synchronizer, ChangedHistory(history))
      viewHolderProbe.send(synchronizer, ChangedMempool(ErgoMemPool.empty(mainnetCostSettings)))

      val txType = ErgoTransaction.modifierTypeId
      sendInv(viewHolderProbe, synchronizer, oldPeer, transactions.map(_.id))
      expectRequest(networkProbe, transactions.take(2).map(_.id))
      sendInv(viewHolderProbe, synchronizer, newPeer, Seq(transactions(2).id))
      networkProbe.expectNoMessage(150.millis)
      viewHolderProbe.awaitCond(tracker.getRequestedInfo(txType, transactions.head.id).isDefined,
        3.seconds)
      val activeAttempt = tracker.getRequestedInfo(txType, transactions.head.id).get.requestId
      viewHolderProbe.send(synchronizer, DisconnectedPeer(oldPeer))
      viewHolderProbe.send(synchronizer,
        CheckDelivery(oldPeer, txType, transactions.head.id, activeAttempt))

      val request = networkProbe.fishForMessage(3.seconds) {
        case send: SendToNetwork =>
          send.message.spec.messageCode == RequestModifierSpec.messageCode
        case _ => false
      }.asInstanceOf[SendToNetwork]
      request.message.data.get.asInstanceOf[InvData].ids shouldBe Seq(transactions(2).id)
      request.sendingStrategy.asInstanceOf[SendToPeer].chosenPeer.handlerRef shouldBe newHandler.ref
    } finally {
      Await.result(system.terminate(), Duration.Inf)
    }
  }

  property("a deferred peer with exhausted cost does not block another peer") {
    val fixture = new AkkaFixture
    implicit val system: ActorSystem = fixture.system
    implicit val ec: ExecutionContextExecutor = system.dispatcher

    try {
      val mainnetCostSettings = settings.copy(
        nodeSettings = settings.nodeSettings.copy(maxTransactionCost = 4900000)
      )
      val history = generateHistory(
        verifyTransactions = true,
        StateType.Utxo,
        PoPoWBootstrap = false,
        blocksToKeep = -1
      )
      val tracker = DeliveryTracker.empty(mainnetCostSettings)
      val networkProbe = TestProbe()
      val viewHolderProbe = TestProbe()
      val handlerProbe = TestProbe()
      val firstPeer = peer(23513, handlerProbe.ref)
      val secondPeer = peer(23514, handlerProbe.ref)
      val txGen = validErgoTransactionGenTemplate(0, 0, maxInputs = 1)
      val transactions = Vector.fill(4)(txGen.sample.get._2)
      transactions.map(_.id).distinct.size shouldBe 4

      val synchronizer = system.actorOf(Props(new ErgoNodeViewSynchronizer(
        networkProbe.ref,
        viewHolderProbe.ref,
        ErgoSyncInfoMessageSpec,
        mainnetCostSettings,
        ErgoSyncTracker(mainnetCostSettings.scorexSettings.network),
        tracker
      )))
      viewHolderProbe.expectMsgType[GetNodeViewChanges]
      viewHolderProbe.send(synchronizer, ChangedHistory(history))
      viewHolderProbe.send(synchronizer, ChangedMempool(ErgoMemPool.empty(mainnetCostSettings)))

      val txType = ErgoTransaction.modifierTypeId
      sendInv(viewHolderProbe, synchronizer, firstPeer, transactions.take(3).map(_.id))
      expectRequest(networkProbe, transactions.take(2).map(_.id))
      sendInv(viewHolderProbe, synchronizer, secondPeer, Seq(transactions(3).id))
      networkProbe.expectNoMessage(150.millis)

      transactions.take(2).foreach { tx =>
        val data = ModifiersData(txType, Map(tx.id -> tx.bytes))
        viewHolderProbe.send(synchronizer,
          Message(ModifiersSpec, Left(ModifiersSpec.toBytes(data)), Some(firstPeer)))
      }
      val forwarded = viewHolderProbe.receiveN(2, 5.seconds).collect {
        case TransactionFromRemote(unconfirmedTx) => unconfirmedTx
      }
      forwarded.map(_.id).toSet shouldBe transactions.take(2).map(_.id).toSet
      forwarded.foreach { utx =>
        viewHolderProbe.send(synchronizer, DeclinedTransaction(utx.withCost(3000000)))
      }
      expectRequest(networkProbe, Seq(transactions(3).id))
      networkProbe.expectNoMessage(150.millis)

      val header = genHeaderChain(1, history, None, false).last
      viewHolderProbe.send(synchronizer, LocalBlockApplied(header, Seq.empty))
      expectRequest(networkProbe, Seq(transactions(2).id))
    } finally {
      Await.result(system.terminate(), Duration.Inf)
    }
  }

  property("a malformed reply from a replaced handler preserves the new request and its budget") {
    val fixture = new AkkaFixture

    implicit val system: ActorSystem = fixture.system
    implicit val ec: ExecutionContextExecutor = system.dispatcher

    try {
      val mainnetCostSettings = settings.copy(
        nodeSettings = settings.nodeSettings.copy(maxTransactionCost = 4900000)
      )
      val history = generateHistory(
        verifyTransactions = true,
        StateType.Utxo,
        PoPoWBootstrap = false,
        blocksToKeep = -1
      )
      val tracker = DeliveryTracker.empty(mainnetCostSettings)
      val networkProbe = TestProbe()
      val viewHolderProbe = TestProbe()
      val oldHandler = TestProbe()
      val newHandler = TestProbe()
      val unrelatedHandler = TestProbe()
      val oldPeer = peer(23601, oldHandler.ref)
      val replacementPeer = peer(23601, newHandler.ref)
      val unrelatedPeer = peer(23602, unrelatedHandler.ref)
      oldPeer shouldBe replacementPeer // ConnectedPeer equality ignores the handler.
      val transactions = Vector.fill(3)(validErgoTransactionGenTemplate(0, 0, maxInputs = 1).sample.get._2)
      transactions.map(_.id).distinct.size shouldBe 3

      val synchronizer = system.actorOf(Props(new ErgoNodeViewSynchronizer(
        networkProbe.ref,
        viewHolderProbe.ref,
        ErgoSyncInfoMessageSpec,
        mainnetCostSettings,
        ErgoSyncTracker(mainnetCostSettings.scorexSettings.network),
        tracker
      )))
      viewHolderProbe.expectMsgType[GetNodeViewChanges]
      viewHolderProbe.send(synchronizer, ChangedHistory(history))
      viewHolderProbe.send(synchronizer, ChangedMempool(ErgoMemPool.empty(mainnetCostSettings)))

      val txType = ErgoTransaction.modifierTypeId
      sendInv(viewHolderProbe, synchronizer, oldPeer, Seq(transactions.head.id))
      expectRequest(networkProbe, Seq(transactions.head.id))
      viewHolderProbe.awaitCond(tracker.getRequestedInfo(txType, transactions.head.id).isDefined,
        3.seconds)
      val oldAttempt = tracker.getRequestedInfo(txType, transactions.head.id).get.requestId
      viewHolderProbe.send(synchronizer,
        CheckDelivery(oldPeer, txType, transactions.head.id, oldAttempt))
      sendInv(viewHolderProbe, synchronizer, replacementPeer, Seq(transactions.head.id))
      expectRequest(networkProbe, Seq(transactions.head.id))
      viewHolderProbe.awaitCond(tracker.getRequestedInfo(txType, transactions.head.id)
        .exists(_.peer.handlerRef == newHandler.ref), 3.seconds)
      val replacementAttempt = tracker.getRequestedInfo(txType, transactions.head.id).get.requestId
      replacementAttempt should not be oldAttempt

      sendInv(viewHolderProbe, synchronizer, unrelatedPeer, Seq(transactions(1).id))
      expectRequest(networkProbe, Seq(transactions(1).id))
      sendInv(viewHolderProbe, synchronizer, unrelatedPeer, Seq(transactions(2).id))
      networkProbe.expectNoMessage(150.millis)

      val malformed = ModifiersData(txType, Map(transactions.head.id -> Array.emptyByteArray))
      viewHolderProbe.send(synchronizer,
        Message(ModifiersSpec, Left(ModifiersSpec.toBytes(malformed)), Some(oldPeer)))
      networkProbe.expectMsg(PenalizePeer(oldPeer.connectionId.remoteAddress,
        PenaltyType.MisbehaviorPenalty))
      tracker.getRequestedInfo(txType, transactions.head.id)
        .map(_.requestId) shouldBe Some(replacementAttempt)
      tracker.getRequestedInfo(txType, transactions.head.id)
        .map(_.peer.handlerRef) shouldBe Some(newHandler.ref)

      sendInv(viewHolderProbe, synchronizer, unrelatedPeer, Seq(transactions(2).id))
      networkProbe.expectNoMessage(150.millis)
    } finally {
      Await.result(system.terminate(), Duration.Inf)
    }
  }

  property("a malformed first response does not strand a valid received successor") {
    val fixture = new AkkaFixture
    implicit val system: ActorSystem = fixture.system
    implicit val ec: ExecutionContextExecutor = system.dispatcher

    try {
      val history = generateHistory(
        verifyTransactions = true,
        StateType.Utxo,
        PoPoWBootstrap = false,
        blocksToKeep = -1
      )
      val networkProbe = TestProbe()
      val viewHolderProbe = TestProbe()
      val handlerProbe = TestProbe()
      val remote = peer(24001, handlerProbe.ref)
      val txGen = validErgoTransactionGenTemplate(0, 0, maxInputs = 1)
      val transactions = Vector.fill(2)(txGen.sample.get._2)
      transactions.map(_.id).distinct.size shouldBe 2

      val synchronizer = system.actorOf(Props(new ErgoNodeViewSynchronizer(
        networkProbe.ref,
        viewHolderProbe.ref,
        ErgoSyncInfoMessageSpec,
        settings,
        ErgoSyncTracker(settings.scorexSettings.network),
        DeliveryTracker.empty(settings)
      )))
      viewHolderProbe.expectMsgType[GetNodeViewChanges]
      viewHolderProbe.send(synchronizer, ChangedHistory(history))
      viewHolderProbe.send(synchronizer, ChangedMempool(ErgoMemPool.empty(settings)))

      val txType = ErgoTransaction.modifierTypeId
      sendInv(viewHolderProbe, synchronizer, remote, transactions.map(_.id))
      expectRequest(networkProbe, transactions.map(_.id))
      val validData = ModifiersData(txType, transactions.map(tx => tx.id -> tx.bytes).toMap)
      val firstId = ModifiersSpec.parseBytesTry(ModifiersSpec.toBytes(validData)).get.modifiers.head._1
      val successor = transactions.find(_.id != firstId).get
      val mixedData = ModifiersData(txType,
        validData.modifiers.updated(firstId, Array.emptyByteArray))
      val mixedBytes = ModifiersSpec.toBytes(mixedData)
      ModifiersSpec.parseBytesTry(mixedBytes).get.modifiers.head._1 shouldBe firstId

      viewHolderProbe.send(synchronizer, Message(ModifiersSpec, Left(mixedBytes), Some(remote)))
      val forwarded = viewHolderProbe.expectMsgType[TransactionFromRemote](3.seconds)
      forwarded.unconfirmedTx.id shouldBe successor.id
    } finally {
      Await.result(system.terminate(), Duration.Inf)
    }
  }
}
