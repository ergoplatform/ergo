package org.ergoplatform.sanity

import akka.actor.{ActorRef, ActorSystem, Props}
import akka.testkit.TestProbe
import org.ergoplatform.modifiers.ErgoFullBlock
import org.ergoplatform.modifiers.history.BlockTransactions
import org.ergoplatform.modifiers.history.header.HeaderSerializer
import org.ergoplatform.network.ErgoNodeViewSynchronizerMessages.{ChangedHistory, ChangedMempool}
import org.ergoplatform.network.ErgoSyncTracker
import org.ergoplatform.nodeView.history.ErgoSyncInfoMessageSpec
import org.ergoplatform.nodeView.mempool.ErgoMemPool
import org.ergoplatform.nodeView.state.wrapped.{WrappedDigestState, WrappedUtxoState}
import org.ergoplatform.nodeView.state.{DigestState, StateType}
import org.ergoplatform.sanity.ErgoSanity._
import org.ergoplatform.settings.ErgoSettingsReader
import org.scalacheck.Gen
import org.ergoplatform.core.idToBytes
import scorex.core.network.{ConnectedPeer, DeliveryTracker}
import org.ergoplatform.network.peer.PeerInfo
import org.ergoplatform.serialization.ErgoSerializer
import scorex.testkit.generators.{SemanticallyInvalidModifierProducer, SemanticallyValidModifierProducer}

import scala.concurrent.ExecutionContextExecutor

class ErgoSanityDigest extends ErgoSanity[DIGEST_ST]
  with SemanticallyValidModifierProducer[DIGEST_ST]
  with SemanticallyInvalidModifierProducer[DIGEST_ST]
  {
  import org.ergoplatform.utils.ErgoCoreTestConstants._
  import org.ergoplatform.utils.ErgoNodeTestConstants._
  import org.ergoplatform.utils.generators.ConnectedPeerGenerators._
  import org.ergoplatform.utils.generators.CoreObjectGenerators._
  import org.ergoplatform.utils.HistoryTestHelpers._
  import org.ergoplatform.utils.generators.ErgoNodeTransactionGenerators._
  import org.ergoplatform.utils.generators.ErgoCoreTransactionGenerators._
  import org.ergoplatform.utils.generators.ValidBlocksGenerators._

  override val historyGen: Gen[HT] =
    generateHistory(verifyTransactions = true, StateType.Digest, PoPoWBootstrap = false, -1)

  override val stateGen: Gen[WrappedDigestState] = {
    boxesHolderGen.map(WrappedUtxoState(_, createTempDir, None, parameters, settings)).map { wus =>
      val digestState = DigestState.create(Some(wus.version), Some(wus.rootDigest), createTempDir, settings)
      new WrappedDigestState(digestState, wus, settings)
    }
  }

  override def semanticallyValidModifier(state: DIGEST_ST): PM =
    validFullBlock(None, state.asInstanceOf[WrappedDigestState].wrappedUtxoState)

  override def semanticallyInvalidModifier(state: DIGEST_ST): PM = invalidErgoFullBlockGen.sample.get

  override def totallyValidModifier(history: HT, state: DIGEST_ST): PM = {
    val parentOpt = history.bestFullBlockOpt
    validFullBlock(parentOpt, state.asInstanceOf[WrappedDigestState].wrappedUtxoState).header
  }

  override def totallyValidModifiers(history: HT, state: DIGEST_ST, count: Int): Seq[PM] = {
    require(count >= 1)
    val blockOpt = history.bestFullBlockOpt
    (0 until count).foldLeft((blockOpt, Seq.empty[PM])) { case (acc, _) =>
      val pm = validFullBlock(blockOpt, state.asInstanceOf[WrappedDigestState].wrappedUtxoState)
      (Some(pm), acc._2 :+ pm)
    }._2.map(_.asInstanceOf[ErgoFullBlock].header)
  }

  override def nodeViewSynchronizer(implicit system: ActorSystem):
  (ActorRef, SI, PM, TX, ConnectedPeer, TestProbe, TestProbe, TestProbe, TestProbe, ErgoSerializer[PM]) = {
    @SuppressWarnings(Array("org.wartremover.warts.OptionPartial"))
    val h = historyGen.sample.get
    @SuppressWarnings(Array("org.wartremover.warts.OptionPartial"))
    val s = stateGen.sample.get
    val settings = ErgoSettingsReader.read()
    val pool = ErgoMemPool.empty(settings)
    val v = h.bestFullBlockIdOpt.orElse(h.bestHeaderIdOpt)
    v.foreach(id => s.store.update(idToBytes(id), Seq(), Seq()).get)
    implicit val ec: ExecutionContextExecutor = system.dispatcher
    val ncProbe = TestProbe("NetworkControllerProbe")
    val vhProbe = TestProbe("ViewHolderProbe")
    val pchProbe = TestProbe("PeerHandlerProbe")
    val eventListener = TestProbe("EventListener")
    val syncTracker = ErgoSyncTracker(settings.scorexSettings.network)
    val deliveryTracker: DeliveryTracker = DeliveryTracker.empty(settings)
    val ref = system.actorOf(Props(
      new SyncronizerMock(
        ncProbe.ref,
        vhProbe.ref,
        ErgoSyncInfoMessageSpec,
        settings,
        syncTracker,
        deliveryTracker
      )
    ))
    val m = totallyValidModifier(h, s)
    @SuppressWarnings(Array("org.wartremover.warts.OptionPartial"))
    val tx = validErgoTransactionGenTemplate(minAssets = 0, maxAssets = 0).sample.get._2


    val peerInfo = PeerInfo(defaultPeerSpec, Long.MaxValue, None, 0L)

    @SuppressWarnings(Array("org.wartremover.warts.OptionPartial"))
    val p: ConnectedPeer = ConnectedPeer(
      connectionIdGen.sample.get,
      pchProbe.ref,
      Some(peerInfo)
    )
    ref ! ChangedHistory(h)
    ref ! ChangedMempool(pool)
    val serializer: ErgoSerializer[PM] = HeaderSerializer.asInstanceOf[ErgoSerializer[PM]]
    (ref, h.syncInfoV1, m, tx, p, pchProbe, ncProbe, vhProbe, eventListener, serializer)
  }

  override def modifierWithTransactions(memoryPoolOpt: Option[MPool], customTransactionsOpt: Option[Seq[TX]]): CTM = {
    val boxHolder = boxesHolderGen.sample.get
    val txs = validTransactionsFromBoxHolder(boxHolder)._1
    val id = modifierIdGen.sample.get
    BlockTransactions(id, 1: Byte, txs)
  }
}
