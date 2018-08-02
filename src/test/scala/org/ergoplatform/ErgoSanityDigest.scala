package org.ergoplatform

import akka.actor.{ActorRef, ActorSystem}
import akka.testkit.TestProbe
import org.ergoplatform.ErgoSanity._
import org.ergoplatform.network.ErgoNodeViewSynchronizer
import org.ergoplatform.nodeView.history.ErgoSyncInfoMessageSpec
import org.ergoplatform.nodeView.state.{DigestState, StateType}
import org.ergoplatform.nodeView.{WrappedDigestState, WrappedUtxoState}
import org.ergoplatform.settings.ErgoSettings
import org.scalacheck.Gen
import scorex.core.network.{ConnectedPeer, Handshake, Outgoing}
import scorex.core.utils.NetworkTimeProvider
import scorex.core.app.{Version => HandshakeV}

class ErgoSanityDigest extends ErgoSanity[DIGEST_ST] {
  override val historyGen: Gen[HT] = generateHistory(verifyTransactions = true, StateType.Digest, PoPoWBootstrap = false, -1)

  override val stateGen: Gen[WrappedDigestState] = {
    boxesHolderGen.map(WrappedUtxoState(_, createTempDir, emission, None)).map { wus =>
      val digestState = DigestState.create(Some(wus.version), Some(wus.rootHash), createTempDir, settings)
      new WrappedDigestState(digestState, wus, settings)
    }
  }

  override def semanticallyValidModifier(state: DIGEST_ST): PM = validFullBlock(None, state.asInstanceOf[WrappedDigestState].wrappedUtxoState)

  override def semanticallyInvalidModifier(state: DIGEST_ST): PM = invalidErgoFullBlockGen.sample.get

  override def totallyValidModifier(history: HT, state: DIGEST_ST): PM = {
    val parentOpt = history.bestHeaderOpt
    validFullBlock(parentOpt, state.asInstanceOf[WrappedDigestState].wrappedUtxoState)
  }

  override def totallyValidModifiers(history: HT, state: DIGEST_ST, count: Int): Seq[PM] = {
    require(count >= 1)
    val headerOpt = history.bestHeaderOpt
    (0 until count).foldLeft((headerOpt, Seq.empty[PM])) { case (acc, _) =>
      val pm = validFullBlock(headerOpt, state.asInstanceOf[WrappedDigestState].wrappedUtxoState)
      (Some(pm.header), acc._2 :+ pm)
    }._2
  }

  override def nodeViewSynchronizer(implicit system: ActorSystem):
  (ActorRef, SI, PM, TX, ConnectedPeer, TestProbe, TestProbe, TestProbe, TestProbe) = {
    @SuppressWarnings(Array("org.wartremover.warts.OptionPartial"))
    val h = historyGen.sample.get
    @SuppressWarnings(Array("org.wartremover.warts.OptionPartial"))
    val s = stateGen.sample.get
    implicit val ec = system.dispatcher
    val settings = ErgoSettings.read(None)
    val tp = new NetworkTimeProvider(settings.scorexSettings.ntp)
    val ncProbe = TestProbe("NetworkControllerProbe")
    val vhProbe = TestProbe("ViewHolderProbe")
    val pchProbe = TestProbe("PeerHandlerProbe")
    val eventListener = TestProbe("EventListener")
    val ref = ErgoNodeViewSynchronizer(
      ncProbe.ref,
      vhProbe.ref,
      ErgoSyncInfoMessageSpec,
      settings.scorexSettings.network,
      tp
    )
    val m = totallyValidModifier(h, s)
    @SuppressWarnings(Array("org.wartremover.warts.OptionPartial"))
    val tx = validErgoTransactionGenTemplate(0, 0).sample.get._2
    @SuppressWarnings(Array("org.wartremover.warts.OptionPartial"))
    val p: ConnectedPeer = ConnectedPeer(inetSocketAddressGen.sample.get, pchProbe.ref, Outgoing,
      Handshake("", HandshakeV(0, 1, 2), "", None, Seq(), 0L))
    (ref, h.syncInfo, m, tx, p, pchProbe, ncProbe, vhProbe, eventListener)
  }
}
