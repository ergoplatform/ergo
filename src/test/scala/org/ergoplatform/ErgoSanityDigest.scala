package org.ergoplatform

import java.io.File

import akka.actor.{ActorRef, ActorSystem, Props}
import akka.testkit.TestProbe
import io.iohk.iodb.ByteArrayWrapper
import org.ergoplatform.ErgoSanity.{DIGEST_ST, HT, PM}
import org.ergoplatform.mining.DefaultFakePowScheme
import org.ergoplatform.mining.emission.CoinsEmission
import org.ergoplatform.modifiers.ErgoFullBlock
import org.ergoplatform.nodeView.mempool.ErgoMemPool
import org.ergoplatform.nodeView.state.{DigestState, ErgoState, StateType, UtxoState}
import org.ergoplatform.nodeView.wallet.ErgoWallet
import org.ergoplatform.nodeView.{ErgoNodeViewHolder, ErgoNodeViewRef, WrappedDigestState, WrappedUtxoState}
import org.ergoplatform.settings.ErgoSettings
import org.scalacheck.Gen
import scorex.core.utils.NetworkTimeProvider
import scorex.core.{idToBytes, idToVersion}

import scala.concurrent.ExecutionContext

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
    validFullBlock(parentOpt, state.asInstanceOf[WrappedDigestState].wrappedUtxoState).header
  }

  override def totallyValidModifiers(history: HT, state: DIGEST_ST, count: Int): Seq[PM] = {
    require(count >= 1)
    val headerOpt = history.bestHeaderOpt
    (0 until count).foldLeft((headerOpt, Seq.empty[PM])) { case (acc, _) =>
      val pm = validFullBlock(headerOpt, state.asInstanceOf[WrappedDigestState].wrappedUtxoState)
      (Some(pm.header), acc._2 :+ pm)
    }._2.map(_.asInstanceOf[ErgoFullBlock].header)
  }

  override def nodeViewHolder(implicit system: ActorSystem): (ActorRef, TestProbe, PM, DIGEST_ST, HT) = {
    implicit val ec = system.dispatcher
    val dir: File = createTempDir
    val defaultSettings: ErgoSettings = ErgoSettings.read(None).copy(directory = dir.getAbsolutePath)
    val settings = defaultSettings.copy(
      nodeSettings = defaultSettings.nodeSettings.copy(
        stateType = StateType.Digest,
        verifyTransactions = false,
        PoPoWBootstrap = false
      )
    )
    val ce = new CoinsEmission(settings.chainSettings.monetary)

    val h = historyGen.sample.get
    val s = stateGen.sample.get
    val v = h.openSurfaceIds().last
    s.store.update(ByteArrayWrapper(idToBytes(v)), Seq(), Seq())
    val ref = system.actorOf(NodeViewHolderForTests.props(h, s, settings, ce))
    val m = totallyValidModifier(h, s)
    val eventListener = TestProbe()
    (ref, eventListener, m, s, h)
  }

  class NodeViewHolderForTests(h: HT, s: DIGEST_ST, settings: ErgoSettings, ce: CoinsEmission)
                              (implicit ec: ExecutionContext)
    extends ErgoNodeViewHolder[DIGEST_ST](settings, new NetworkTimeProvider(settings.scorexSettings.ntp), ce) {

    override protected def genesisState: (HIS, MS, VL, MP) = {
      @SuppressWarnings(Array("org.wartremover.warts.OptionPartial"))
      val w = new ErgoWallet
      (h, s, w, ErgoMemPool.empty)
    }

    override def restoreState(): Option[(HIS, MS, VL, MP)] = None
  }

  object NodeViewHolderForTests {
    def props(h: HT, s: DIGEST_ST, settings: ErgoSettings, ce: CoinsEmission)(implicit ec: ExecutionContext): Props =
      Props(new NodeViewHolderForTests(h, s, settings, ce))
  }
}
