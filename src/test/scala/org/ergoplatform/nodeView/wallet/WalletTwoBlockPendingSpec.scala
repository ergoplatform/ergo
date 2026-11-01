package org.ergoplatform.nodeView.wallet

import akka.actor.{ActorSystem, Props}
import akka.testkit.TestProbe
import org.ergoplatform._
import org.ergoplatform.db.DBSpec
import org.ergoplatform.modifiers.ErgoFullBlock
import org.ergoplatform.modifiers.history.BlockTransactions
import org.ergoplatform.modifiers.history.extension.Extension
import org.ergoplatform.modifiers.mempool.{ErgoTransaction, UnconfirmedTransaction}
import org.ergoplatform.network.ErgoNodeViewSynchronizerMessages.ChangedMempool
import org.ergoplatform.nodeView.wallet.ErgoWalletActorMessages._
import org.ergoplatform.nodeView.wallet.WalletScanLogic.ScanResults
import org.ergoplatform.nodeView.wallet.persistence.{OffChainRegistry, WalletRegistry, WalletStorage}
import org.ergoplatform.sdk.SecretString
import org.ergoplatform.settings.ErgoSettings
import org.ergoplatform.utils.{ErgoCorePropertyTest, MempoolTestHelpers, WalletTestOps}
import org.ergoplatform.utils.fixtures.WalletFixture
import org.ergoplatform.wallet.Constants.PaymentsScanId
import org.ergoplatform.wallet.boxes.TrackedBox
import org.scalatest.BeforeAndAfterAll
import sigmastate.helpers.TestingHelpers.testBox

import scala.collection.compat.immutable.ArraySeq
import scala.concurrent.Await
import scala.concurrent.duration._

/** Current-tree actor control: real registry scans, with only bootstrap and mempool stubbed. */
class WalletTwoBlockPendingSpec extends ErgoCorePropertyTest
  with MempoolTestHelpers with WalletTestOps with ErgoWalletSupport
  with DBSpec with BeforeAndAfterAll {
  import org.ergoplatform.utils.ErgoCoreTestConstants._
  import org.ergoplatform.utils.ErgoNodeTestConstants._
  import org.ergoplatform.utils.generators.CoreObjectGenerators._
  import org.ergoplatform.utils.generators.ErgoCoreGenerators._

  override val ergoSettings: ErgoSettings = settings
  private implicit val fixture: WalletFixture =
    new WalletFixture(settings, parameters, getCurrentView(_).vault)

  override def afterAll(): Unit = try super.afterAll() finally fixture.stop()

  for (storeChild <- Seq(false, true)) {
    property(s"two real block scans retain a live child with stale reader, stored=$storeChild") {
      withVersionedStore(2) { versionedStore =>
        withStore { store =>
          val registry = new WalletRegistry(versionedStore)(settings.walletSettings)
          val script = getPublicKeys.head.script
          val source = TrackedBox(modifierIdGen.sample.get, 0, Some(100), None, None,
            testBox(1000000000L, script, 100), Set(PaymentsScanId))
          registry.updateOnBlock(ScanResults(Seq(source), ArraySeq.empty, ArraySeq.empty),
            modifierIdGen.sample.get, 100).get
          val dataBox = testBox(500000000L, org.ergoplatform.settings.Constants.TrueTree, 100)
          val parent = ErgoTransaction(IndexedSeq(Input(source.box.id, emptyProverResult)),
            IndexedSeq(DataInput(dataBox.id)),
            IndexedSeq(new ErgoBoxCandidate(700000000L, script, 100)))
          val child = ErgoTransaction(IndexedSeq(Input(parent.outputs.head.id, emptyProverResult)),
            IndexedSeq(new ErgoBoxCandidate(400000000L, script, 100)))
          val storage = new WalletStorage(store, settings)
          storage.addUnconfirmedTransaction(parent, 100).get
          if (storeChild) storage.addUnconfirmedTransaction(child, 100).get
          val staleReader = new FakeMempool(Seq(parent, child).map(UnconfirmedTransaction(_, None)))
          val service = new ErgoWalletServiceImpl(settings) {
            override def readWallet(state: ErgoWalletState, mnemonic: Option[SecretString],
                keys: Option[Int], secretSettings: org.ergoplatform.wallet.settings.SecretStorageSettings): ErgoWalletState = state
            override def restoreOffChainState(state: ErgoWalletState): ErgoWalletState = state
          }
          val initial = ErgoWalletState(storage, None, registry, OffChainRegistry.init(registry),
            None, WalletVars(Some(defaultProver), Seq.empty, None), None, Some(staleReader),
            None, parameters, 1000, rescanInProgress = false)
          val pending = service.scanOffChainUpdate(
            service.scanOffChainUpdate(initial, parent)._1, child)._1
          val system = ActorSystem("two-block-pending-" + java.util.UUID.randomUUID())
          try {
            val ref = system.actorOf(Props(new ErgoWalletActor(settings, parameters, service, null, null) {
              override def preStart(): Unit = self ! ReadWallet(pending)
            }))
            val probe = TestProbe()(system)
            def checkChild(): Unit = {
              probe.send(ref, GetWalletStatus)
              probe.expectMsgType[WalletStatus](10.seconds).error shouldBe empty
              probe.send(ref, GetWalletBoxes(unspentOnly = true, considerUnconfirmed = true))
              val boxes = probe.expectMsgType[Seq[WalletBox]](10.seconds)
              boxes.map(_.trackedBox.boxId).toSet shouldBe Set(scorex.util.bytesToId(child.outputs.head.id))
              storage.unconfirmedTransactionHeights.contains(child.id) shouldBe storeChild
            }
            def block(height: Int, tx: ErgoTransaction): ErgoFullBlock = {
              val header = defaultHeaderGen.sample.get.copy(height = height)
              ErgoFullBlock(header, BlockTransactions(header.id, header.version, Seq(tx)),
                Extension(header.id, Seq.empty), None)
            }
            checkChild()
            probe.send(ref, ScanOnChain(block(101, parent)))
            checkChild()
            registry.fetchDigest().height shouldBe 101
            storage.unconfirmedTransactionHeights should not contain key(parent.id)
            // X is only a data input of the now-confirmed parent; the child does not read it.
            val spendData = ErgoTransaction(IndexedSeq(Input(dataBox.id, emptyProverResult)),
              IndexedSeq(new ErgoBoxCandidate(dataBox.value, org.ergoplatform.settings.Constants.TrueTree, 101)))
            probe.send(ref, ScanOnChain(block(102, spendData)))
            checkChild()
            registry.fetchDigest().height shouldBe 102
            probe.send(ref, ChangedMempool(new FakeMempool(Seq(UnconfirmedTransaction(child, None)))))
            checkChild()
            probe.watch(ref)
            system.stop(ref)
            probe.expectTerminated(ref, 10.seconds)
          } finally Await.result(system.terminate(), 15.seconds)
        }
      }
    }
  }
}

