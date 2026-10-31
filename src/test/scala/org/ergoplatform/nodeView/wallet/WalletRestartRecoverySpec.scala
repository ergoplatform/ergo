package org.ergoplatform.nodeView.wallet

import java.io.File

import akka.actor.{ActorRef, ActorSystem}
import akka.pattern.ask
import akka.util.Timeout
import org.ergoplatform._
import org.ergoplatform.modifiers.mempool.{ErgoTransaction, UnconfirmedTransaction}
import org.ergoplatform.nodeView.ErgoNodeViewHolder.{CurrentView, ReceivableMessages}
import org.ergoplatform.nodeView.ErgoNodeViewRef
import org.ergoplatform.nodeView.mempool.ErgoMemPoolUtils.{ProcessingOutcome, SortingOption}
import org.ergoplatform.nodeView.state.ErgoState
import org.ergoplatform.nodeView.wallet.requests.PaymentRequest
import org.ergoplatform.settings.Constants
import org.ergoplatform.settings.Constants.TrueTree
import org.ergoplatform.settings.ErgoSettings
import sigma.ast.ShortConstant
import sigma.interpreter.{ContextExtension, ProverResult}
import org.ergoplatform.utils.{ErgoCorePropertyTest, WalletTestOps}
import org.ergoplatform.utils.fixtures.WalletFixture
import org.ergoplatform.wallet.boxes.ErgoBoxSerializer
import org.scalatest.concurrent.Eventually
import scorex.crypto.authds.ADKey
import scorex.db.{LDBFactory, StoreRegistry}
import scorex.util.encode.Base16

import scala.concurrent.Await
import scala.concurrent.duration._

class WalletRestartRecoverySpec extends ErgoCorePropertyTest with WalletTestOps with Eventually {
  import org.ergoplatform.utils.ErgoCoreTestConstants._
  import org.ergoplatform.utils.ErgoNodeTestConstants._
  override implicit val patienceConfig: PatienceConfig = PatienceConfig(15.seconds, 100.millis)
  private implicit val addressEncoder: ErgoAddressEncoder =
    ErgoAddressEncoder(ErgoAddressEncoder.TestnetNetworkPrefix)
  private implicit val askTimeout: Timeout = Timeout(20.seconds)

  private type View = CurrentView[ErgoState[_]]

  private def closeStoresUnder(dir: File): Unit = {
    val normalizedRoot = dir.toPath.toAbsolutePath.normalize()
    val registry = LDBFactory.factory.asInstanceOf[StoreRegistry]
    registry.lock.writeLock().lock()
    try {
      val matching = registry.map.toVector.collect {
        case (path, store) if path.toPath.toAbsolutePath.normalize().startsWith(normalizedRoot) =>
          path -> store
      }
      matching.foreach { case (path, store) =>
        store.close()
        registry.map.remove(path)
      }
    } finally registry.lock.writeLock().unlock()
  }

  private def withRestartedView[A](restartedSettings: ErgoSettings, dir: File)
                                  (body: (ActorRef, () => View) => A): A = {
    val restartedSystem = ActorSystem()
    val restarted = ErgoNodeViewRef(restartedSettings)(restartedSystem)
    def fetchView(): View = {
      val request = ReceivableMessages.GetDataFromCurrentView[ErgoState[_], View](v => v)
      Await.result((restarted ? request).mapTo[View], 20.seconds)
    }
    try body(restarted, () => fetchView())
    finally {
      Await.result(restartedSystem.terminate(), 30.seconds)
      closeStoresUnder(dir)
    }
  }

  private def encodeBox(box: ErgoBox): String =
    Base16.encode(ErgoBoxSerializer.toBytes(box))

  private def unsignedLexLess(a: String, b: String): Boolean = {
    val ab = Base16.decode(a).get
    val bb = Base16.decode(b).get
    val len = math.min(ab.length, bb.length)
    var i = 0
    while (i < len) {
      val ai = ab(i) & 0xff
      val bi = bb(i) & 0xff
      if (ai != bi) return ai < bi
      i += 1
    }
    ab.length < bb.length
  }

  property("restore a persisted pending payment into the real mempool after closing and reopening stores") {
    val fixture = new WalletFixture(settings, parameters, getCurrentView(_).vault)
    val pending: ErgoTransaction = try fixture.apply { implicit active =>
      val address = getPublicKeys.head
      applyBlock(makeGenesisBlock(address.pubkey)) shouldBe 'success
      val tx = eventually {
        val amount = getConfirmedBalances.walletBalance / 2
        val fee = math.max(settings.nodeSettings.minimalFeeAmount, 1000000L)
        val requests = Seq(
          PaymentRequest(address, amount, Array.empty, Map.empty),
          PaymentRequest(Pay2SAddress(settings.chainSettings.monetary.feeProposition),
            fee, Array.empty, Map.empty)
        )
        await(wallet.generateTransaction(requests)).get
      }
      wallet.scanOffchain(tx)
      eventually {
        await(wallet.unconfirmedTransactionsToRestore).map(_.id) shouldBe Seq(tx.id)
      }
      getCurrentView.pool.contains(tx.id) shouldBe false
      tx
    } finally {
      Await.result(fixture.actorSystem.whenTerminated, 30.seconds)
      // A wrapper around the same live DB would not establish persistence.
      closeStoresUnder(fixture.nodeViewDir)
    }

    withRestartedView(fixture.settings, fixture.nodeViewDir) { (_, fetchView) =>
      eventually {
        val view = fetchView()
        view.pool.getAll.map(_.id) shouldBe Seq(pending.id)
        Await.result(view.vault.unconfirmedTransactionsToRestore, 20.seconds)
          .map(_.id) shouldBe Seq(pending.id)
      }
    }
  }

  property("retain persisted record when restored transaction is already in the mempool (Declined: AlreadyInPool)") {
    val fixture = new WalletFixture(settings, parameters, getCurrentView(_).vault)
    val pending: ErgoTransaction = try fixture.apply { implicit active =>
      val address = getPublicKeys.head
      applyBlock(makeGenesisBlock(address.pubkey)) shouldBe 'success
      val tx = eventually {
        val amount = getConfirmedBalances.walletBalance / 2
        val fee = math.max(settings.nodeSettings.minimalFeeAmount, 1000000L)
        val requests = Seq(
          PaymentRequest(address, amount, Array.empty, Map.empty),
          PaymentRequest(Pay2SAddress(settings.chainSettings.monetary.feeProposition),
            fee, Array.empty, Map.empty)
        )
        await(wallet.generateTransaction(requests)).get
      }
      wallet.scanOffchain(tx)
      eventually {
        await(wallet.unconfirmedTransactionsToRestore).map(_.id) shouldBe Seq(tx.id)
      }
      tx
    } finally {
      Await.result(fixture.actorSystem.whenTerminated, 30.seconds)
      closeStoresUnder(fixture.nodeViewDir)
    }

    // First restart: once pending is in the pool, a duplicate RestoredTransaction hits
    // Declined(AlreadyInPool) and must not delete the stored wallet record.
    withRestartedView(fixture.settings, fixture.nodeViewDir) { (restarted, fetchView) =>
      val initialView = eventually {
        val v = fetchView()
        v.pool.getAll.map(_.id) shouldBe Seq(pending.id)
        v
      }
      val (_, duplicateOutcome) = initialView.pool.process(UnconfirmedTransaction(pending, None), initialView.state)
      duplicateOutcome shouldBe a[ProcessingOutcome.Declined]
      duplicateOutcome.asInstanceOf[ProcessingOutcome.Declined].reason shouldBe
        ProcessingOutcome.DeclineReason.AlreadyInPool
      duplicateOutcome.retainPersistedWalletTx shouldBe true

      restarted ! ReceivableMessages.RestoredTransaction(UnconfirmedTransaction(pending, None))
      val afterView = fetchView()
      afterView.pool.getAll.map(_.id) shouldBe Seq(pending.id)
      Await.result(afterView.vault.unconfirmedTransactionsToRestore, 20.seconds)
        .map(_.id) shouldBe Seq(pending.id)
    }

    // Second restart into an empty mempool proves the record survived the AlreadyInPool refusal.
    withRestartedView(fixture.settings, fixture.nodeViewDir) { (_, fetchView) =>
      eventually {
        val view = fetchView()
        view.pool.getAll.map(_.id) shouldBe Seq(pending.id)
      }
    }
  }

  property("retain persisted record when mempool refuses restore due to capacity (Declined: PoolCapacity)") {
    val fixture = new WalletFixture(settings, parameters, getCurrentView(_).vault)
    val baseFee = math.max(settings.nodeSettings.minimalFeeAmount, 1000000L)
    val feeProp = settings.chainSettings.monetary.feeProposition
    val feeAddr = Pay2SAddress(feeProp)

    val (lowFeeWalletTx, highFeeNonWalletTx) = try fixture.apply { implicit active =>
      val address = getPublicKeys.head
      applyBlock(makeGenesisBlock(address.pubkey)) shouldBe 'success
      val splitTx = eventually {
        val quarter = getConfirmedBalances.walletBalance / 4
        val requests = Seq(
          PaymentRequest(address, quarter, Array.empty, Map.empty),
          PaymentRequest(Pay2SAddress(TrueTree), quarter, Array.empty, Map.empty),
          PaymentRequest(feeAddr, baseFee, Array.empty, Map.empty)
        )
        await(wallet.generateTransaction(requests)).get
      }
      applyBlock(makeNextBlock(getUtxoState, Seq(splitTx))) shouldBe 'success

      val walletBox = splitTx.outputs.find(_.ergoTree == address.script).get
      val nonWalletBox = splitTx.outputs.find(_.ergoTree == TrueTree).get
      eventually {
        await(wallet.walletBoxes(unspentOnly = true, considerUnconfirmed = false))
          .exists(_.trackedBox.box.id.sameElements(walletBox.id)) shouldBe true
      }

      val walletPayment = await(wallet.generateTransaction(
        requests = Seq(
          PaymentRequest(address, walletBox.value - baseFee, Array.empty, Map.empty),
          PaymentRequest(feeAddr, baseFee, Array.empty, Map.empty)
        ),
        inputsRaw = Seq(encodeBox(walletBox))
      )).get
      wallet.scanOffchain(walletPayment)
      eventually {
        await(wallet.unconfirmedTransactionsToRestore).map(_.id) shouldBe Seq(walletPayment.id)
      }

      val highFee = baseFee * 25
      val externalHighFeeTx = ErgoTransaction(
        IndexedSeq(new Input(nonWalletBox.id, emptyProverResult)),
        IndexedSeq(
          new ErgoBoxCandidate(nonWalletBox.value - highFee, TrueTree, creationHeight = 1),
          new ErgoBoxCandidate(highFee, feeProp, creationHeight = 1)
        )
      )
      (walletPayment, externalHighFeeTx)
    } finally {
      Await.result(fixture.actorSystem.whenTerminated, 30.seconds)
      closeStoresUnder(fixture.nodeViewDir)
    }

    val capOneSettings = fixture.settings.copy(
      nodeSettings = fixture.settings.nodeSettings.copy(
        mempoolCapacity = 1,
        mempoolSorting = SortingOption.FeePerByte
      )
    )

    withRestartedView(capOneSettings, fixture.nodeViewDir) { (restarted, fetchView) =>
      eventually {
        fetchView().pool.contains(lowFeeWalletTx.id) shouldBe true
      }
      val admitHigh = Await.result(
        (restarted ? ReceivableMessages.LocallyGeneratedTransaction(
          UnconfirmedTransaction(highFeeNonWalletTx, None)
        )).mapTo[ProcessingOutcome],
        20.seconds
      )
      admitHigh shouldBe a[ProcessingOutcome.Accepted]

      val fullView = fetchView()
      fullView.pool.getAll.map(_.id) shouldBe Seq(highFeeNonWalletTx.id)
      val (_, capOutcome) = fullView.pool.process(UnconfirmedTransaction(lowFeeWalletTx, None), fullView.state)
      capOutcome shouldBe a[ProcessingOutcome.Declined]
      capOutcome.asInstanceOf[ProcessingOutcome.Declined].reason shouldBe
        ProcessingOutcome.DeclineReason.PoolCapacity
      capOutcome.retainPersistedWalletTx shouldBe true

      restarted ! ReceivableMessages.RestoredTransaction(UnconfirmedTransaction(lowFeeWalletTx, None))
      val afterRefusal = fetchView()
      afterRefusal.pool.getAll.map(_.id) shouldBe Seq(highFeeNonWalletTx.id)
      Await.result(afterRefusal.vault.unconfirmedTransactionsToRestore, 20.seconds)
        .map(_.id) shouldBe Seq(lowFeeWalletTx.id)
    }

    // Reopening with normal capacity restores the retained wallet transaction into the mempool.
    withRestartedView(fixture.settings, fixture.nodeViewDir) { (_, fetchView) =>
      eventually {
        val view = fetchView()
        view.pool.getAll.map(_.id) shouldBe Seq(lowFeeWalletTx.id)
      }
    }
  }

  property("restore dataInput-chained wallet transactions in topological order and retain MissingInputs records") {
    val fixture = new WalletFixture(settings, parameters, getCurrentView(_).vault)
    val baseFee = math.max(settings.nodeSettings.minimalFeeAmount, 1000000L)
    val feeProp = settings.chainSettings.monetary.feeProposition
    val feeAddr = Pay2SAddress(feeProp)

    val (parentTx, dataChildTx, missingInputTx) = try fixture.apply { implicit active =>
      val address = getPublicKeys.head
      applyBlock(makeGenesisBlock(address.pubkey)) shouldBe 'success
      val splitTx = eventually {
        val third = getConfirmedBalances.walletBalance / 3
        val requests = Seq(
          PaymentRequest(address, third, Array.empty, Map.empty),
          PaymentRequest(address, third, Array.empty, Map.empty),
          PaymentRequest(feeAddr, baseFee, Array.empty, Map.empty)
        )
        await(wallet.generateTransaction(requests)).get
      }
      applyBlock(makeNextBlock(getUtxoState, Seq(splitTx))) shouldBe 'success

      val walletBoxes = splitTx.outputs.filter(_.ergoTree == address.script)
      walletBoxes.size should be >= 2
      val box1 = walletBoxes(0)
      val box2 = walletBoxes(1)
      eventually {
        val unspentIds = await(wallet.walletBoxes(unspentOnly = true, considerUnconfirmed = false))
          .map(wb => Base16.encode(wb.trackedBox.box.id)).toSet
        unspentIds.contains(Base16.encode(box1.id)) shouldBe true
        unspentIds.contains(Base16.encode(box2.id)) shouldBe true
      }

      val parent = await(wallet.generateTransaction(
        requests = Seq(
          PaymentRequest(address, box1.value - baseFee, Array.empty, Map.empty),
          PaymentRequest(feeAddr, baseFee, Array.empty, Map.empty)
        ),
        inputsRaw = Seq(encodeBox(box1))
      )).get
      val parentOut = parent.outputs.find(_.ergoTree == address.script).get

      // Pick a child whose txId sorts lexicographically before parent.id in LevelDB so raw DB
      // iteration returns the child first; without dataInputs dependency ordering, validateWithCost
      // would invalidate and delete the child before the parent enters the mempool.
      val candidates = (0L to 64L).map { delta =>
        val fee = baseFee + delta * 1000L
        await(wallet.generateTransaction(
          requests = Seq(
            PaymentRequest(address, box2.value - fee, Array.empty, Map.empty),
            PaymentRequest(feeAddr, fee, Array.empty, Map.empty)
          ),
          inputsRaw = Seq(encodeBox(box2)),
          dataInputsRaw = Seq(encodeBox(parentOut))
        )).get
      }
      val child = candidates.find(c => unsignedLexLess(c.id, parent.id)).getOrElse(candidates.head)
      unsignedLexLess(child.id, parent.id) shouldBe true

      // Also persist an incoming wallet payment spending an external unconfirmed parent box not yet
      // present in UtxoState or mempool, exercising Declined(MissingInputs) on restore.
      val unknownExternalBoxId = ADKey @@ Array.fill[Byte](32)(0x5a.toByte)
      val missingParentChild = ErgoTransaction(
        IndexedSeq(new Input(unknownExternalBoxId, emptyProverResult)),
        IndexedSeq(
          new ErgoBoxCandidate(10000000L, address.script, creationHeight = 1),
          new ErgoBoxCandidate(baseFee, feeProp, creationHeight = 1)
        )
      )

      wallet.scanOffchain(child)
      wallet.scanOffchain(parent)
      wallet.scanOffchain(missingParentChild)
      eventually {
        val toRestore = await(wallet.unconfirmedTransactionsToRestore).map(_.id)
        toRestore.indexOf(parent.id) should be < toRestore.indexOf(child.id)
        toRestore should contain(missingParentChild.id)
      }
      (parent, child, missingParentChild)
    } finally {
      Await.result(fixture.actorSystem.whenTerminated, 30.seconds)
      closeStoresUnder(fixture.nodeViewDir)
    }

    withRestartedView(fixture.settings, fixture.nodeViewDir) { (restarted, fetchView) =>
      val view = eventually {
        val v = fetchView()
        v.pool.getAll.map(_.id).toSet shouldBe Set(parentTx.id, dataChildTx.id)
        v
      }
      val (_, missingOutcome) = view.pool.process(UnconfirmedTransaction(missingInputTx, None), view.state)
      missingOutcome shouldBe a[ProcessingOutcome.Declined]
      missingOutcome.asInstanceOf[ProcessingOutcome.Declined].reason shouldBe
        ProcessingOutcome.DeclineReason.MissingInputs
      missingOutcome.retainPersistedWalletTx shouldBe true

      restarted ! ReceivableMessages.RestoredTransaction(UnconfirmedTransaction(missingInputTx, None))
      val afterMissing = fetchView()
      afterMissing.pool.contains(missingInputTx.id) shouldBe false
      val retainedIds = Await.result(afterMissing.vault.unconfirmedTransactionsToRestore, 20.seconds).map(_.id)
      retainedIds.indexOf(parentTx.id) should be < retainedIds.indexOf(dataChildTx.id)
      retainedIds should contain(missingInputTx.id)
    }
  }

  property("forget persisted record and unconfirmed descendants on permanent rejection (Invalidated, MinFeeNotMet, MempoolPolicy)") {
    val fixture = new WalletFixture(settings, parameters, getCurrentView(_).vault)
    val baseFee = math.max(settings.nodeSettings.minimalFeeAmount, 1000000L)
    val feeProp = settings.chainSettings.monetary.feeProposition
    val feeAddr = Pay2SAddress(feeProp)

    val (address, boxAValue, blacklistedParentTx, childOfBlacklisted, underFeeTx, storageRentPolicyTx) =
      try fixture.apply { implicit active =>
        val addr = getPublicKeys.head
        applyBlock(makeGenesisBlock(addr.pubkey)) shouldBe 'success
        val splitTx = eventually {
          val quarter = getConfirmedBalances.walletBalance / 4
          val requests = Seq(
            PaymentRequest(addr, quarter, Array.empty, Map.empty),
            PaymentRequest(addr, quarter, Array.empty, Map.empty),
            PaymentRequest(Pay2SAddress(TrueTree), quarter, Array.empty, Map.empty),
            PaymentRequest(feeAddr, baseFee, Array.empty, Map.empty)
          )
          await(wallet.generateTransaction(requests)).get
        }
        applyBlock(makeNextBlock(getUtxoState, Seq(splitTx))) shouldBe 'success

        val walletBoxes = splitTx.outputs.filter(_.ergoTree == addr.script)
        val boxA = walletBoxes(0)
        val boxB = walletBoxes(1)
        val rentSourceBox = splitTx.outputs.find(_.ergoTree == TrueTree).get
        eventually {
          val unspentIds = await(wallet.walletBoxes(unspentOnly = true, considerUnconfirmed = false))
            .map(wb => Base16.encode(wb.trackedBox.box.id)).toSet
          unspentIds.contains(Base16.encode(boxA.id)) shouldBe true
          unspentIds.contains(Base16.encode(boxB.id)) shouldBe true
        }

        val parentFee = baseFee * 4
        val blacklistedParent = await(wallet.generateTransaction(
          requests = Seq(
            PaymentRequest(addr, boxA.value - parentFee, Array.empty, Map.empty),
            PaymentRequest(feeAddr, parentFee, Array.empty, Map.empty)
          ),
          inputsRaw = Seq(encodeBox(boxA))
        )).get
        val parentOut = blacklistedParent.outputs.find(_.ergoTree == addr.script).get

        val child = await(wallet.generateTransaction(
          requests = Seq(
            PaymentRequest(addr, parentOut.value - parentFee, Array.empty, Map.empty),
            PaymentRequest(feeAddr, parentFee, Array.empty, Map.empty)
          ),
          inputsRaw = Seq(encodeBox(parentOut))
        )).get

        val lowFee = await(wallet.generateTransaction(
          requests = Seq(
            PaymentRequest(addr, boxB.value - baseFee, Array.empty, Map.empty),
            PaymentRequest(feeAddr, baseFee, Array.empty, Map.empty)
          ),
          inputsRaw = Seq(encodeBox(boxB))
        )).get

        val rentClaimInput = new Input(
          rentSourceBox.id,
          ProverResult(
            Array.emptyByteArray,
            ContextExtension(Map(Constants.StorageIndexVarId -> ShortConstant(0)))
          )
        )
        val rentPolicyTx = ErgoTransaction(
          IndexedSeq(rentClaimInput),
          IndexedSeq(
            new ErgoBoxCandidate(rentSourceBox.value - parentFee, addr.script, creationHeight = 1),
            new ErgoBoxCandidate(parentFee, feeProp, creationHeight = 1)
          )
        )

        wallet.scanOffchain(blacklistedParent)
        wallet.scanOffchain(child)
        wallet.scanOffchain(lowFee)
        wallet.scanOffchain(rentPolicyTx)
        eventually {
          await(wallet.unconfirmedTransactionsToRestore).map(_.id).toSet shouldBe
            Set(blacklistedParent.id, child.id, lowFee.id, rentPolicyTx.id)
        }
        (addr, boxA.value, blacklistedParent, child, lowFee, rentPolicyTx)
      } finally {
        Await.result(fixture.actorSystem.whenTerminated, 30.seconds)
        closeStoresUnder(fixture.nodeViewDir)
      }

    val strictSettings = fixture.settings.copy(
      nodeSettings = fixture.settings.nodeSettings.copy(
        blacklistedTransactions = Seq(blacklistedParentTx.id),
        minimalFeeAmount = baseFee * 3,
        rejectStorageRentTxs = true
      )
    )

    withRestartedView(strictSettings, fixture.nodeViewDir) { (_, fetchView) =>
      eventually {
        val view = fetchView()
        view.pool.getAll shouldBe empty

        val (_, policyOutcome) = view.pool.process(
          UnconfirmedTransaction(storageRentPolicyTx, None),
          view.state
        )
        policyOutcome shouldBe a[ProcessingOutcome.Declined]
        policyOutcome.asInstanceOf[ProcessingOutcome.Declined].reason shouldBe
          ProcessingOutcome.DeclineReason.MempoolPolicy
        policyOutcome.retainPersistedWalletTx shouldBe false

        Await.result(view.vault.unconfirmedTransactionsToRestore, 20.seconds) shouldBe empty
        // Once the permanently rejected transactions and dependent child are forgotten, the
        // confirmed on-chain wallet inputs are released in OffChainRegistry and spendable again.
        val replacement = Await.result(
          view.vault.generateTransaction(
            Seq(
              PaymentRequest(address, boxAValue / 2, Array.empty, Map.empty),
              PaymentRequest(feeAddr, baseFee * 3, Array.empty, Map.empty)
            )
          ),
          20.seconds
        )
        replacement.isSuccess shouldBe true
        val newTx = replacement.get
        newTx.id should not be blacklistedParentTx.id
        newTx.id should not be childOfBlacklisted.id
        newTx.id should not be underFeeTx.id
        newTx.id should not be storageRentPolicyTx.id
      }
    }
  }
}
