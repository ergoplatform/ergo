package org.ergoplatform.nodeView.wallet

import org.ergoplatform.modifiers.mempool.ErgoTransaction
import org.ergoplatform.utils._
import org.ergoplatform.{ErgoBoxCandidate, Input}
import org.scalatest.PropSpec
import scorex.crypto.authds.ADKey
import scorex.crypto.hash.Blake2b256
import sigmastate.Values.{ByteArrayConstant, TrueLeaf}
import sigmastate._

import scala.concurrent.{Await, blocking}
import scala.util.Random


class ErgoWalletSpec extends PropSpec with WalletTestOps {

  private implicit val ergoAddressEncoder: ErgoAddressEncoder = new ErgoAddressEncoder(settings)

  property("off-chain scan") {
    withFixture { implicit w =>
      val pubKey = getTrackedAddresses.head.script
      val fakeInput = IndexedSeq(Input(ADKey @@ Array.fill(32)(0: Byte), emptyProverResult))

      val bs0 = getUnconfirmedBalances
      bs0.balance shouldBe 0
      bs0.assetBalances.isEmpty shouldBe true

      val balance1 = Random.nextInt(1000) + 1
      wallet.scanOffchain(ErgoTransaction(fakeInput, IndexedSeq(new ErgoBoxCandidate(balance1, pubKey))))

      blocking(Thread.sleep(1000))

      val bs1 = getUnconfirmedBalances
      bs1.balance shouldBe balance1
      bs1.assetBalances.isEmpty shouldBe true

      val balance2 = Random.nextInt(1000) + 1
      wallet.scanOffchain(ErgoTransaction(fakeInput, IndexedSeq(new ErgoBoxCandidate(balance2, pubKey))))

      blocking(Thread.sleep(1000))

      val bs2 = getUnconfirmedBalances
      bs2.balance shouldBe (balance1 + balance2)
      bs2.assetBalances.isEmpty shouldBe true

      wallet.watchFor(Pay2SAddress(Values.TrueLeaf))
      val balance3 = Random.nextInt(1000) + 1
      wallet.scanOffchain(ErgoTransaction(fakeInput, IndexedSeq(new ErgoBoxCandidate(balance3, Values.TrueLeaf))))

      blocking(Thread.sleep(1000))

      val bs3 = getUnconfirmedBalances
      bs3.balance shouldBe (balance1 + balance2 + balance3)
      bs3.assetBalances.isEmpty shouldBe true

      //todo: enhance the test, e.g. add assets
    }
  }

  property("off-chain box spending") {
    withFixture { implicit w =>
      val p2pk = getTrackedAddresses.head
      val tx = makeGenesisTx(p2pk.script)
      wallet.scanOffchain(tx)
      val boxesToSpend = boxesAvailable(tx, p2pk.script)
      val balanceToSpend = sum(boxesToSpend)
      blocking(Thread.sleep(offchainScanTime(tx)))
      val unconfirmedBalance = getUnconfirmedBalances.balance
      unconfirmedBalance shouldEqual balanceToSpend

      val balanceToReturn = randomInt(balanceToSpend)
      val spendingTx = makeSpendingTx(boxesToSpend, p2pk, balanceToReturn)
      wallet.scanOffchain(spendingTx)
      blocking(Thread.sleep(offchainScanTime(tx)))

      val balanceAfterSpending = getUnconfirmedBalances.balance
      log.info(s"Unconfirmed balance: $unconfirmedBalance")
      log.info(s"Balance to spent: $balanceToSpend")
      log.info(s"Balance to return back: $balanceToReturn")
      balanceAfterSpending shouldEqual (unconfirmedBalance - balanceToSpend + balanceToReturn)
    }
  }

  property("on-chain box spending") {
    withFixture { implicit w =>
      val p2pk = getTrackedAddresses.head
      val genesisBlock = makeGenesisBlock(p2pk.script)
      applyBlock(genesisBlock) shouldBe 'success
      wallet.scanPersistent(genesisBlock)
      blocking(Thread.sleep(scanTime(genesisBlock)))

      val confirmedBalance = getConfirmedBalances.balance
      val boxesToSpend = boxesAvailable(genesisBlock, p2pk.script)
      val balanceToSpend = sum(boxesToSpend)
      log.info(s"Confirmed balance $confirmedBalance")
      log.info(s"Sum balance: $balanceToSpend")
      confirmedBalance should be > 0L
      confirmedBalance shouldBe balanceToSpend

      val balanceToReturn = randomInt(balanceToSpend)
      val spendingTx = makeSpendingTx(boxesToSpend, p2pk, balanceToReturn)

      val spendingBlock = makeNextBlock(getUtxoState, Seq(spendingTx)) // throws Exception: Key ... does not exist
      applyBlock(spendingBlock) shouldBe 'success
      wallet.scanPersistent(spendingBlock)
      blocking(Thread.sleep(scanTime(spendingBlock)))

      val balanceAfterSpending = getConfirmedBalances.balance
      log.info(s"Unconfirmed balance: $confirmedBalance")
      log.info(s"Balance to spent: $balanceToSpend")
      log.info(s"Balance to return back: $balanceToReturn")
      balanceAfterSpending shouldEqual (confirmedBalance - balanceToSpend + balanceToReturn)
    }
  }

  property("off-chain transaction becomes on-chain") {
    withFixture { implicit w =>
      val pubKey = getTrackedAddresses.head.script
      val tx = makeGenesisTx(pubKey)
      wallet.scanOffchain(tx)
      blocking(Thread.sleep(offchainScanTime(tx)))
      val sumBalance = sum(boxesAvailable(tx, pubKey))
      val unconfirmedBalance = getUnconfirmedBalances.balance
      unconfirmedBalance shouldBe sumBalance

      val block = makeNextBlock(getUtxoState, Seq(tx))
      applyBlock(block) shouldBe 'success
      wallet.scanPersistent(block)
      blocking(Thread.sleep(scanTime(block)))
      val confirmedBalance = getConfirmedBalances.balance
      log.info(s"Confirmed balance $confirmedBalance")
      log.info(s"Sum balance: $sumBalance")
      confirmedBalance should be > 0L
      confirmedBalance shouldBe sumBalance
      confirmedBalance shouldBe unconfirmedBalance

      getUnconfirmedBalances.balance shouldBe 0L
    }
  }

  property("on-chain rollback") {
    withFixture { implicit w =>
      val pubKey = getTrackedAddresses.head.script
      val genesisBlock = makeGenesisBlock(TrueLeaf)
      val boxesToSpend = boxesAvailable(genesisBlock, TrueLeaf)

      applyBlock(genesisBlock) shouldBe 'success
      val initialState = getCurrentState
      val initialHeight = getHistory.heightOf(scorex.core.versionToId(initialState.version))
      wallet.scanPersistent(genesisBlock)
      blocking(Thread.sleep(scanTime(genesisBlock)))
      val initialBalance = getConfirmedBalances.balance

      val balance = randomInt(sum(boxesToSpend))
      val spendingTx = makeSpendingTx(boxesToSpend, emptyProverResult, balance, pubKey)
      val block = makeNextBlock(getUtxoState, Seq(spendingTx))
      wallet.scanPersistent(block)
      blocking(Thread.sleep(scanTime(block)))
      val historyHeight = getHistory.headersHeight

      val balanceBeforeRollback = getConfirmedBalances.balance
      val unconfirmedBeforeRollback = getUnconfirmedBalances.balance

      wallet.rollback(initialState.version)
      blocking(Thread.sleep(100))
      val balanceAfterRollback = getConfirmedBalances.balance
      val unconfirmedAfterRollback = getUnconfirmedBalances.balance

      log.info(s"Initial height: $initialHeight")
      log.info(s"Initial balance: $initialBalance")
      log.info(s"History height: $historyHeight")
      log.info(s"Confirmed balance: $balanceBeforeRollback")
      log.info(s"Unconfirmed balance: $unconfirmedBeforeRollback")
      log.info(s"Balance after rollback: $balanceAfterRollback")
      log.info(s"Unconfirmed balance after rollback: $unconfirmedAfterRollback")

      initialBalance shouldBe 0L

      balanceBeforeRollback shouldBe balance
      unconfirmedBeforeRollback shouldBe 0L

      balanceAfterRollback shouldBe 0L
      unconfirmedAfterRollback shouldBe balance
    }
  }

  property("on-chain spending rollback") {
    withFixture { implicit w =>
      val p2pk = getTrackedAddresses.head
      val genesisBlock = makeGenesisBlock(p2pk.script)
      val boxesToSpend = boxesAvailable(genesisBlock, p2pk.script)
      val sumBalance = sum(boxesToSpend)

      applyBlock(genesisBlock) shouldBe 'success
      val initialState = getCurrentState
      val initialHeight = getHistory.heightOf(scorex.core.versionToId(initialState.version))
      wallet.scanPersistent(genesisBlock)
      blocking(Thread.sleep(scanTime(genesisBlock)))
      val initialBalance = getConfirmedBalances.balance

      val spendingTx = makeSpendingTx(boxesToSpend, p2pk)
      val block = makeNextBlock(getUtxoState, Seq(spendingTx))
      wallet.scanPersistent(block)
      blocking(Thread.sleep(scanTime(block)))
      val historyHeight = getHistory.headersHeight

      val balanceBeforeRollback = getConfirmedBalances.balance
      val unconfirmedBeforeRollback = getUnconfirmedBalances.balance

      wallet.rollback(initialState.version)
      blocking(Thread.sleep(100))
      val balanceAfterRollback = getConfirmedBalances.balance
      val unconfirmedAfterRollback = getUnconfirmedBalances.balance

      log.info(s"Initial height: $initialHeight")
      log.info(s"Initial balance: $initialBalance")
      log.info(s"Balance to spend: $sumBalance")
      log.info(s"History height: $historyHeight")
      log.info(s"Confirmed balance: $balanceBeforeRollback")
      log.info(s"Unconfirmed balance: $unconfirmedBeforeRollback")
      log.info(s"Balance after rollback: $balanceAfterRollback")
      log.info(s"Unconfirmed balance after rollback: $unconfirmedAfterRollback")

      initialBalance shouldBe sumBalance

      balanceBeforeRollback shouldBe 0L
      unconfirmedBeforeRollback shouldBe 0L

      balanceAfterRollback shouldBe initialBalance
      unconfirmedAfterRollback shouldBe 0L
    }
  }

  property("on-chain spending with return rollback") {
    withFixture { implicit w =>
      val p2pk = getTrackedAddresses.head
      val genesisBlock = makeGenesisBlock(p2pk.script)
      val boxesToSpend = boxesAvailable(genesisBlock, p2pk.script)
      val sumBalance = sum(boxesToSpend)

      applyBlock(genesisBlock) shouldBe 'success
      val initialState = getCurrentState
      val initialHeight = getHistory.heightOf(scorex.core.versionToId(initialState.version))
      wallet.scanPersistent(genesisBlock)
      blocking(Thread.sleep(scanTime(genesisBlock)))
      val initialBalance = getConfirmedBalances.balance

      val balanceToReturn = randomInt(sumBalance)
      val spendingTx = makeSpendingTx(boxesToSpend, p2pk, balanceToReturn)
      val block = makeNextBlock(getUtxoState, Seq(spendingTx))
      wallet.scanPersistent(block)
      blocking(Thread.sleep(scanTime(block)))
      val historyHeight = getHistory.headersHeight

      val balanceBeforeRollback = getConfirmedBalances.balance
      val unconfirmedBeforeRollback = getUnconfirmedBalances.balance

      wallet.rollback(initialState.version)
      blocking(Thread.sleep(100))
      val balanceAfterRollback = getConfirmedBalances.balance
      val unconfirmedAfterRollback = getUnconfirmedBalances.balance

      log.info(s"Initial height: $initialHeight")
      log.info(s"Initial balance: $initialBalance")
      log.info(s"Balance to spend: $sumBalance")
      log.info(s"Balance to return $balanceToReturn")
      log.info(s"History height: $historyHeight")
      log.info(s"Confirmed balance: $balanceBeforeRollback")
      log.info(s"Unconfirmed balance: $unconfirmedBeforeRollback")
      log.info(s"Balance after rollback: $balanceAfterRollback")
      log.info(s"Unconfirmed balance after rollback: $unconfirmedAfterRollback")

      initialBalance shouldBe sumBalance

      balanceBeforeRollback should be > 0L
      balanceBeforeRollback shouldBe balanceToReturn
      unconfirmedBeforeRollback shouldBe 0L

      balanceAfterRollback shouldBe initialBalance
      unconfirmedAfterRollback shouldBe balanceToReturn
    }
  }

  property("successfully generates a transaction") {
    withFixture { implicit w =>
      val pubKey = getTrackedAddresses.head.script
      val genesisBlock = makeGenesisBlock(pubKey)
      applyBlock(genesisBlock) shouldBe 'success
      wallet.scanPersistent(genesisBlock)
      blocking(Thread.sleep(scanTime(genesisBlock)))
      val confirmedBalance = getConfirmedBalances.balance

      //pay out all the wallet balance:
      val req1 = PaymentRequest(Pay2SAddress(Values.FalseLeaf), confirmedBalance, None, None)

      val tx1 = Await.result(wallet.generateTransaction(Seq(req1)), awaitDuration).get
      tx1.outputs.size shouldBe 1
      tx1.outputs.head.value shouldBe confirmedBalance

      //change == 1:
      val req2 = PaymentRequest(Pay2SAddress(Values.FalseLeaf), confirmedBalance - 1, None, None)

      val tx2 = Await.result(wallet.generateTransaction(Seq(req2)), awaitDuration).get
      tx2.outputs.size shouldBe 2
      tx2.outputs.head.value shouldBe confirmedBalance - 1
      tx2.outputs(1).value shouldBe 1
    }
  }

  property("watchFor") {
    withFixture { implicit w =>
      val preimage = ByteArrayConstant("hello world".getBytes("UTF-8"))
      val hash = Blake2b256(preimage.value)
      val p2s = Pay2SAddress(EQ(CalcBlake2b256(preimage), hash))

      val initialState = getCurrentState
      val genesisBlock = makeGenesisBlock(p2s.script)
      applyBlock(genesisBlock) shouldBe 'success
      wallet.scanPersistent(genesisBlock)
      blocking(Thread.sleep(scanTime(genesisBlock)))
      val confirmedBalance = getConfirmedBalances.balance
      val sumOutputs = sum(boxesAvailable(genesisBlock, p2s.script))
      confirmedBalance should be < sumOutputs

      wallet.rollback(initialState.version)
      blocking(Thread.sleep(100))

      wallet.watchFor(p2s)

      wallet.scanPersistent(genesisBlock)
      blocking(Thread.sleep(scanTime(genesisBlock)))
      val confirmedBalance2 = getConfirmedBalances.balance
      confirmedBalance2 shouldBe sumOutputs
    }
  }

}
