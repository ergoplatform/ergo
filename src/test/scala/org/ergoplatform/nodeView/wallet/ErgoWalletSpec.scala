package org.ergoplatform.nodeView.wallet

import org.ergoplatform.modifiers.mempool.ErgoTransaction
import org.ergoplatform.nodeView.state.ErgoStateContext
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


  property("Generate transaction with multiple inputs") {
    withFixture { implicit w =>
      val addresses = getTrackedAddresses
      addresses.length should be > 1
      val genesisBlock = makeGenesisBlock(addresses.head.script)
      val genesisTx = genesisBlock.transactions.head
      applyBlock(genesisBlock) shouldBe 'success
      wallet.scanPersistent(genesisBlock)
      blocking(Thread.sleep(scanTime(genesisBlock)))
      val confirmedBalance = getConfirmedBalances.balance
      val requests = addresses map (a => PaymentRequest(a, confirmedBalance / (addresses.length + 1), None, None))
      val tx = Await.result(wallet.generateTransaction(requests), awaitDuration).get
      val context = ErgoStateContext(genesisBlock.header.height, genesisBlock.header.stateRoot)
      val boxesToSpend = tx.inputs.map(i => genesisTx.outputs.find(_.id sameElements i.boxId).get)
      tx.statefulValidity(boxesToSpend, context) shouldBe 'success

      val block = makeNextBlock(getUtxoState, Seq(tx))
      applyBlock(block) shouldBe 'success
      wallet.scanPersistent(block)
      blocking(Thread.sleep(scanTime(block)))
      val newBalance = getConfirmedBalances.balance
      val requests2 = addresses map (a => PaymentRequest(a, newBalance / (addresses.length + 1), None, None))
      val tx2 = Await.result(wallet.generateTransaction(requests2), awaitDuration).get
      val context2 = ErgoStateContext(block.header.height, block.header.stateRoot)
      val boxesToSpend2 = tx2.inputs.map(i => tx.outputs.find(_.id sameElements i.boxId).get)
      tx2.statefulValidity(boxesToSpend2, context2) shouldBe 'success
    }
  }

  property("off-chain scan") {
    withFixture { implicit w =>
      val pubKey = getTrackedAddresses.head.script
      val fakeInput = IndexedSeq(Input(ADKey @@ Array.fill(32)(0: Byte), emptyProverResult))

      val bs0 = getBalancesWithUnconfirmed
      bs0.balance shouldBe 0
      bs0.assetBalances.isEmpty shouldBe true

      val balance1 = Random.nextInt(1000) + 1
      wallet.scanOffchain(ErgoTransaction(fakeInput, IndexedSeq(new ErgoBoxCandidate(balance1, pubKey))))

      blocking(Thread.sleep(1000))

      val bs1 = getBalancesWithUnconfirmed
      bs1.balance shouldBe balance1
      bs1.assetBalances.isEmpty shouldBe true

      val balance2 = Random.nextInt(1000) + 1
      wallet.scanOffchain(ErgoTransaction(fakeInput, IndexedSeq(new ErgoBoxCandidate(balance2, pubKey))))

      blocking(Thread.sleep(1000))

      val bs2 = getBalancesWithUnconfirmed
      bs2.balance shouldBe (balance1 + balance2)
      bs2.assetBalances.isEmpty shouldBe true

      wallet.watchFor(Pay2SAddress(Values.TrueLeaf))
      val balance3 = Random.nextInt(1000) + 1
      wallet.scanOffchain(ErgoTransaction(fakeInput, IndexedSeq(new ErgoBoxCandidate(balance3, Values.TrueLeaf))))

      blocking(Thread.sleep(1000))

      val bs3 = getBalancesWithUnconfirmed
      bs3.balance shouldBe (balance1 + balance2 + balance3)
      bs3.assetBalances.isEmpty shouldBe true

      //todo: enhance the test, e.g. add assets
    }
  }

  property("off-chain box spending") {
    withFixture { implicit w =>
      val address = getTrackedAddresses.head
      val tx = makeGenesisTx(address.script)
      wallet.scanOffchain(tx)
      val boxesToSpend = boxesAvailable(tx, address.script)
      val balanceToSpend = sum(boxesToSpend)
      blocking(Thread.sleep(offchainScanTime(tx)))
      val totalBalance = getBalancesWithUnconfirmed.balance
      totalBalance shouldEqual balanceToSpend

      val balanceToReturn = randomLong(balanceToSpend)
      val spendingTx = makeSpendingTx(boxesToSpend, address, balanceToReturn)
      wallet.scanOffchain(spendingTx)
      blocking(Thread.sleep(offchainScanTime(tx)))
      val balanceAfterSpending = getBalancesWithUnconfirmed.balance

      log.info(s"Total balance with unconfirmed: $totalBalance")
      log.info(s"Balance to spent: $balanceToSpend")
      log.info(s"Balance to return back: $balanceToReturn")
      balanceAfterSpending shouldEqual balanceToReturn
    }
  }

  property("off-chain double registration") {
    withFixture { implicit w =>
      val address = getTrackedAddresses.head
      val tx = makeGenesisTx(address.script)
      wallet.scanOffchain(tx)
      val boxesToSpend = boxesAvailable(tx, address.script)
      val balanceToSpend = sum(boxesToSpend)
      blocking(Thread.sleep(offchainScanTime(tx)))
      val totalBalance = getBalancesWithUnconfirmed.balance

      val balanceToReturn = randomLong(balanceToSpend)
      val spendingTx = makeSpendingTx(boxesToSpend, address, balanceToReturn)
//      val doubleSpendingTx = makeSpendingTx(boxesToSpend, address, randomLong(balanceToSpend))
      wallet.scanOffchain(Seq(spendingTx, spendingTx))
      wallet.scanOffchain(spendingTx)
      blocking(Thread.sleep(offchainScanTime(tx) * 3))
      val balanceAfterSpending = getBalancesWithUnconfirmed.balance

      log.info(s"Total with unconfirmed balance: $totalBalance")
      log.info(s"Balance to spent: $balanceToSpend")
      log.info(s"Balance to return back: $balanceToReturn")
      totalBalance shouldEqual balanceToSpend

      balanceAfterSpending shouldEqual balanceToReturn
    }
  }

  property("off-chain spending of the on-chain box") {
    withFixture { implicit w =>
      val address = getTrackedAddresses.head
      val genesisBlock = makeGenesisBlock(address.script)
      val boxesToSpend = boxesAvailable(genesisBlock, address.script)
      val sumBalance = sum(boxesToSpend)
      applyBlock(genesisBlock) shouldBe 'success
      wallet.scanPersistent(genesisBlock)
      blocking(Thread.sleep(scanTime(genesisBlock)))
      val totalBalance = getBalancesWithUnconfirmed.balance
      val confirmedBalance = getConfirmedBalances.balance

      val balanceToReturn = randomLong(sumBalance)
      val spendingTx = makeSpendingTx(boxesToSpend, address, balanceToReturn)
      wallet.scanOffchain(spendingTx)
      blocking(Thread.sleep(offchainScanTime(spendingTx)))
      val confirmedAfterSpending = getConfirmedBalances.balance
      val totalAfterSpending = getBalancesWithUnconfirmed.balance

      log.info(s"Sum balance: $sumBalance")
      log.info(s"Balance before spending: $confirmedBalance")
      log.info(s"Total with unconfirmed balance before spending: $totalBalance")
      log.info(s"Balance after spending: $confirmedAfterSpending")
      log.info(s"Total with unconfirmed after spending: $totalAfterSpending")

      confirmedBalance shouldBe sumBalance
      totalBalance shouldBe sumBalance
      confirmedAfterSpending shouldBe 0L
      totalAfterSpending shouldBe balanceToReturn
    }
  }

  property("on-chain box spending") {
    withFixture { implicit w =>
      val address = getTrackedAddresses.head
      val genesisBlock = makeGenesisBlock(address.script)
      applyBlock(genesisBlock) shouldBe 'success
      wallet.scanPersistent(genesisBlock)
      blocking(Thread.sleep(scanTime(genesisBlock)))

      val confirmedBalance = getConfirmedBalances.balance
      val boxesToSpend = boxesAvailable(genesisBlock, address.script)
      val balanceToSpend = sum(boxesToSpend)
      log.info(s"Confirmed balance $confirmedBalance")
      log.info(s"Sum balance: $balanceToSpend")
      confirmedBalance should be > 0L
      confirmedBalance shouldBe balanceToSpend

      val balanceToReturn = randomLong(balanceToSpend)
      val spendingTx = makeSpendingTx(boxesToSpend, address, balanceToReturn)

      val spendingBlock = makeNextBlock(getUtxoState, Seq(spendingTx)) // throws Exception: Key ... does not exist
      applyBlock(spendingBlock) shouldBe 'success
      wallet.scanPersistent(spendingBlock)
      blocking(Thread.sleep(scanTime(spendingBlock)))

      val balanceAfterSpending = getConfirmedBalances.balance
      log.info(s"Total with unconfirmed balance: $confirmedBalance")
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
      val initialBalance = getBalancesWithUnconfirmed.balance
      initialBalance shouldBe sumBalance

      val block = makeNextBlock(getUtxoState, Seq(tx))
      applyBlock(block) shouldBe 'success
      wallet.scanPersistent(block)
      blocking(Thread.sleep(scanTime(block)))
      val confirmedBalance = getConfirmedBalances.balance
      log.info(s"Confirmed balance $confirmedBalance")
      log.info(s"Sum balance: $sumBalance")
      confirmedBalance should be > 0L
      confirmedBalance shouldBe sumBalance
      confirmedBalance shouldBe initialBalance

      getBalancesWithUnconfirmed.balance shouldBe sumBalance
    }
  }

  property("off-chain spending rollback") {
    withFixture { implicit w =>
      val address = getTrackedAddresses.head
      val genesisBlock = makeGenesisBlock(TrueLeaf)
      val boxesToSpend = boxesAvailable(genesisBlock, TrueLeaf)
      applyBlock(genesisBlock) shouldBe 'success
      val initialState = getCurrentState

      val sumBalance = randomLong(sum(boxesToSpend))
      val creationTx = makeTx(boxesToSpend, emptyProverResult, sumBalance, address.script)
      val boxesCreated = boxesAvailable(creationTx, address.script)
      val block = makeNextBlock(getUtxoState, Seq(creationTx))
      applyBlock(block) shouldBe 'success
      wallet.scanPersistent(block)
      blocking(Thread.sleep(scanTime(block)))
      val totalBalance = getBalancesWithUnconfirmed.balance
      val confirmedBalance = getConfirmedBalances.balance

      val balanceToReturn = randomLong(sum(boxesCreated))
      val spendingTx = makeSpendingTx(boxesCreated, address, balanceToReturn)
      wallet.scanOffchain(spendingTx)
      blocking(Thread.sleep(offchainScanTime(spendingTx)))
      val confirmedAfterSpending = getConfirmedBalances.balance
      val totalAfterSpending = getBalancesWithUnconfirmed.balance

      wallet.rollback(initialState.version)
      val balanceAfterRollback = getConfirmedBalances.balance
      val totalAfterRollback = getBalancesWithUnconfirmed.balance

      log.info(s"Sum balance: $sumBalance")
      log.info(s"Balance before spending: $confirmedBalance")
      log.info(s"Total with unconfirmed balance before spending: $totalBalance")
      log.info(s"After spending before rollback: $confirmedAfterSpending")
      log.info(s"Total with unconfirmed balance after spending before rollback: $totalAfterSpending")
      log.info(s"Balance after rollback: $balanceAfterRollback")
      log.info(s"Total with unconfirmed balance after rollback: $totalAfterRollback")

      confirmedBalance shouldBe sumBalance
      totalBalance shouldBe sumBalance
      confirmedAfterSpending shouldBe 0L
      totalAfterSpending shouldBe balanceToReturn
      balanceAfterRollback shouldBe 0L
      totalAfterRollback shouldBe balanceToReturn
    }
  }

  property("on-chain rollback") {
    withFixture { implicit w =>
      val pubKey = getTrackedAddresses.head.script
      val genesisBlock = makeGenesisBlock(TrueLeaf)
      val boxesToSpend = boxesAvailable(genesisBlock, TrueLeaf)
      applyBlock(genesisBlock) shouldBe 'success
      val initialState = getCurrentState
      val initialBalance = getConfirmedBalances.balance

      val balance = randomLong(sum(boxesToSpend))
      val creationTx = makeTx(boxesToSpend, emptyProverResult, balance, pubKey)
      val block = makeNextBlock(getUtxoState, Seq(creationTx))
      wallet.scanPersistent(block)
      blocking(Thread.sleep(scanTime(block)))
      val historyHeight = getHistory.headersHeight

      val balanceBeforeRollback = getConfirmedBalances.balance
      val totalBeforeRollback = getBalancesWithUnconfirmed.balance

      wallet.rollback(initialState.version)
      blocking(Thread.sleep(100))
      val balanceAfterRollback = getConfirmedBalances.balance
      val totalAfterRollback = getBalancesWithUnconfirmed.balance

      log.info(s"Initial balance: $initialBalance")
      log.info(s"History height: $historyHeight")
      log.info(s"Confirmed balance: $balanceBeforeRollback")
      log.info(s"Total with unconfirmed balance: $totalBeforeRollback")
      log.info(s"Balance after rollback: $balanceAfterRollback")
      log.info(s"Total with unconfirmed balance after rollback: $totalAfterRollback")

      initialBalance shouldBe 0L

      balanceBeforeRollback shouldBe balance
      totalBeforeRollback shouldBe balance

      balanceAfterRollback shouldBe 0L
      totalAfterRollback shouldBe balance
    }
  }

  property("on-chain spending rollback") {
    withFixture { implicit w =>
      val address = getTrackedAddresses.head
      val genesisBlock = makeGenesisBlock(address.script)
      val boxesToSpend = boxesAvailable(genesisBlock, address.script)
      val sumBalance = sum(boxesToSpend)

      applyBlock(genesisBlock) shouldBe 'success
      val initialState = getCurrentState
      wallet.scanPersistent(genesisBlock)
      blocking(Thread.sleep(scanTime(genesisBlock)))
      val initialBalance = getConfirmedBalances.balance

      val spendingTx = makeSpendingTx(boxesToSpend, address)
      val block = makeNextBlock(getUtxoState, Seq(spendingTx))
      wallet.scanPersistent(block)
      blocking(Thread.sleep(scanTime(block)))
      val historyHeight = getHistory.headersHeight

      val balanceBeforeRollback = getConfirmedBalances.balance
      val totalBeforeRollback = getBalancesWithUnconfirmed.balance

      wallet.rollback(initialState.version)
      blocking(Thread.sleep(100))
      val balanceAfterRollback = getConfirmedBalances.balance
      val totalAfterRollback = getBalancesWithUnconfirmed.balance

      log.info(s"Initial balance: $initialBalance")
      log.info(s"Balance to spend: $sumBalance")
      log.info(s"History height: $historyHeight")
      log.info(s"Confirmed balance: $balanceBeforeRollback")
      log.info(s"Total with unconfirmed balance: $totalBeforeRollback")
      log.info(s"Balance after rollback: $balanceAfterRollback")
      log.info(s"Total with unconfirmed balance after rollback: $totalAfterRollback")

      initialBalance shouldBe sumBalance

      balanceBeforeRollback shouldBe 0L
      totalBeforeRollback shouldBe 0L

      balanceAfterRollback shouldBe initialBalance
      totalAfterRollback shouldBe initialBalance
    }
  }

  property("on-chain spending with return rollback") {
    withFixture { implicit w =>
      val address = getTrackedAddresses.head
      val genesisBlock = makeGenesisBlock(address.script)
      val boxesToSpend = boxesAvailable(genesisBlock, address.script)
      val sumBalance = sum(boxesToSpend)

      applyBlock(genesisBlock) shouldBe 'success
      val initialState = getCurrentState
      wallet.scanPersistent(genesisBlock)
      blocking(Thread.sleep(scanTime(genesisBlock)))
      val initialBalance = getConfirmedBalances.balance

      val balanceToReturn = randomLong(sumBalance)
      val spendingTx = makeSpendingTx(boxesToSpend, address, balanceToReturn)
      val block = makeNextBlock(getUtxoState, Seq(spendingTx))
      wallet.scanPersistent(block)
      blocking(Thread.sleep(scanTime(block)))
      val historyHeight = getHistory.headersHeight

      val balanceBeforeRollback = getConfirmedBalances.balance
      val totalBeforeRollback = getBalancesWithUnconfirmed.balance

      wallet.rollback(initialState.version)
      blocking(Thread.sleep(100))
      val balanceAfterRollback = getConfirmedBalances.balance
      val totalAfterRollback = getBalancesWithUnconfirmed.balance

      log.info(s"Initial balance: $initialBalance")
      log.info(s"Balance to spend: $sumBalance")
      log.info(s"Balance to return $balanceToReturn")
      log.info(s"History height: $historyHeight")
      log.info(s"Confirmed balance: $balanceBeforeRollback")
      log.info(s"Total with unconfirmed balance: $totalBeforeRollback")
      log.info(s"Balance after rollback: $balanceAfterRollback")
      log.info(s"Total with unconfirmed balance after rollback: $totalAfterRollback")

      initialBalance shouldBe sumBalance

      balanceBeforeRollback should be > 0L
      balanceBeforeRollback shouldBe balanceToReturn
      totalBeforeRollback shouldBe balanceToReturn

      balanceAfterRollback shouldBe initialBalance
      totalAfterRollback shouldBe initialBalance + balanceToReturn
    }
  }

  property("on-chain spending to off-chain rollback") {
    withFixture { implicit w =>
      val address = getTrackedAddresses.head
      val genesisBlock = makeGenesisBlock(TrueLeaf)
      val boxesToSpend = boxesAvailable(genesisBlock, TrueLeaf)
      applyBlock(genesisBlock) shouldBe 'success
      val initialState = getCurrentState

      val balance = randomLong(sum(boxesToSpend))
      val creationTx = makeTx(boxesToSpend, emptyProverResult, balance, address.script)
      val boxesCreated = boxesAvailable(creationTx, address.script)
      val balanceCreated = sum(boxesCreated)
      val balanceToReturn = randomLong(balanceCreated)
      val spendingTx = makeSpendingTx(boxesCreated, address, balanceToReturn)
      val block = makeNextBlock(getUtxoState, Seq(creationTx, spendingTx))
      wallet.scanPersistent(block)
      blocking(Thread.sleep(scanTime(block)))
      val historyHeight = getHistory.headersHeight

      val balanceBeforeRollback = getConfirmedBalances.balance
      val totalBeforeRollback = getBalancesWithUnconfirmed.balance

      wallet.rollback(initialState.version)
      blocking(Thread.sleep(100))
      val balanceAfterRollback = getConfirmedBalances.balance
      val totalAfterRollback = getBalancesWithUnconfirmed.balance

      log.info(s"Balance created: $balanceCreated")
      log.info(s"History height: $historyHeight")
      log.info(s"Confirmed balance: $balanceBeforeRollback")
      log.info(s"Total with unconfirmed balance: $totalBeforeRollback")
      log.info(s"Balance after rollback: $balanceAfterRollback")
      log.info(s"Total with unconfirmed balance after rollback: $totalAfterRollback")

      balanceCreated shouldBe balance
      balanceBeforeRollback shouldBe balanceToReturn
      totalBeforeRollback shouldBe balanceToReturn

      balanceAfterRollback shouldBe 0L
      totalAfterRollback shouldBe balance + balanceToReturn
    }
  }

  property("single-input transaction generation") {
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
