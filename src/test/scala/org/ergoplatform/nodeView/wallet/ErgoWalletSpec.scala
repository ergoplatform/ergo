package org.ergoplatform.nodeView.wallet

import org.ergoplatform._
import org.ergoplatform.modifiers.mempool.{ErgoTransaction, UnsignedErgoTransaction}
import org.ergoplatform.nodeView.state.{ErgoStateContext, VotingData}
import org.ergoplatform.nodeView.wallet.IdUtils._
import org.ergoplatform.nodeView.wallet.persistence.RegistryIndex
import org.ergoplatform.nodeView.wallet.requests.{AssetIssueRequest, PaymentRequest}
import org.ergoplatform.settings.{Constants, LaunchParameters}
import org.ergoplatform.utils._
import org.ergoplatform.wallet.interpreter.{ErgoInterpreter, ErgoUnsafeProver}
import org.scalatest.PropSpec
import scorex.crypto.authds.ADKey
import scorex.crypto.hash.Blake2b256
import scorex.util.encode.Base16
import sigmastate.Values.ByteArrayConstant
import sigmastate._
import sigmastate.basics.DLogProtocol.DLogProverInput
import sigmastate.eval._
import sigmastate.eval.Extensions._
import sigmastate.Values._

import scala.concurrent.blocking
import scala.util.Random
import org.ergoplatform.wallet.boxes.BoxSelector.MinBoxValue
import org.ergoplatform.wallet.boxes.ErgoBoxSerializer


class ErgoWalletSpec extends PropSpec with WalletTestOps {

  private implicit val verifier: ErgoInterpreter = ErgoInterpreter(LaunchParameters)

  property("uncertain boxes spending") {
    withFixture { implicit w =>
      val walletPk = getPublicKeys.head.pubkey

      val foreignSecret = DLogProverInput.random()
      val foreignPk = foreignSecret.publicImage
      val uncertainProp = SigmaOr(SigmaAnd(walletPk, GE(IntConstant(0), IntConstant(10))), foreignPk)

      val genesisBlock = makeGenesisBlock(walletPk, randomNewAsset)

      applyBlock(genesisBlock) shouldBe 'success
      waitForScanning(genesisBlock)

      val confirmedBalance = getConfirmedBalances.balance
      val balanceToMakeUncertain = confirmedBalance - MinBoxValue
      val balanceAfterSpending = confirmedBalance - balanceToMakeUncertain

      val req = PaymentRequest(Pay2SAddress(uncertainProp), balanceToMakeUncertain, Seq.empty, Map.empty)
      val tx = await(wallet.generateTransaction(Seq(req))).get

      val block = makeNextBlock(getUtxoState, Seq(tx))
      applyBlock(block) shouldBe 'success
      waitForScanning(block)

      val index = await(wallet.confirmedBalances)

      index.uncertainBoxes.size shouldBe 1
      index.balance shouldBe balanceAfterSpending

      val uncertainTxUnsigned = new UnsignedErgoTransaction(
        index.uncertainBoxes.map(id => new UnsignedInput(decodedBoxId(id))).toIndexedSeq,
        IndexedSeq(),
        IndexedSeq(new ErgoBoxCandidate(balanceToMakeUncertain, Constants.TrueLeaf, block.height + 1))
      )
      val uncertainTx = ErgoUnsafeProver.prove(uncertainTxUnsigned, foreignSecret)

      val finalBlock = makeNextBlock(getUtxoState, Seq(ErgoTransaction(uncertainTx)))
      applyBlock(finalBlock) shouldBe 'success
      waitForScanning(finalBlock)

      val finalIndex = await(wallet.confirmedBalances)

      finalIndex.uncertainBoxes shouldBe empty
      finalIndex.balance shouldBe balanceAfterSpending
    }
  }

  property("do not use inputs spent in off-chain transaction") {
    withFixture { implicit w =>
      val addresses = getPublicKeys
      val pubkey = addresses.head.pubkey
      addresses.length should be > 0
      val genesisBlock = makeGenesisBlock(pubkey, randomNewAsset)
      val genesisTx = genesisBlock.transactions.head
      applyBlock(genesisBlock) shouldBe 'success //scan by wallet happens during apply
      waitForScanning(genesisBlock)
      val snap = getConfirmedBalances

      // prepare a lot of inputs
      val inputsToCreate = 50
      val sumToSpend = (snap.balance - MinBoxValue) / (inputsToCreate + 1)
      val req = (0 until inputsToCreate).map(_ => PaymentRequest(addresses.head, sumToSpend, Seq.empty, Map.empty))
      log.info(s"Confirmed balance $snap")
      log.info(s"Payment request $req")
      val tx = await(wallet.generateTransaction(req)).get
      log.info(s"Generated transaction $tx")
      val context = new ErgoStateContext(Seq(genesisBlock.header), Some(genesisBlock.extension), startDigest, parameters, validationSettingsNoIl, VotingData.empty)
      val boxesToSpend = tx.inputs.map(i => genesisTx.outputs.find(o => java.util.Arrays.equals(o.id, i.boxId)).get)
      tx.statefulValidity(boxesToSpend, emptyDataBoxes, context) shouldBe 'success
      val block = makeNextBlock(getUtxoState, Seq(tx))
      applyBlock(block) shouldBe 'success //scan by wallet happens during apply
      waitForScanning(block)

      // generate transaction spending part of inputs
      val newSumToSpend = tx.outputs.head.value
      val req2 = Seq(PaymentRequest(addresses.head, newSumToSpend, Seq.empty, Map.empty))
      log.info(s"Payment requests 2 $req2")
      val tx2 = await(wallet.generateTransaction(req2)).get
      log.info(s"Generated transaction $tx2")
      wallet.scanOffchain(tx2)
      blocking(Thread.sleep(1000))
      tx2.inputs.size should be < tx.outputs.size

      // trying to create a new transaction
      val tx3 = await(wallet.generateTransaction(req2)).get
      // check that tx3 has inputs different from tx2
      tx3.inputs.foreach { in =>
        tx2.inputs.exists(tx2In => tx2In.boxId sameElements in.boxId) shouldBe false
      }
    }
  }

  property("Generate asset issuing transaction") {
    withFixture { implicit w =>
      val address = getPublicKeys.head
      val genesisBlock = makeGenesisBlock(address.pubkey)
      val genesisTx = genesisBlock.transactions.head
      applyBlock(genesisBlock) shouldBe 'success //scan by wallet happens during apply
      waitForScanning(genesisBlock)
      val availableAmount = getConfirmedBalances.balance
      val emissionAmount: Int = 100000000
      val tokenName: String = "ERG"
      val tokenDescription: String = s"ERG description"
      val tokenDecimals: Int = 9
      val feeAmount = availableAmount / 4
      val feeReq = PaymentRequest(Pay2SAddress(Constants.TrueLeaf), feeAmount, Seq.empty, Map.empty)
      val req = AssetIssueRequest(address, emissionAmount, tokenName, tokenDescription, tokenDecimals)
      val tx = await(wallet.generateTransaction(Seq(feeReq, req))).get
      log.info(s"Generated transaction $tx")
      val context = new ErgoStateContext(
        Seq(genesisBlock.header),
        Some(genesisBlock.extension),
        startDigest,
        parameters,
        validationSettingsNoIl,
        VotingData.empty)
      val boxesToSpend = tx.inputs.map(i => genesisTx.outputs.find(o => java.util.Arrays.equals(o.id, i.boxId)).get)
      tx.statefulValidity(boxesToSpend, emptyDataBoxes, context) shouldBe 'success
    }
  }

  property("Generate transaction with user-defined input") {
    withFixture { implicit w =>
      val pubKey = getPublicKeys.head.pubkey
      val genesisBlock = makeGenesisBlock(pubKey, randomNewAsset)
      val initialBoxes = boxesAvailable(genesisBlock, pubKey)

      val boxesToUseEncoded = initialBoxes.map { box =>
        Base16.encode(ErgoBoxSerializer.toBytes(box))
      }

      applyBlock(genesisBlock) shouldBe 'success
      waitForScanning(genesisBlock)

      val confirmedBalance = getConfirmedBalances.balance

      //pay out all the wallet balance:
      val assetToSpend = assetsByTokenId(boxesAvailable(genesisBlock, pubKey)).toSeq
      assetToSpend should not be empty
      val req1 = PaymentRequest(Pay2SAddress(Constants.TrueLeaf), confirmedBalance, assetToSpend, Map.empty)

      val tx1 = await(wallet.generateTransaction(Seq(req1), boxesToUseEncoded)).get
      tx1.outputs.size shouldBe 1
      tx1.outputs.head.value shouldBe confirmedBalance
      toAssetMap(tx1.outputs.head.additionalTokens.toArray) shouldBe toAssetMap(assetToSpend)

      //change == 1:
      val assetToSpend2 = assetToSpend.map { case (tokenId, tokenValue) => (tokenId, tokenValue - 1) }
      val assetToReturn = assetToSpend.map { case (tokenId, _) => (tokenId, 1L) }
      val req2 = PaymentRequest(Pay2SAddress(Constants.TrueLeaf), confirmedBalance - MinBoxValue, assetToSpend2, Map.empty)

      val tx2 = await(wallet.generateTransaction(Seq(req2))).get
      tx2.outputs.size shouldBe 2
      tx2.outputs.head.value shouldBe confirmedBalance - MinBoxValue
      toAssetMap(tx2.outputs.head.additionalTokens.toArray) shouldBe toAssetMap(assetToSpend2)
      tx2.outputs(1).value shouldBe MinBoxValue
      toAssetMap(tx2.outputs(1).additionalTokens.toArray) shouldBe toAssetMap(assetToReturn)
    }
  }

  property("Generate transaction with multiple inputs") {
    withFixture { implicit w =>
      val addresses = getPublicKeys
      val pubkey = addresses.head.pubkey
      addresses.length should be > 0
      val genesisBlock = makeGenesisBlock(pubkey, randomNewAsset)
      val genesisTx = genesisBlock.transactions.head
      val initialBoxes = boxesAvailable(genesisTx, pubkey)
      applyBlock(genesisBlock) shouldBe 'success //scan by wallet happens during apply
      waitForScanning(genesisBlock)
      val snap = getConfirmedBalances
      val assetsToSpend = assetsByTokenId(initialBoxes).toSeq
      assetsToSpend should not be empty

      val sumToSpend = snap.balance / (addresses.length + 1)
      val req =
        PaymentRequest(addresses.head, sumToSpend, assetsToSpend, Map.empty) +:
          addresses.tail.map(a => PaymentRequest(a, sumToSpend, Seq.empty, Map.empty))
      log.info(s"Confirmed balance $snap")
      log.info(s"Payment request $req")
      val tx = await(wallet.generateTransaction(req)).get
      log.info(s"Generated transaction $tx")
      val context = new ErgoStateContext(
        Seq(genesisBlock.header),
        Some(genesisBlock.extension),
        startDigest,
        parameters,
        validationSettingsNoIl,
        VotingData.empty
      )
      val boxesToSpend = tx.inputs.map(i => genesisTx.outputs.find(o => java.util.Arrays.equals(o.id, i.boxId)).get)
      tx.statefulValidity(boxesToSpend, emptyDataBoxes, context) shouldBe 'success

      val block = makeNextBlock(getUtxoState, Seq(tx))
      applyBlock(block) shouldBe 'success //scan by wallet happens during apply
      waitForScanning(block)
      val newSnap = getConfirmedBalances
      val newSumToSpend = newSnap.balance / addresses.length
      val req2 = PaymentRequest(addresses.head, newSumToSpend, assetsToSpend, Map.empty) +:
        addresses.tail.map(a => PaymentRequest(a, newSumToSpend, Seq.empty, Map.empty))
      log.info(s"New balance $newSnap")
      log.info(s"Payment requests 2 $req2")
      val tx2 = await(wallet.generateTransaction(req2)).get
      log.info(s"Generated transaction $tx2")
      val context2 = new ErgoStateContext(Seq(block.header), Some(block.extension), startDigest, parameters, validationSettingsNoIl, VotingData.empty)
      val knownBoxes = tx.outputs ++ genesisTx.outputs
      val boxesToSpend2 = tx2.inputs.map(i => knownBoxes.find(o => java.util.Arrays.equals(o.id, i.boxId)).get)
      tx2.statefulValidity(boxesToSpend2, emptyDataBoxes, context2) shouldBe 'success
    }
  }

  property("off-chain scan") {
    withFixture { implicit w =>
      val pubKey = getPublicKeys.head.script
      val fakeInput = IndexedSeq(Input(ADKey @@ Array.fill(32)(0: Byte), emptyProverResult))

      val bs0 = getBalancesWithUnconfirmed
      bs0.balance shouldBe 0
      bs0.assetBalances shouldBe empty

      val balance1 = Random.nextInt(1000) + 1
      val box1 = IndexedSeq(new ErgoBoxCandidate(balance1, pubKey, startHeight, randomNewAsset.toColl))
      wallet.scanOffchain(ErgoTransaction(fakeInput, box1))

      blocking(Thread.sleep(1000))

      val bs1 = getBalancesWithUnconfirmed
      bs1.balance shouldBe balance1
      bs1.assetBalances shouldBe assetAmount(box1)

      val balance2 = Random.nextInt(1000) + 1
      val box2 = IndexedSeq(new ErgoBoxCandidate(balance2, pubKey, startHeight, randomNewAsset.toColl))
      wallet.scanOffchain(ErgoTransaction(fakeInput, IndexedSeq(), box2))

      blocking(Thread.sleep(1000))

      val bs2 = getBalancesWithUnconfirmed
      bs2.balance shouldBe (balance1 + balance2)
      bs2.assetBalances shouldBe assetAmount(box1 ++ box2)

      wallet.watchFor(Pay2SAddress(Constants.TrueLeaf))
      val balance3 = Random.nextInt(1000) + 1
      val box3 = IndexedSeq(new ErgoBoxCandidate(balance3, Constants.TrueLeaf, startHeight, randomNewAsset.toColl))
      wallet.scanOffchain(ErgoTransaction(fakeInput, IndexedSeq(), box3))

      blocking(Thread.sleep(1000))

      val bs3 = getBalancesWithUnconfirmed
      bs3.balance shouldBe (balance1 + balance2 + balance3)
      bs3.assetBalances shouldBe assetAmount(box1 ++ box2 ++ box3)
    }
  }

  property("off-chain box spending") {
    withFixture { implicit w =>
      val address = getPublicKeys.head
      val tx = makeGenesisTx(address.pubkey, randomNewAsset)
      wallet.scanOffchain(tx)
      val boxesToSpend = boxesAvailable(tx, address.pubkey)
      val balanceToSpend = balanceAmount(boxesToSpend)
      waitForOffchainScanning(tx)
      val totalBalance = getBalancesWithUnconfirmed.balance
      totalBalance shouldEqual balanceToSpend

      val balanceToReturn = randomLong(balanceToSpend)
      val spendingTx = makeSpendingTx(boxesToSpend, address, balanceToReturn, assetsWithRandom(boxesToSpend))
      val assetsAfterSpending = assetAmount(boxesAvailable(spendingTx, address.pubkey))
      assetsAfterSpending should not be empty

      wallet.scanOffchain(spendingTx)
      waitForOffchainScanning(tx)
      val totalAfterSpending = getBalancesWithUnconfirmed

      log.info(s"Total balance with unconfirmed: $totalBalance")
      log.info(s"Balance to spent: $balanceToSpend")
      log.info(s"Balance to return back: $balanceToReturn")
      totalAfterSpending.balance shouldEqual balanceToReturn
      totalAfterSpending.assetBalances shouldEqual assetsAfterSpending
    }
  }

  property("off-chain double registration") {
    withFixture { implicit w =>
      val address = getPublicKeys.head
      val tx = makeGenesisTx(address.pubkey, randomNewAsset)
      wallet.scanOffchain(tx)
      val boxesToSpend = boxesAvailable(tx, address.pubkey)
      val balanceToSpend = balanceAmount(boxesToSpend)
      waitForOffchainScanning(tx)
      val totalBalance = getBalancesWithUnconfirmed.balance

      val balanceToReturn = randomLong(balanceToSpend)
      val spendingTx = makeSpendingTx(boxesToSpend, address, balanceToReturn, assetsWithRandom(boxesToSpend))
      //      val doubleSpendingTx = makeSpendingTx(boxesToSpend, address, randomLong(balanceToSpend))
      val assets = assetAmount(boxesAvailable(spendingTx, address.pubkey))
      assets should not be empty

      wallet.scanOffchain(Seq(spendingTx, spendingTx))
      wallet.scanOffchain(spendingTx)
      blocking(Thread.sleep(offchainScanTime(tx) * 3))
      val totalAfterSpending = getBalancesWithUnconfirmed

      log.info(s"Total with unconfirmed balance: $totalBalance")
      log.info(s"Balance to spent: $balanceToSpend")
      log.info(s"Balance to return back: $balanceToReturn")
      totalBalance shouldEqual balanceToSpend
      totalAfterSpending.balance shouldEqual balanceToReturn
      totalAfterSpending.assetBalances shouldEqual assets
    }
  }

  property("off-chain spending of the on-chain box") {
    withFixture { implicit w =>
      val address = getPublicKeys.head
      val genesisBlock = makeGenesisBlock(address.pubkey, randomNewAsset)
      val boxesToSpend = boxesAvailable(genesisBlock, address.pubkey)
      val sumBalance = balanceAmount(boxesToSpend)
      applyBlock(genesisBlock) shouldBe 'success
      waitForScanning(genesisBlock)
      val totalBalance = getBalancesWithUnconfirmed.balance
      val confirmedBalance = getConfirmedBalances.balance

      val balanceToReturn = randomLong(sumBalance)
      val spendingTx = makeSpendingTx(boxesToSpend, address, balanceToReturn, assetsWithRandom(boxesToSpend))
      val assets = assetAmount(boxesAvailable(spendingTx, address.pubkey))
      assets should not be empty

      wallet.scanOffchain(spendingTx)
      waitForOffchainScanning(spendingTx)
      val confirmedAfterSpending = getConfirmedBalances.balance
      val totalAfterSpending = getBalancesWithUnconfirmed

      log.info(s"Sum balance: $sumBalance")
      log.info(s"Balance before spending: $confirmedBalance")
      log.info(s"Total with unconfirmed balance before spending: $totalBalance")
      log.info(s"Balance after spending: $confirmedAfterSpending")
      log.info(s"Total with unconfirmed after spending: $totalAfterSpending")

      confirmedBalance shouldBe sumBalance
      totalBalance shouldBe sumBalance
      confirmedAfterSpending shouldBe sumBalance
      totalAfterSpending.balance shouldBe balanceToReturn
      totalAfterSpending.assetBalances shouldBe assets
    }
  }

  property("assets application") {
    withFixture { implicit w =>
      val address = getPublicKeys.head
      val asset1Sum = randomLong()
      val genesisBlock = makeGenesisBlock(address.pubkey, Seq(newAssetIdStub -> asset1Sum))
      val boxesToSpend = boxesAvailable(genesisBlock, address.pubkey)
      applyBlock(genesisBlock) shouldBe 'success
      waitForScanning(genesisBlock)
      val initialBalance = getConfirmedBalances
      val initialTotal = getBalancesWithUnconfirmed
      val initialAssets = initialBalance.assetBalances
      log.info(s"Initial assets: ${boxesToSpend.flatMap(_.additionalTokens.toArray)}")
      log.info(s"Confirmed: $initialBalance")
      log.info(s"With unconfirmed: $initialTotal")
      initialAssets should not be empty
      val (asset1Token, asset1InitialValue) = initialAssets.head
      asset1InitialValue shouldBe asset1Sum
      initialTotal.assetBalances shouldBe initialAssets

      val asset2Sum = randomLong()
      val asset1ToReturn = randomLong(asset1Sum)
      val assets2Seq = Seq(decodedTokenId(asset1Token) -> asset1ToReturn, newAssetIdStub -> asset2Sum)
      val balanceToReturn = 1000 * parameters.minValuePerByte
      val spendingTx = makeSpendingTx(boxesToSpend, address, balanceToReturn, assets2Seq)
      val spendingBlock = makeNextBlock(getUtxoState, Seq(spendingTx))
      applyBlock(spendingBlock) shouldBe 'success
      wallet.scanPersistent(spendingBlock)
      waitForScanning(spendingBlock)
      val balanceAfterSpending = getConfirmedBalances
      val totalAfterSpending = getBalancesWithUnconfirmed
      log.info(s"After spending: $balanceAfterSpending")
      log.info(s"With unconfirmed after spending: $balanceAfterSpending")
      val assets = balanceAfterSpending.assetBalances
      totalAfterSpending.assetBalances shouldBe assets
      assets(asset1Token) shouldBe asset1ToReturn
      val asset2 = assets.filter(_._1 != asset1Token)
      asset2 should not be empty
      asset2.head._2 shouldBe asset2Sum
    }
  }

  property("on-chain box spending (without return)") {
    withFixture { implicit w =>
      val address = getPublicKeys.head
      val genesisBlock = makeGenesisBlock(address.pubkey, randomNewAsset)
      applyBlock(genesisBlock) shouldBe 'success
      waitForScanning(genesisBlock)

      val confirmedBalance = getConfirmedBalances.balance
      val boxesToSpend = boxesAvailable(genesisBlock, address.pubkey)
      val balanceToSpend = balanceAmount(boxesToSpend)
      log.info(s"Confirmed balance $confirmedBalance")
      log.info(s"Sum balance: $balanceToSpend")
      confirmedBalance should be > 0L
      confirmedBalance shouldBe balanceToSpend

      val spendingTx = makeSpendingTx(boxesToSpend, address, 0, assetsWithRandom(boxesToSpend))

      val spendingBlock = makeNextBlock(getUtxoState, Seq(spendingTx))
      applyBlock(spendingBlock) shouldBe 'success
      wallet.scanPersistent(spendingBlock)
      waitForScanning(spendingBlock)

      val balanceAfterSpending = getConfirmedBalances
      log.info(s"Boxes to spend: $boxesToSpend")
      log.info(s"Total with unconfirmed balance: $confirmedBalance")
      log.info(s"Balance to spent: $balanceToSpend")
      log.info(s"Balance after spend: ${balanceAfterSpending.balance}")
      balanceAfterSpending.balance shouldEqual 0
      getBalancesWithUnconfirmed shouldEqual balanceAfterSpending
    }
  }

  property("on-chain box spending (with return)") {
    withFixture { implicit w =>
      val address = getPublicKeys.head
      val genesisBlock = makeGenesisBlock(address.pubkey, randomNewAsset)
      applyBlock(genesisBlock) shouldBe 'success
      waitForScanning(genesisBlock)

      val confirmedBalance = getConfirmedBalances.balance
      val boxesToSpend = boxesAvailable(genesisBlock, address.pubkey)
      val balanceToSpend = balanceAmount(boxesToSpend)
      log.info(s"Confirmed balance $confirmedBalance")
      log.info(s"Sum balance: $balanceToSpend")
      confirmedBalance should be > 0L
      confirmedBalance shouldBe balanceToSpend

      val balanceToReturn = randomLong(balanceToSpend)
      val spendingTx = makeSpendingTx(boxesToSpend, address, balanceToReturn, assetsWithRandom(boxesToSpend))
      val assets = assetAmount(boxesAvailable(spendingTx, address.pubkey))
      assets should not be empty

      val spendingBlock = makeNextBlock(getUtxoState, Seq(spendingTx))
      applyBlock(spendingBlock) shouldBe 'success
      wallet.scanPersistent(spendingBlock)
      waitForScanning(spendingBlock)

      val balanceAfterSpending = getConfirmedBalances
      log.info(s"Boxes to spend: $boxesToSpend")
      log.info(s"Total with unconfirmed balance: $confirmedBalance")
      log.info(s"Balance to spent: $balanceToSpend")
      log.info(s"Balance to return back: $balanceToReturn")
      balanceAfterSpending.balance shouldEqual (confirmedBalance - balanceToSpend + balanceToReturn)
      balanceAfterSpending.assetBalances shouldBe assets
      getBalancesWithUnconfirmed shouldEqual balanceAfterSpending
    }
  }

  property("off-chain transaction becomes on-chain") {
    withFixture { implicit w =>
      val pubKey = getPublicKeys.head.pubkey
      val tx = makeGenesisTx(pubKey, randomNewAsset)
      wallet.scanOffchain(tx)
      waitForOffchainScanning(tx)
      val boxesToSpend = boxesAvailable(tx, pubKey)
      val sumBalance = balanceAmount(boxesToSpend)
      val sumAssets = assetAmount(boxesToSpend)
      sumAssets should not be empty

      val initialBalance = getBalancesWithUnconfirmed.balance
      initialBalance shouldBe sumBalance

      val block = makeNextBlock(getUtxoState, Seq(tx))
      applyBlock(block) shouldBe 'success
      waitForScanning(block)

      val confirmedBalance = getConfirmedBalances
      log.info(s"Confirmed balance $confirmedBalance")
      log.info(s"Sum balance: $sumBalance")
      initialBalance shouldBe sumBalance
      confirmedBalance.balance should be > 0L
      confirmedBalance.balance shouldBe initialBalance
      confirmedBalance.assetBalances shouldBe sumAssets
      getBalancesWithUnconfirmed shouldBe confirmedBalance
    }
  }

  property("off-chain spending rollback") {
    withFixture { implicit w =>
      val address = getPublicKeys.head
      val genesisBlock = makeGenesisBlock(address.pubkey)
      val initialBoxes = boxesAvailable(genesisBlock, address.pubkey)
      val initialBalance = balanceAmount(initialBoxes)
      applyBlock(genesisBlock) shouldBe 'success
      val initialState = getCurrentState

      // We need this second block to have something to rollback. Just spent some balance to anyone
      val balanceToSpend = randomLong(initialBalance)
      val onchainSpendingTx = makeTx(initialBoxes, emptyProverResult, balanceToSpend, address.pubkey)
      val boxesToSpend = boxesAvailable(onchainSpendingTx, address.pubkey)
      val block = makeNextBlock(getUtxoState, Seq(onchainSpendingTx))
      applyBlock(block) shouldBe 'success
      wallet.scanPersistent(block)
      waitForScanning(block)
      val totalBalance = getBalancesWithUnconfirmed.balance
      val confirmedBalance = getConfirmedBalances.balance

      val balanceToReturn = randomLong(balanceAmount(boxesToSpend))
      val spendingTx = makeSpendingTx(boxesToSpend, address, balanceToReturn)
      wallet.scanOffchain(spendingTx)
      waitForOffchainScanning(spendingTx)
      val confirmedAfterSpending = getConfirmedBalances.balance
      val totalAfterSpending = getBalancesWithUnconfirmed.balance

      wallet.rollback(initialState.version)
      val balanceAfterRollback = getConfirmedBalances.balance
      val totalAfterRollback = getBalancesWithUnconfirmed.balance

      log.info(s"Initial balance: $initialBalance")
      log.info(s"Balance before off-chain spending: $confirmedBalance")
      log.info(s"Total with unconfirmed balance before spending: $totalBalance")
      log.info(s"After spending before rollback: $confirmedAfterSpending")
      log.info(s"Total with unconfirmed balance after spending before rollback: $totalAfterSpending")
      log.info(s"Balance after rollback: $balanceAfterRollback")
      log.info(s"Total with unconfirmed balance after rollback: $totalAfterRollback")

      confirmedBalance shouldBe balanceToSpend
      totalBalance shouldBe confirmedBalance
      confirmedAfterSpending shouldBe confirmedBalance
      totalAfterSpending shouldBe balanceToReturn
      balanceAfterRollback shouldBe initialBalance
      totalAfterRollback shouldBe balanceToReturn
    }
  }

  property("on-chain rollback") {
    withFixture { implicit w =>
      val pubKey = getPublicKeys.head.pubkey
      val genesisBlock = makeGenesisBlock(pubKey)
      val boxesToSpend = boxesAvailable(genesisBlock, pubKey)
      applyBlock(genesisBlock) shouldBe 'success
      val initialState = getCurrentState
      waitForScanning(genesisBlock)
      val initialBalance = getConfirmedBalances.balance

      val balanceToSpend = randomLong(balanceAmount(boxesToSpend))
      val creationTx = makeTx(boxesToSpend, emptyProverResult, balanceToSpend, pubKey, randomNewAsset)
      val initialAssets = assetAmount(boxesAvailable(creationTx, pubKey))
      initialAssets should not be empty

      val block = makeNextBlock(getUtxoState, Seq(creationTx))
      wallet.scanPersistent(block)
      waitForScanning(block)
      val historyHeight = getHistory.headersHeight

      val confirmedBeforeRollback: RegistryIndex = getConfirmedBalances
      val totalBeforeRollback = getBalancesWithUnconfirmed
      wallet.rollback(initialState.version)
      blocking(Thread.sleep(100))
      val confirmedAfterRollback = getConfirmedBalances
      val totalAfterRollback = getBalancesWithUnconfirmed

      log.info(s"Initial balance: $initialBalance")
      log.info(s"Initial assets: $initialAssets")
      log.info(s"History height: $historyHeight")
      log.info(s"Confirmed balance: $confirmedBeforeRollback")
      log.info(s"Total with unconfirmed balance: $totalBeforeRollback")
      log.info(s"Balance after rollback: $confirmedAfterRollback")
      log.info(s"Total with unconfirmed balance after rollback: $totalAfterRollback")

      confirmedBeforeRollback.balance shouldBe balanceToSpend
      confirmedBeforeRollback.assetBalances shouldBe initialAssets
      totalBeforeRollback shouldBe confirmedBeforeRollback

      confirmedAfterRollback.balance shouldBe initialBalance
      confirmedAfterRollback.assetBalances shouldBe empty
      totalAfterRollback.balance shouldBe balanceToSpend
      totalAfterRollback.assetBalances shouldBe initialAssets
    }
  }

  property("on-chain spending rollback") {
    withFixture { implicit w =>
      val address = getPublicKeys.head
      val genesisBlock = makeGenesisBlock(address.pubkey, randomNewAsset)
      val boxesToSpend = boxesAvailable(genesisBlock, address.pubkey)
      val sumBalance = balanceAmount(boxesToSpend)
      val sumAssets = assetAmount(boxesToSpend)
      sumAssets should not be empty

      applyBlock(genesisBlock) shouldBe 'success
      val initialState = getCurrentState
      waitForScanning(genesisBlock)
      val initialSnapshot = getConfirmedBalances

      val spendingTx = makeSpendingTx(boxesToSpend, address)
      val block = makeNextBlock(getUtxoState, Seq(spendingTx))
      wallet.scanPersistent(block)
      waitForScanning(block)
      val historyHeight = getHistory.headersHeight

      val confirmedBeforeRollback = getConfirmedBalances
      val totalBeforeRollback = getBalancesWithUnconfirmed

      wallet.rollback(initialState.version)
      blocking(Thread.sleep(1000))
      val confirmedAfterRollback = getConfirmedBalances
      val totalAfterRollback = getBalancesWithUnconfirmed

      log.info(s"Initial balance: $initialSnapshot")
      log.info(s"Balance to spend: $sumBalance")
      log.info(s"History height: $historyHeight")
      log.info(s"Confirmed balance: $confirmedBeforeRollback")
      log.info(s"Total with unconfirmed balance: $totalBeforeRollback")
      log.info(s"Balance after rollback: $confirmedAfterRollback")
      log.info(s"Total with unconfirmed balance after rollback: $totalAfterRollback")

      initialSnapshot.balance shouldBe sumBalance
      initialSnapshot.assetBalances shouldBe sumAssets

      confirmedBeforeRollback.balance shouldBe 0L
      confirmedBeforeRollback.assetBalances shouldBe empty
      totalBeforeRollback shouldBe confirmedBeforeRollback

      confirmedAfterRollback shouldBe initialSnapshot
      totalAfterRollback.balance shouldBe confirmedBeforeRollback.balance
      totalAfterRollback.assetBalances shouldBe confirmedBeforeRollback.assetBalances
    }
  }

  property("on-chain spending with return rollback") {
    withFixture { implicit w =>
      val address = getPublicKeys.head
      val genesisBlock = makeGenesisBlock(address.pubkey, randomNewAsset)
      val boxesToSpend = boxesAvailable(genesisBlock, address.pubkey)
      val sumBalance = balanceAmount(boxesToSpend)

      applyBlock(genesisBlock) shouldBe 'success
      val initialState = getCurrentState
      waitForScanning(genesisBlock)
      val initialSnapshot = getConfirmedBalances

      val balanceToReturn = randomLong(sumBalance)
      val sumAsset1 = assetsByTokenId(boxesToSpend).toSeq
      sumAsset1 should not be empty

      val asset1Map = toAssetMap(sumAsset1)
      val assetToReturn = sumAsset1.map { case (tokenId, tokenValue) => (tokenId, randomLong(tokenValue)) }
      val assetsForSpending = randomNewAsset ++ assetToReturn
      val spendingTx = makeSpendingTx(boxesToSpend, address, balanceToReturn, assetsForSpending)
      val block = makeNextBlock(getUtxoState, Seq(spendingTx))
      wallet.scanPersistent(block)
      waitForScanning(block)
      val historyHeight = getHistory.headersHeight

      val confirmedBeforeRollback = getConfirmedBalances
      val totalBeforeRollback = getBalancesWithUnconfirmed

      wallet.rollback(initialState.version)
      blocking(Thread.sleep(100))
      val confirmedAfterRollback = getConfirmedBalances
      val totalAfterRollback = getBalancesWithUnconfirmed

      log.info(s"Initial balance: $initialSnapshot")
      log.info(s"Balance to spend: $sumBalance")
      log.info(s"Balance to return $balanceToReturn")
      log.info(s"History height: $historyHeight")
      log.info(s"Confirmed balance: $confirmedBeforeRollback")
      log.info(s"Total with unconfirmed balance: $totalBeforeRollback")
      log.info(s"Balance after rollback: $confirmedAfterRollback")
      log.info(s"Total with unconfirmed balance after rollback: $totalAfterRollback")

      initialSnapshot.balance shouldBe sumBalance
      initialSnapshot.assetBalances shouldBe asset1Map
      confirmedBeforeRollback.balance should be > 0L
      confirmedBeforeRollback.balance shouldBe balanceToReturn
      confirmedBeforeRollback.assetBalances should have size 2
      totalBeforeRollback.balance shouldBe balanceToReturn
      totalBeforeRollback.assetBalances shouldBe confirmedBeforeRollback.assetBalances

      confirmedAfterRollback shouldBe initialSnapshot
      confirmedAfterRollback.assetBalances shouldBe asset1Map
      totalAfterRollback.balance shouldBe balanceToReturn
      totalAfterRollback.assetBalances shouldBe totalBeforeRollback.assetBalances
    }
  }

  property("on-chain spent box to off-chain box rollback") {
    withFixture { implicit w =>
      val address = getPublicKeys.head
      val genesisBlock = makeGenesisBlock(address.pubkey)
      val initialBoxes = boxesAvailable(genesisBlock, address.pubkey)
      applyBlock(genesisBlock) shouldBe 'success
      val initialState = getCurrentState
      val initialBalance = balanceAmount(initialBoxes)

      val balancePicked = randomLong(initialBalance)
      val creationTx = makeTx(initialBoxes, emptyProverResult, balancePicked, address.pubkey, randomNewAsset)
      val boxesToSpend = boxesAvailable(creationTx, address.pubkey)
      val balanceToSpend = balanceAmount(boxesToSpend)

      val balanceToReturn = randomLong(balanceToSpend)
      val sumAsset1 = assetsByTokenId(boxesToSpend).toSeq
      sumAsset1 should not be empty

      val assetToReturn = sumAsset1.map { case (tokenId, tokenValue) => (tokenId, randomLong(tokenValue)) }
      val assetsForSpending = randomNewAsset ++ assetToReturn
      val spendingTx = makeSpendingTx(boxesToSpend, address, balanceToReturn, assetsForSpending)
      val block = makeNextBlock(getUtxoState, Seq(creationTx, spendingTx))
      wallet.scanPersistent(block)
      waitForScanning(block)
      val historyHeight = getHistory.headersHeight

      val confirmedBeforeRollback = getConfirmedBalances
      val totalBeforeRollback = getBalancesWithUnconfirmed

      wallet.rollback(initialState.version)
      blocking(Thread.sleep(100))
      val confirmedAfterRollback = getConfirmedBalances
      val totalAfterRollback = getBalancesWithUnconfirmed

      log.info(s"Balance to spend: $balanceToSpend")
      log.info(s"History height: $historyHeight")
      log.info(s"Confirmed balance: $confirmedBeforeRollback")
      log.info(s"Total with unconfirmed balance: $totalBeforeRollback")
      log.info(s"Balance after rollback: $confirmedAfterRollback")
      log.info(s"Total with unconfirmed balance after rollback: $totalAfterRollback")

      balanceToSpend shouldBe balancePicked
      confirmedBeforeRollback.balance shouldBe balanceToReturn
      confirmedBeforeRollback.assetBalances should have size 2
      totalBeforeRollback shouldBe confirmedBeforeRollback

      confirmedAfterRollback.balance shouldBe initialBalance
      totalAfterRollback.balance shouldBe balanceToReturn
      totalAfterRollback.assetBalances shouldBe totalBeforeRollback.assetBalances
    }
  }

  property("single-input transaction generation") {
    withFixture { implicit w =>
      val pubKey = getPublicKeys.head.pubkey
      val genesisBlock = makeGenesisBlock(pubKey, randomNewAsset)
      applyBlock(genesisBlock) shouldBe 'success
      waitForScanning(genesisBlock)
      val confirmedBalance = getConfirmedBalances.balance

      //pay out all the wallet balance:
      val assetToSpend = assetsByTokenId(boxesAvailable(genesisBlock, pubKey)).toSeq
      assetToSpend should not be empty
      val req1 = PaymentRequest(Pay2SAddress(Constants.TrueLeaf), confirmedBalance, assetToSpend, Map.empty)

      val tx1 = await(wallet.generateTransaction(Seq(req1))).get
      tx1.outputs.size shouldBe 1
      tx1.outputs.head.value shouldBe confirmedBalance
      toAssetMap(tx1.outputs.head.additionalTokens.toArray) shouldBe toAssetMap(assetToSpend)

      //change == 1:
      val assetToSpend2 = assetToSpend.map { case (tokenId, tokenValue) => (tokenId, tokenValue - 1) }
      val assetToReturn = assetToSpend.map { case (tokenId, _) => (tokenId, 1L) }
      val req2 = PaymentRequest(Pay2SAddress(Constants.TrueLeaf), confirmedBalance - MinBoxValue, assetToSpend2, Map.empty)

      val tx2 = await(wallet.generateTransaction(Seq(req2))).get
      tx2.outputs.size shouldBe 2
      tx2.outputs.head.value shouldBe confirmedBalance - MinBoxValue
      toAssetMap(tx2.outputs.head.additionalTokens.toArray) shouldBe toAssetMap(assetToSpend2)
      tx2.outputs(1).value shouldBe MinBoxValue
      toAssetMap(tx2.outputs(1).additionalTokens.toArray) shouldBe toAssetMap(assetToReturn)
    }
  }

  property("only unspent certain boxes is used for transaction generation") {
    withFixture { implicit w =>
      val pubKey = getPublicKeys.head.pubkey
      val genesisBlock = makeGenesisBlock(pubKey)
      val uncertainAmount = 2000000
      val modifiedBlock = {
        val prop = ErgoScriptPredef.rewardOutputScript(100, pubKey)
        val txToModify = genesisBlock.blockTransactions.txs.last
        val txWithUncertainOutput = txToModify
          .copy(outputCandidates = IndexedSeq(new ErgoBoxCandidate(uncertainAmount, prop, 1)))
        genesisBlock.copy(
          blockTransactions = genesisBlock.blockTransactions.copy(
            txs = genesisBlock.blockTransactions.txs :+ txWithUncertainOutput
          )
        )
      }
      val initialBoxes = boxesAvailable(modifiedBlock, pubKey)
      val totalAvailableAmount = initialBoxes.map(_.value).sum
      val certainAmount = totalAvailableAmount - uncertainAmount

      wallet.scanPersistent(modifiedBlock)

      blocking(Thread.sleep(100))

      val requestWithTotalAmount = PaymentRequest(
        ErgoAddressEncoder(0: Byte).fromProposition(pubKey).get, totalAvailableAmount, Seq.empty, Map.empty)
      val requestWithCertainAmount = requestWithTotalAmount.copy(value = certainAmount)

      val uncertainTxTry = await(wallet.generateTransaction(Seq(requestWithTotalAmount)))
      uncertainTxTry shouldBe 'failure
      uncertainTxTry.failed.get.getMessage.startsWith("Failed to find boxes to assemble a transaction") shouldBe true
      await(wallet.generateTransaction(Seq(requestWithCertainAmount))) shouldBe 'success
    }
  }

  property("watchFor") {
    withFixture { implicit w =>
      val pubKey = getPublicKeys.head.pubkey
      val genesisBlock = makeGenesisBlock(pubKey)
      val initialBoxes = boxesAvailable(genesisBlock, pubKey)
      val initialBalance = balanceAmount(initialBoxes)
      applyBlock(genesisBlock) shouldBe 'success
      waitForScanning(genesisBlock)
      val initialState = getCurrentState

      val preimage = ByteArrayConstant("hello world".getBytes("UTF-8"))
      val hash = Blake2b256(preimage.value.toArray)
      val p2s = Pay2SAddress(EQ(CalcBlake2b256(preimage), hash).toSigmaProp)
      val balanceToSpend = randomLong(initialBalance)
      val tx = makeTx(initialBoxes, emptyProverResult, balanceToSpend, p2s.script, randomNewAsset)
      val assets = assetAmount(boxesAvailable(tx, p2s.script.bytes))
      val block = makeNextBlock(getUtxoState, Seq(tx))

      wallet.scanPersistent(block)
      waitForScanning(block)
      val confirmedBalance = getConfirmedBalances
      val sumOutputs = balanceAmount(boxesAvailable(block, p2s.script.bytes))
      confirmedBalance.balance shouldBe 0
      confirmedBalance.assetBalances shouldBe empty

      wallet.rollback(initialState.version)
      blocking(Thread.sleep(100))

      wallet.watchFor(p2s)
      wallet.scanPersistent(block)
      waitForScanning(block)
      val confirmedBalance2 = getConfirmedBalances
      confirmedBalance2.balance shouldBe sumOutputs
      confirmedBalance2.assetBalances shouldBe assets
    }
  }

}
