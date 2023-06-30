package org.ergoplatform.nodeView.wallet

import org.ergoplatform._
import org.ergoplatform.modifiers.mempool.{ErgoTransaction, UnsignedErgoTransaction}
import org.ergoplatform.nodeView.state.{ErgoStateContext, VotingData}
import org.ergoplatform.nodeView.wallet.IdUtils._
import org.ergoplatform.nodeView.wallet.persistence.{WalletDigest, WalletDigestSerializer}
import org.ergoplatform.nodeView.wallet.requests.{AssetIssueRequest, BurnTokensRequest, ExternalSecret, PaymentRequest}
import org.ergoplatform.sdk.wallet.secrets.PrimitiveSecretKey
import org.ergoplatform.settings.{Algos, Constants}
import org.ergoplatform.utils._
import org.ergoplatform.utils.fixtures.WalletFixture
import org.ergoplatform.wallet.boxes.BoxSelector.MinBoxValue
import org.ergoplatform.wallet.boxes.ErgoBoxSerializer
import org.ergoplatform.wallet.interpreter.{ErgoInterpreter, TransactionHintsBag}
import org.scalacheck.Gen
import org.scalatest.concurrent.Eventually
import scorex.util.ModifierId
import scorex.util.encode.Base16
import sigmastate.basics.DLogProtocol.DLogProverInput
import sigmastate.eval.Extensions._
import sigmastate.eval._
import sigmastate.{CAND, CTHRESHOLD}

import scala.concurrent.duration._

class ErgoWalletSpec extends ErgoPropertyTest with WalletTestOps with Eventually {

  private implicit val verifier: ErgoInterpreter = ErgoInterpreter(parameters)

  property("assets in WalletDigest are deterministic against serialization") {
    forAll(Gen.listOfN(5, assetGen)) { preAssets =>
      val assets = preAssets.map { case (id, amt) => ModifierId @@ Algos.encode(id) -> amt }
      val wd0 = WalletDigest(1, 0, assets)
      val bs = WalletDigestSerializer.toBytes(wd0)
      WalletDigestSerializer.parseBytes(bs).walletAssetBalances shouldBe wd0.walletAssetBalances
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
      implicit val patienceConfig: PatienceConfig = PatienceConfig(5.second, 300.millis)
      val tx =
        eventually {
          val snap = getConfirmedBalances
          // prepare a lot of inputs
          val inputsToCreate = 50
          val sumToSpend = (snap.walletBalance - MinBoxValue) / (inputsToCreate + 1)
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
          tx
        }
      val (req2, tx2) =
        eventually {
          // generate transaction spending part of inputs
          val newSumToSpend = tx.outputs.head.value
          val req2 = Seq(PaymentRequest(addresses.head, newSumToSpend, Seq.empty, Map.empty))
          log.info(s"Payment requests 2 $req2")
          val tx2 = await(wallet.generateTransaction(req2)).get
          (req2, tx2)
        }
      log.info(s"Generated transaction $tx2")
      wallet.scanOffchain(tx2)

      eventually {
        tx2.inputs.size should be < tx.outputs.size
        // trying to create a new transaction
        val tx3 = await(wallet.generateTransaction(req2)).get
        // check that tx3 has inputs different from tx2
        tx3.inputs.foreach { in =>
          tx2.inputs.exists(tx2In => tx2In.boxId sameElements in.boxId) shouldBe false
        }
      }
    }
  }

  property("Generate asset issuing transaction") {
    withFixture { implicit w =>
      val address = getPublicKeys.head
      val genesisBlock = makeGenesisBlock(address.pubkey)
      val genesisTx = genesisBlock.transactions.head
      applyBlock(genesisBlock) shouldBe 'success //scan by wallet happens during apply
      implicit val patienceConfig: PatienceConfig = PatienceConfig(5.second, 100.millis)
      eventually {
        val availableAmount = getConfirmedBalances.walletBalance
        val emissionAmount: Int = 100000000
        val tokenName: String = "ERG"
        val tokenDescription: String = s"ERG description"
        val tokenDecimals: Int = 9
        val feeAmount = availableAmount / 4
        val feeReq = PaymentRequest(Pay2SAddress(Constants.TrueLeaf), feeAmount, Seq.empty, Map.empty)
        val req = AssetIssueRequest(address, None, emissionAmount, tokenName, tokenDescription, tokenDecimals)
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
      implicit val patienceConfig: PatienceConfig = PatienceConfig(5.second, 300.millis)
      eventually {
        val confirmedBalance = getConfirmedBalances.walletBalance

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
  }

  property("Generate transaction with BurnTokensRequest") {
    withFixture { implicit w =>
      val pubKey = getPublicKeys.head.pubkey
      val genesisBlock = makeGenesisBlock(pubKey, randomNewAsset)
      val initialBoxes = boxesAvailable(genesisBlock, pubKey)

      val boxesToUseEncoded = initialBoxes.map { box =>
        Base16.encode(ErgoBoxSerializer.toBytes(box))
      }

      applyBlock(genesisBlock) shouldBe 'success
      implicit val patienceConfig: PatienceConfig = PatienceConfig(5.second, 300.millis)
      eventually {
        val confirmedBalance = getConfirmedBalances.walletBalance

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
        val req2 = Seq(BurnTokensRequest(assetToSpend2))

        val tx2 = await(wallet.generateTransaction(req2)).get
        tx2.outputs.size shouldBe 1
        tx2.outputs.head.value shouldBe confirmedBalance
        toAssetMap(tx2.outputs.head.additionalTokens.toArray) shouldBe toAssetMap(assetToReturn)
      }
    }
  }

  property("Generate transaction with PaymentRequest (no tokens) and BurnTokensRequest") {
    withFixture { implicit w =>
      val pubKey = getPublicKeys.head.pubkey
      val genesisBlock = makeGenesisBlock(pubKey, randomNewAsset)
      val initialBoxes = boxesAvailable(genesisBlock, pubKey)

      val boxesToUseEncoded = initialBoxes.map { box =>
        Base16.encode(ErgoBoxSerializer.toBytes(box))
      }

      applyBlock(genesisBlock) shouldBe 'success
      implicit val patienceConfig: PatienceConfig = PatienceConfig(5.second, 300.millis)
      eventually {
        val confirmedBalance = getConfirmedBalances.walletBalance
        log.error(s"Confirmed balance $confirmedBalance")
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
        val req2 = Seq(BurnTokensRequest(assetToSpend2), PaymentRequest(Pay2SAddress(Constants.TrueLeaf), confirmedBalance - MinBoxValue, Seq.empty, Map.empty))

        val tx2 = await(wallet.generateTransaction(req2)).get
        tx2.outputs.size shouldBe 2
        tx2.outputs.head.value shouldBe confirmedBalance - MinBoxValue
        toAssetMap(tx2.outputs.head.additionalTokens.toArray) shouldBe toAssetMap(Seq.empty)
        tx2.outputs(1).value shouldBe MinBoxValue
        toAssetMap(tx2.outputs(1).additionalTokens.toArray) shouldBe toAssetMap(assetToReturn)
      }
    }
  }

  property("whitelist set, preserve tokens from auto-burn") {
    val inputs = {
      val x = IndexedSeq(new Input(genesisEmissionBox.id, emptyProverResult))
      Seq(encodedTokenId(x.head.boxId.toTokenId))
    }

    implicit val ww: WalletFixture = new WalletFixture(settings
      .copy(walletSettings = settings
        .walletSettings.copy(tokensWhitelist = Some(inputs))), parameters, getCurrentView(_).vault)

    val pubKey = getPublicKeys.head.pubkey
    val genesisBlock = makeNextBlock(getUtxoState, Seq(makeGenesisTxWithAsset(pubKey, issueAsset = true)))
    val initialBoxes = boxesAvailable(genesisBlock, pubKey)
    val assetR = assetsByTokenId(initialBoxes).toSeq
    Some(assetR.map(x => encodedTokenId(x._1))) shouldBe ww.settings.walletSettings.tokensWhitelist

    val boxesToUseEncoded = initialBoxes.map { box =>
      Base16.encode(ErgoBoxSerializer.toBytes(box))
    }

    applyBlock(genesisBlock) shouldBe 'success
    implicit val patienceConfig: PatienceConfig = PatienceConfig(5.second, 300.millis)
    eventually {
      val confirmedBalance = getConfirmedBalances.walletBalance
      log.error(s"Confirmed balance $confirmedBalance")
      //pay out all the wallet balance:
      val assetToSpend = assetsByTokenId(boxesAvailable(genesisBlock, pubKey)).toSeq
      Some(assetToSpend.map(x => encodedTokenId(x._1))) shouldBe ww.settings.walletSettings.tokensWhitelist
      assetToSpend should not be empty

      val req1 = PaymentRequest(Pay2SAddress(Constants.TrueLeaf), confirmedBalance / 2, Seq.empty, Map.empty)

      val tx1 = await(wallet.generateTransaction(Seq(req1), boxesToUseEncoded)).get
      tx1.outputs.size shouldBe 2
      tx1.outputs.head.value shouldBe (confirmedBalance / 2)
      tx1.outputs.head.additionalTokens.toArray shouldBe Seq.empty
      toAssetMap(tx1.outputs(1).additionalTokens.toArray) shouldBe toAssetMap(assetToSpend)
    }
  }

  property("whitelist empty, auto-burn tokens on arbitrary tx") {
    implicit val ww: WalletFixture = new WalletFixture(settings
      .copy(walletSettings = settings
        .walletSettings.copy(tokensWhitelist = Some(Seq.empty))), parameters, getCurrentView(_).vault)

    val pubKey = getPublicKeys.head.pubkey
    val genesisBlock = makeNextBlock(getUtxoState, Seq(makeGenesisTxWithAsset(pubKey, issueAsset = true)))
    val initialBoxes = boxesAvailable(genesisBlock, pubKey)

    val boxesToUseEncoded = initialBoxes.map { box =>
      Base16.encode(ErgoBoxSerializer.toBytes(box))
    }

    applyBlock(genesisBlock) shouldBe 'success
    implicit val patienceConfig: PatienceConfig = PatienceConfig(5.second, 300.millis)
    eventually {
      val confirmedBalance = getConfirmedBalances.walletBalance
      log.error(s"Confirmed balance $confirmedBalance")
      //pay out all the wallet balance:
      val assetToSpend = assetsByTokenId(boxesAvailable(genesisBlock, pubKey)).toSeq
      assetToSpend should not be empty

      val req1 = PaymentRequest(Pay2SAddress(Constants.TrueLeaf), confirmedBalance / 2, Seq.empty, Map.empty)

      val tx1 = await(wallet.generateTransaction(Seq(req1), boxesToUseEncoded)).get
      tx1.outputs.size shouldBe 2
      tx1.outputs.head.value shouldBe (confirmedBalance / 2)
      tx1.outputs.head.additionalTokens.toArray shouldBe Seq.empty
      toAssetMap(tx1.outputs(1).additionalTokens.toArray) shouldBe toAssetMap(Seq.empty)
    }
  }

  property("whitelist not set, ignore auto-burn") {
    implicit val ww: WalletFixture = new WalletFixture(settings
      .copy(walletSettings = settings
        .walletSettings.copy(tokensWhitelist = None)), parameters, getCurrentView(_).vault)

    val pubKey = getPublicKeys.head.pubkey
    val genesisBlock = makeNextBlock(getUtxoState, Seq(makeGenesisTxWithAsset(pubKey, issueAsset = true)))
    val initialBoxes = boxesAvailable(genesisBlock, pubKey)

    val boxesToUseEncoded = initialBoxes.map { box =>
      Base16.encode(ErgoBoxSerializer.toBytes(box))
    }

    applyBlock(genesisBlock) shouldBe 'success
    implicit val patienceConfig: PatienceConfig = PatienceConfig(5.second, 300.millis)
    eventually {
      val confirmedBalance = getConfirmedBalances.walletBalance
      log.error(s"Confirmed balance $confirmedBalance")
      //pay out all the wallet balance:
      val assetToSpend = assetsByTokenId(boxesAvailable(genesisBlock, pubKey)).toSeq
      assetToSpend should not be empty

      val req1 = PaymentRequest(Pay2SAddress(Constants.TrueLeaf), confirmedBalance / 2, Seq.empty, Map.empty)

      val tx1 = await(wallet.generateTransaction(Seq(req1), boxesToUseEncoded)).get
      tx1.outputs.size shouldBe 2
      tx1.outputs.head.value shouldBe (confirmedBalance / 2)
      tx1.outputs.head.additionalTokens.toArray shouldBe Seq.empty
      toAssetMap(tx1.outputs(1).additionalTokens.toArray) shouldBe toAssetMap(assetToSpend)
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
      implicit val patienceConfig: PatienceConfig = PatienceConfig(5.second, 300.millis)
      val (tx, block, assetsToSpend) =
        eventually {
          val snap = getConfirmedBalances
          val assetsToSpend = assetsByTokenId(initialBoxes).toSeq
          assetsToSpend should not be empty

          val sumToSpend = snap.walletBalance / (addresses.length + 1)
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
            VotingData.empty)
          val boxesToSpend = tx.inputs.map(i => genesisTx.outputs.find(o => java.util.Arrays.equals(o.id, i.boxId)).get)
          tx.statefulValidity(boxesToSpend, emptyDataBoxes, context) shouldBe 'success

          val block = makeNextBlock(getUtxoState, Seq(tx))
          applyBlock(block) shouldBe 'success //scan by wallet happens during apply
          (tx, block, assetsToSpend)
        }
      eventually {
        val newSnap = getConfirmedBalances
        val newSumToSpend = newSnap.walletBalance / addresses.length
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
  }

  property("off-chain scan") {
    withFixture { implicit w =>
      val pubKey = getPublicKeys.head.script

      val bs0 = getBalancesWithUnconfirmed
      bs0.walletBalance shouldBe 0
      bs0.walletAssetBalances shouldBe empty

      val balance1 = settings.walletSettings.dustLimit.getOrElse(1000000L) + 1
      val box1 = IndexedSeq(new ErgoBoxCandidate(balance1, pubKey, startHeight, randomNewAsset.toColl))
      wallet.scanOffchain(ErgoTransaction(fakeInputs, box1))

      implicit val patienceConfig: PatienceConfig = PatienceConfig(1.second, 100.millis)
      eventually {
        val bs1 = getBalancesWithUnconfirmed
        bs1.walletBalance shouldBe balance1
        bs1.walletAssetBalances shouldBe assetAmount(box1)
      }

      val balance2 = settings.walletSettings.dustLimit.getOrElse(1000000L) + 1
      val box2 = IndexedSeq(new ErgoBoxCandidate(balance2, pubKey, startHeight, randomNewAsset.toColl))
      wallet.scanOffchain(ErgoTransaction(fakeInputs, IndexedSeq(), box2))

      eventually {
        val bs2 = getBalancesWithUnconfirmed
        bs2.walletBalance shouldBe (balance1 + balance2)
        bs2.walletAssetBalances shouldBe assetAmount(box1 ++ box2)
      }
    }
  }

  property("off-chain box spending") {
    withFixture { implicit w =>
      val address = getPublicKeys.head
      val tx = makeGenesisTx(address.pubkey, randomNewAsset)
      wallet.scanOffchain(tx)
      val boxesToSpend = boxesAvailable(tx, address.pubkey)
      val balanceToSpend = balanceAmount(boxesToSpend)
      log.info(s"Balance to spent: $balanceToSpend")
      implicit val patienceConfig: PatienceConfig = PatienceConfig(offchainScanTime(tx).millis, 100.millis)
      val (spendingTx, balanceToReturn, assetsAfterSpending) =
        eventually {
          val totalBalance = getBalancesWithUnconfirmed.walletBalance
          totalBalance shouldEqual balanceToSpend
          log.info(s"Total balance with unconfirmed: $totalBalance")
          val balanceToReturn = randomLong(balanceToSpend)
          val spendingTx = makeSpendingTx(boxesToSpend, address, balanceToReturn, assetsWithRandom(boxesToSpend))
          val assetsAfterSpending = assetAmount(boxesAvailable(spendingTx, address.pubkey))
          assetsAfterSpending should not be empty
          (spendingTx, balanceToReturn, assetsAfterSpending)
        }
      wallet.scanOffchain(spendingTx)
      eventually {
        val totalAfterSpending = getBalancesWithUnconfirmed

        log.info(s"Balance to return back: $balanceToReturn")
        totalAfterSpending.walletBalance shouldEqual balanceToReturn
        totalAfterSpending.walletAssetBalances shouldEqual assetsAfterSpending
      }
    }
  }

  property("off-chain double registration") {
    withFixture { implicit w =>
      val address = getPublicKeys.head
      val tx = makeGenesisTx(address.pubkey, randomNewAsset)
      wallet.scanOffchain(tx)
      val boxesToSpend = boxesAvailable(tx, address.pubkey)
      val balanceToSpend = balanceAmount(boxesToSpend)
      implicit val patienceConfig: PatienceConfig = PatienceConfig((offchainScanTime(tx) * 3).millis, 100.millis)
      val (spendingTx, totalBalance, balanceToReturn, assets) =
        eventually {
          val totalBalance = getBalancesWithUnconfirmed.walletBalance

          val balanceToReturn = randomLong(balanceToSpend)
          val spendingTx = makeSpendingTx(boxesToSpend, address, balanceToReturn, assetsWithRandom(boxesToSpend))
          //      val doubleSpendingTx = makeSpendingTx(boxesToSpend, address, randomLong(balanceToSpend))
          val assets = assetAmount(boxesAvailable(spendingTx, address.pubkey))
          assets should not be empty
          (spendingTx, totalBalance, balanceToReturn, assets)
        }
      wallet.scanOffchain(Seq(spendingTx, spendingTx))
      wallet.scanOffchain(spendingTx)

      log.info(s"Total with unconfirmed balance: $totalBalance")
      log.info(s"Balance to spent: $balanceToSpend")
      log.info(s"Balance to return back: $balanceToReturn")
      eventually {
        val totalAfterSpending = getBalancesWithUnconfirmed
        totalBalance shouldEqual balanceToSpend
        totalAfterSpending.walletBalance shouldEqual balanceToReturn
        totalAfterSpending.walletAssetBalances shouldEqual assets
      }
    }
  }

  property("off-chain spending of the on-chain box") {
    withFixture { implicit w =>
      val address = getPublicKeys.head
      val genesisBlock = makeGenesisBlock(address.pubkey, randomNewAsset)
      val boxesToSpend = boxesAvailable(genesisBlock, address.pubkey)
      val sumBalance = balanceAmount(boxesToSpend)
      log.info(s"Sum balance: $sumBalance")
      val balanceToReturn = randomLong(sumBalance)
      applyBlock(genesisBlock) shouldBe 'success
      implicit val patienceConfig: PatienceConfig = PatienceConfig(5.second, 300.millis)
      val (spendingTx, assets) =
        eventually {
          val totalBalance = getBalancesWithUnconfirmed.walletBalance
          val confirmedBalance = getConfirmedBalances.walletBalance

          val spendingTx = makeSpendingTx(boxesToSpend, address, balanceToReturn, assetsWithRandom(boxesToSpend))
          val assets = assetAmount(boxesAvailable(spendingTx, address.pubkey))
          assets should not be empty
          confirmedBalance shouldBe sumBalance
          totalBalance shouldBe sumBalance
          log.info(s"Balance before spending: $confirmedBalance")
          log.info(s"Total with unconfirmed balance before spending: $totalBalance")
          (spendingTx, assets)
        }
      wallet.scanOffchain(spendingTx)
      eventually {
        val confirmedAfterSpending = getConfirmedBalances.walletBalance
        val totalAfterSpending = getBalancesWithUnconfirmed

        log.info(s"Balance after spending: $confirmedAfterSpending")
        log.info(s"Total with unconfirmed after spending: $totalAfterSpending")

        confirmedAfterSpending shouldBe sumBalance
        totalAfterSpending.walletBalance shouldBe balanceToReturn
        totalAfterSpending.walletAssetBalances shouldBe assets
      }
    }
  }

  property("assets application") {
    withFixture { implicit w =>
      val address = getPublicKeys.head
      val asset1Sum = randomLong()
      val genesisBlock = makeGenesisBlock(address.pubkey, Seq(newAssetIdStub -> asset1Sum))
      val boxesToSpend = boxesAvailable(genesisBlock, address.pubkey)
      applyBlock(genesisBlock) shouldBe 'success
      implicit val patienceConfig: PatienceConfig = PatienceConfig(5.second, 300.millis)
      val (asset1Token, asset1ToReturn, asset2Sum, spendingBlock) =
        eventually {
          val initialBalance = getConfirmedBalances
          val initialTotal = getBalancesWithUnconfirmed
          val initialAssets = initialBalance.walletAssetBalances
          log.info(s"Initial assets: ${boxesToSpend.flatMap(_.additionalTokens.toArray)}")
          log.info(s"Confirmed: $initialBalance")
          log.info(s"With unconfirmed: $initialTotal")
          initialAssets should not be empty
          val (asset1Token, asset1InitialValue) = initialAssets.head
          asset1InitialValue shouldBe asset1Sum
          initialTotal.walletAssetBalances shouldBe initialAssets

          val asset2Sum = randomLong()
          val asset1ToReturn = randomLong(asset1Sum)
          val assets2Seq = Seq(decodedTokenId(asset1Token) -> asset1ToReturn, newAssetIdStub -> asset2Sum)
          val balanceToReturn = 1000 * parameters.minValuePerByte
          val spendingTx = makeSpendingTx(boxesToSpend, address, balanceToReturn, assets2Seq)
          val spendingBlock = makeNextBlock(getUtxoState, Seq(spendingTx))
          applyBlock(spendingBlock) shouldBe 'success
          (asset1Token, asset1ToReturn, asset2Sum, spendingBlock)
        }
      wallet.scanPersistent(spendingBlock)
      eventually {
        val balanceAfterSpending = getConfirmedBalances
        val totalAfterSpending = getBalancesWithUnconfirmed
        log.info(s"After spending: $balanceAfterSpending")
        log.info(s"With unconfirmed after spending: $balanceAfterSpending")
        val assets = balanceAfterSpending.walletAssetBalances
        totalAfterSpending.walletAssetBalances.toMap shouldBe assets.toMap
        assets.find(_._1 == asset1Token).get._2 shouldBe asset1ToReturn
        val asset2 = assets.filter(_._1 != asset1Token)
        asset2 should not be empty
        asset2.head._2 shouldBe asset2Sum
      }
    }
  }

  property("on-chain box spending (without return)") {
    withFixture { implicit w =>
      val address = getPublicKeys.head
      val genesisBlock = makeGenesisBlock(address.pubkey, randomNewAsset)
      applyBlock(genesisBlock) shouldBe 'success
      implicit val patienceConfig: PatienceConfig = PatienceConfig(5.second, 300.millis)
      val (spendingBlock, boxesToSpend, confirmedBalance, balanceToSpend) =
        eventually {
          val confirmedBalance = getConfirmedBalances.walletBalance
          val boxesToSpend = boxesAvailable(genesisBlock, address.pubkey)
          val balanceToSpend = balanceAmount(boxesToSpend)
          log.info(s"Confirmed balance $confirmedBalance")
          log.info(s"Sum balance: $balanceToSpend")
          confirmedBalance should be > 0L
          confirmedBalance shouldBe balanceToSpend

          val spendingTx = makeSpendingTx(boxesToSpend, address, 0, assetsWithRandom(boxesToSpend))

          val spendingBlock = makeNextBlock(getUtxoState, Seq(spendingTx))
          applyBlock(spendingBlock) shouldBe 'success
          (spendingBlock, boxesToSpend, confirmedBalance, balanceToSpend)
        }
      wallet.scanPersistent(spendingBlock)
      eventually {
        val balanceAfterSpending = getConfirmedBalances
        log.info(s"Boxes to spend: $boxesToSpend")
        log.info(s"Total with unconfirmed balance: $confirmedBalance")
        log.info(s"Balance to spent: $balanceToSpend")
        log.info(s"Balance after spend: ${balanceAfterSpending.walletBalance}")
        balanceAfterSpending.walletBalance shouldEqual 0
        getBalancesWithUnconfirmed shouldEqual balanceAfterSpending
      }
    }
  }

  property("on-chain box spending (with return)") {
    withFixture { implicit w =>
      val address = getPublicKeys.head
      val genesisBlock = makeGenesisBlock(address.pubkey, randomNewAsset)
      applyBlock(genesisBlock) shouldBe 'success
      implicit val patienceConfig: PatienceConfig = PatienceConfig(5.second, 300.millis)
      val (confirmedBalance, balanceToSpend, balanceToReturn, assets, spendingBlock) =
        eventually {
          val confirmedBalance = getConfirmedBalances.walletBalance
          val boxesToSpend = boxesAvailable(genesisBlock, address.pubkey)
          val balanceToSpend = balanceAmount(boxesToSpend)
          log.info(s"Boxes to spend: $boxesToSpend")
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
          (confirmedBalance, balanceToSpend, balanceToReturn, assets, spendingBlock)
        }
      wallet.scanPersistent(spendingBlock)
      eventually {
        val balanceAfterSpending = getConfirmedBalances
        log.info(s"Total with unconfirmed balance: $confirmedBalance")
        log.info(s"Balance to spent: $balanceToSpend")
        log.info(s"Balance to return back: $balanceToReturn")
        balanceAfterSpending.walletBalance shouldEqual (confirmedBalance - balanceToSpend + balanceToReturn)
        balanceAfterSpending.walletAssetBalances.toMap shouldBe assets.toMap

        getBalancesWithUnconfirmed.height shouldEqual balanceAfterSpending.height
        getBalancesWithUnconfirmed.walletBalance shouldEqual balanceAfterSpending.walletBalance
        getBalancesWithUnconfirmed.walletAssetBalances.toMap shouldEqual balanceAfterSpending.walletAssetBalances.toMap
      }
    }
  }

  property("off-chain transaction becomes on-chain") {
    withFixture { implicit w =>
      val pubKey = getPublicKeys.head.pubkey
      val tx = makeGenesisTx(pubKey, randomNewAsset)
      wallet.scanOffchain(tx)
      implicit val patienceConfig: PatienceConfig = PatienceConfig(offchainScanTime(tx).millis, 100.millis)
      val (initialBalance, sumBalance, sumAssets) =
        eventually {
          val boxesToSpend = boxesAvailable(tx, pubKey)
          val sumBalance = balanceAmount(boxesToSpend)
          val sumAssets = assetAmount(boxesToSpend)
          sumAssets should not be empty

          val initialBalance = getBalancesWithUnconfirmed.walletBalance
          initialBalance shouldBe sumBalance

          val block = makeNextBlock(getUtxoState, Seq(tx))
          applyBlock(block) shouldBe 'success
          (initialBalance, sumBalance, sumAssets)
        }

      eventually {
        val confirmedBalance = getConfirmedBalances
        log.info(s"Confirmed balance $confirmedBalance")
        log.info(s"Sum balance: $sumBalance")
        initialBalance shouldBe sumBalance
        confirmedBalance.walletBalance should be > 0L
        confirmedBalance.walletBalance shouldBe initialBalance
        confirmedBalance.walletAssetBalances shouldBe sumAssets
        getBalancesWithUnconfirmed shouldBe confirmedBalance
      }

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
      implicit val patienceConfig: PatienceConfig = PatienceConfig(5.seconds, 100.millis)
      val confirmedBalance =
        eventually {
          val totalBalance = getBalancesWithUnconfirmed.walletBalance
          val confirmedBalance = getConfirmedBalances.walletBalance

          confirmedBalance shouldBe balanceToSpend
          totalBalance shouldBe confirmedBalance
          log.info(s"Initial balance: $initialBalance")
          log.info(s"Balance before off-chain spending: $confirmedBalance")
          log.info(s"Total with unconfirmed balance before spending: $totalBalance")
          confirmedBalance
        }

      val balanceToReturn = randomLong(balanceAmount(boxesToSpend))
      val spendingTx = makeSpendingTx(boxesToSpend, address, balanceToReturn)
      wallet.scanOffchain(spendingTx)

      eventually {
        val confirmedAfterSpending = getConfirmedBalances.walletBalance
        val totalAfterSpending = getBalancesWithUnconfirmed.walletBalance

        confirmedAfterSpending shouldBe confirmedBalance
        totalAfterSpending shouldBe balanceToReturn

        log.info(s"After spending before rollback: $confirmedAfterSpending")
        log.info(s"Total with unconfirmed balance after spending before rollback: $totalAfterSpending")
      }

      wallet.rollback(initialState.version)
      eventually {
        val balanceAfterRollback = getConfirmedBalances.walletBalance
        val totalAfterRollback = getBalancesWithUnconfirmed.walletBalance

        log.info(s"Balance after rollback: $balanceAfterRollback")
        log.info(s"Total with unconfirmed balance after rollback: $totalAfterRollback")

        balanceAfterRollback shouldBe initialBalance
        totalAfterRollback shouldBe balanceToReturn
      }
    }
  }

  property("on-chain rollback") {
    withFixture { implicit w =>
      val pubKey = getPublicKeys.head.pubkey
      val genesisBlock = makeGenesisBlock(pubKey)
      val boxesToSpend = boxesAvailable(genesisBlock, pubKey)
      applyBlock(genesisBlock) shouldBe 'success
      val initialState = getCurrentState
      implicit val patienceConfig: PatienceConfig = PatienceConfig(5.seconds, 100.millis)
      val (initialBalance, creationTx, initialAssets, balanceToSpend) =
        eventually {
          val initialBalance = getConfirmedBalances.walletBalance
          val balanceToSpend = randomLong(balanceAmount(boxesToSpend))
          val creationTx = makeTx(boxesToSpend, emptyProverResult, balanceToSpend, pubKey, randomNewAsset)
          val initialAssets = assetAmount(boxesAvailable(creationTx, pubKey))
          initialAssets should not be empty
          log.info(s"Initial balance: $initialBalance")
          log.info(s"Initial assets: $initialAssets")
          (initialBalance, creationTx, initialAssets, balanceToSpend)
        }

      val block = makeNextBlock(getUtxoState, Seq(creationTx))
      wallet.scanPersistent(block)
      eventually {
        val historyHeight = getHistory.headersHeight

        val confirmedBeforeRollback: WalletDigest = getConfirmedBalances
        val totalBeforeRollback = getBalancesWithUnconfirmed
        log.info(s"History height: $historyHeight")
        log.info(s"Confirmed balance: $confirmedBeforeRollback")
        log.info(s"Total with unconfirmed balance: $totalBeforeRollback")

        confirmedBeforeRollback.walletBalance shouldBe balanceToSpend
        confirmedBeforeRollback.walletAssetBalances shouldBe initialAssets
        totalBeforeRollback shouldBe confirmedBeforeRollback
      }
      wallet.rollback(initialState.version)
      eventually {
        val confirmedAfterRollback = getConfirmedBalances
        val totalAfterRollback = getBalancesWithUnconfirmed

        log.info(s"Balance after rollback: $confirmedAfterRollback")
        log.info(s"Total with unconfirmed balance after rollback: $totalAfterRollback")

        confirmedAfterRollback.walletBalance shouldBe initialBalance
        confirmedAfterRollback.walletAssetBalances shouldBe empty
        totalAfterRollback.walletBalance shouldBe balanceToSpend
        totalAfterRollback.walletAssetBalances shouldBe initialAssets
      }
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
      implicit val patienceConfig: PatienceConfig = PatienceConfig(5.second, 100.millis)
      val (block, initialSnapshot) =
        eventually {
          val initialSnapshot = getConfirmedBalances
          log.info(s"Initial balance: $initialSnapshot")
          val spendingTx = makeSpendingTx(boxesToSpend, address)
          val block = makeNextBlock(getUtxoState, Seq(spendingTx))
          initialSnapshot.walletBalance shouldBe sumBalance
          initialSnapshot.walletAssetBalances shouldBe sumAssets
          (block, initialSnapshot)
        }
      wallet.scanPersistent(block)

      val confirmedBeforeRollback =
        eventually {
          val historyHeight = getHistory.headersHeight

          val confirmedBeforeRollback = getConfirmedBalances
          val totalBeforeRollback = getBalancesWithUnconfirmed

          log.info(s"Balance to spend: $sumBalance")
          log.info(s"History height: $historyHeight")
          log.info(s"Confirmed balance: $confirmedBeforeRollback")
          log.info(s"Total with unconfirmed balance: $totalBeforeRollback")

          confirmedBeforeRollback.walletBalance shouldBe 0L
          confirmedBeforeRollback.walletAssetBalances shouldBe empty
          totalBeforeRollback shouldBe confirmedBeforeRollback
          confirmedBeforeRollback
        }

      wallet.rollback(initialState.version)
      eventually {
        val confirmedAfterRollback = getConfirmedBalances
        val totalAfterRollback = getBalancesWithUnconfirmed
        log.info(s"Balance after rollback: $confirmedAfterRollback")
        log.info(s"Total with unconfirmed balance after rollback: $totalAfterRollback")

        confirmedAfterRollback shouldBe initialSnapshot
        totalAfterRollback.walletBalance shouldBe confirmedBeforeRollback.walletBalance
        totalAfterRollback.walletAssetBalances shouldBe confirmedBeforeRollback.walletAssetBalances
      }
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
      implicit val patienceConfig: PatienceConfig = PatienceConfig(5.seconds, 100.millis)
      val (block, initialSnapshot, asset1Map, balanceToReturn) =
        eventually {
          val initialSnapshot = getConfirmedBalances

          val balanceToReturn = randomLong(sumBalance)
          val sumAsset1 = assetsByTokenId(boxesToSpend).toSeq
          sumAsset1 should not be empty

          val asset1Map = toAssetMap(sumAsset1)
          val assetToReturn = sumAsset1.map { case (tokenId, tokenValue) => (tokenId, randomLong(tokenValue)) }
          val assetsForSpending = randomNewAsset ++ assetToReturn
          val spendingTx = makeSpendingTx(boxesToSpend, address, balanceToReturn, assetsForSpending)
          val block = makeNextBlock(getUtxoState, Seq(spendingTx))
          log.info(s"Initial balance: $initialSnapshot")
          log.info(s"Balance to spend: $sumBalance")
          log.info(s"Balance to return $balanceToReturn")
          initialSnapshot.walletBalance shouldBe sumBalance
          initialSnapshot.walletAssetBalances.toMap shouldBe asset1Map
          (block, initialSnapshot, asset1Map, balanceToReturn)
        }
      wallet.scanPersistent(block)

      val totalBeforeRollback =
        eventually {
          val historyHeight = getHistory.headersHeight
          val confirmedBeforeRollback = getConfirmedBalances
          val totalBeforeRollback = getBalancesWithUnconfirmed
          log.info(s"History height: $historyHeight")
          log.info(s"Confirmed balance: $confirmedBeforeRollback")
          log.info(s"Total with unconfirmed balance: $totalBeforeRollback")
          confirmedBeforeRollback.walletBalance should be > 0L
          confirmedBeforeRollback.walletBalance shouldBe balanceToReturn
          confirmedBeforeRollback.walletAssetBalances should have size 2
          totalBeforeRollback.walletBalance shouldBe balanceToReturn
          totalBeforeRollback.walletAssetBalances.toMap shouldBe confirmedBeforeRollback.walletAssetBalances.toMap
          totalBeforeRollback
        }
      wallet.rollback(initialState.version)

      eventually {
        val confirmedAfterRollback = getConfirmedBalances
        val totalAfterRollback = getBalancesWithUnconfirmed
        log.info(s"Balance after rollback: $confirmedAfterRollback")
        log.info(s"Total with unconfirmed balance after rollback: $totalAfterRollback")
        confirmedAfterRollback shouldBe initialSnapshot
        confirmedAfterRollback.walletAssetBalances.toMap shouldBe asset1Map
        totalAfterRollback.walletBalance shouldBe balanceToReturn
        totalAfterRollback.walletAssetBalances shouldBe totalBeforeRollback.walletAssetBalances
      }
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

      log.info(s"Initial balance: $initialBalance")
      log.info(s"Balance to spend: $balanceToSpend")
      balanceToSpend shouldBe balancePicked

      val balanceToReturn = randomLong(balanceToSpend)
      val sumAsset1 = assetsByTokenId(boxesToSpend).toSeq
      sumAsset1 should not be empty

      val assetToReturn = sumAsset1.map { case (tokenId, tokenValue) => (tokenId, randomLong(tokenValue)) }
      val assetsForSpending = randomNewAsset ++ assetToReturn
      val spendingTx = makeSpendingTx(boxesToSpend, address, balanceToReturn, assetsForSpending)
      val block = makeNextBlock(getUtxoState, Seq(creationTx, spendingTx))
      wallet.scanPersistent(block)

      implicit val patienceConfig: PatienceConfig = PatienceConfig(5.seconds, 100.millis)
      val totalBeforeRollback =
        eventually {
          val historyHeight = getHistory.headersHeight

          val confirmedBeforeRollback = getConfirmedBalances
          val totalBeforeRollback = getBalancesWithUnconfirmed
          log.info(s"History height: $historyHeight")
          log.info(s"Confirmed balance: $confirmedBeforeRollback")
          log.info(s"Total with unconfirmed balance: $totalBeforeRollback")

          confirmedBeforeRollback.walletBalance shouldBe balanceToReturn
          confirmedBeforeRollback.walletAssetBalances should have size 2

          totalBeforeRollback.walletBalance shouldBe confirmedBeforeRollback.walletBalance
          totalBeforeRollback.walletAssetBalances.toMap shouldBe confirmedBeforeRollback.walletAssetBalances.toMap
          totalBeforeRollback
        }
      wallet.rollback(initialState.version)

      eventually {
        val confirmedAfterRollback = getConfirmedBalances
        val totalAfterRollback = getBalancesWithUnconfirmed

        log.info(s"Balance after rollback: $confirmedAfterRollback")
        log.info(s"Total with unconfirmed balance after rollback: $totalAfterRollback")

        confirmedAfterRollback.walletBalance shouldBe initialBalance
        totalAfterRollback.walletBalance shouldBe balanceToReturn
        totalAfterRollback.walletAssetBalances shouldBe totalBeforeRollback.walletAssetBalances
      }
    }
  }

  property("single-input transaction generation") {
    withFixture { implicit w =>
      val pubKey = getPublicKeys.head.pubkey
      val genesisBlock = makeGenesisBlock(pubKey, randomNewAsset)
      applyBlock(genesisBlock) shouldBe 'success
      implicit val patienceConfig: PatienceConfig = PatienceConfig(5.seconds, 100.millis)
      eventually {
        val confirmedBalance = getConfirmedBalances.walletBalance

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
  }

  property("generate unsigned transaction + sign (single input)") {
    withFixture { implicit w =>
      val pubKey = getPublicKeys.head.pubkey
      val genesisBlock = makeGenesisBlock(pubKey, randomNewAsset)
      applyBlock(genesisBlock) shouldBe 'success
      implicit val patienceConfig: PatienceConfig = PatienceConfig(5.seconds, 100.millis)
      eventually {
        val confirmedBalance = getConfirmedBalances.walletBalance

        //pay out all the wallet balance:
        val assetToSpend = assetsByTokenId(boxesAvailable(genesisBlock, pubKey)).toSeq
        assetToSpend should not be empty
        val req1 = PaymentRequest(Pay2SAddress(Constants.TrueLeaf), confirmedBalance, assetToSpend, Map.empty)

        val utx = await(wallet.generateUnsignedTransaction(Seq(req1))).get
        utx.outputs.size shouldBe 1
        utx.outputs.head.value shouldBe confirmedBalance
        toAssetMap(utx.outputs.head.additionalTokens.toArray) shouldBe toAssetMap(assetToSpend)

        val tx = await(wallet.signTransaction(utx, Seq.empty, TransactionHintsBag.empty, None, None)).get
        tx.id shouldBe utx.id // signing preserves transaction id
      }
    }
  }

  property("co-signing (external secrets) - 2-out-of-2") {
    withFixture { implicit w =>

      val secret1 = DLogProverInput.random()
      val es1 = ExternalSecret(PrimitiveSecretKey(secret1))

      val secret2 = DLogProverInput.random()
      val es2 = ExternalSecret(PrimitiveSecretKey(secret2))

      val pubKey = getPublicKeys.head.pubkey
      val genesisBlock = makeGenesisBlock(pubKey, randomNewAsset)
      applyBlock(genesisBlock) shouldBe 'success
      implicit val patienceConfig: PatienceConfig = PatienceConfig(5.seconds, 100.millis)
      eventually {
        val confirmedBalance = getConfirmedBalances.walletBalance

        //pay out all the wallet balance:
        val assetToSpend = assetsByTokenId(boxesAvailable(genesisBlock, pubKey)).toSeq
        assetToSpend should not be empty
        val req1 = PaymentRequest(Pay2SAddress(CAND(Seq(secret1.publicImage, secret2.publicImage))), confirmedBalance, assetToSpend, Map.empty)

        val tx = await(wallet.generateTransaction(Seq(req1))).get

        val in = tx.outputs.head

        val utx = new UnsignedErgoTransaction(IndexedSeq(new UnsignedInput(in.id)), IndexedSeq.empty, IndexedSeq(in.toCandidate))

        val hints1 = await(wallet.generateCommitmentsFor(utx, Some(Seq(es1)), Some(Seq(in)), None)).response.get

        val txSigned = await(wallet.signTransaction(utx, Seq(es2), hints1, Some(Seq(in)), None)).get

        txSigned.statelessValidity().isSuccess shouldBe true
      }
    }
  }

  property("co-signing (external secrets) - 2-out-of-3") {
    withFixture { implicit w =>
      val secret1 = DLogProverInput.random()
      val es1 = ExternalSecret(PrimitiveSecretKey(secret1))

      val secret2 = DLogProverInput.random()
      val es2 = ExternalSecret(PrimitiveSecretKey(secret2))

      val secret3 = DLogProverInput.random()

      val pubKey = getPublicKeys.head.pubkey
      val genesisBlock = makeGenesisBlock(pubKey, randomNewAsset)
      applyBlock(genesisBlock) shouldBe 'success
      implicit val patienceConfig: PatienceConfig = PatienceConfig(5.seconds, 100.millis)
      eventually {
        val confirmedBalance = getConfirmedBalances.walletBalance

        //pay out all the wallet balance:
        val assetToSpend = assetsByTokenId(boxesAvailable(genesisBlock, pubKey)).toSeq
        assetToSpend should not be empty
        val addr = Pay2SAddress(CTHRESHOLD(2, Seq(secret1.publicImage, secret2.publicImage, secret3.publicImage)))
        val req1 = PaymentRequest(addr, confirmedBalance, assetToSpend, Map.empty)

        val tx = await(wallet.generateTransaction(Seq(req1))).get

        val in = tx.outputs.head

        // secret1 and secret2 are signing
        val utx = new UnsignedErgoTransaction(IndexedSeq(new UnsignedInput(in.id)), IndexedSeq.empty, IndexedSeq(in.toCandidate))

        val cmts1 = await(wallet.generateCommitmentsFor(utx, Some(Seq(es1)), Some(Seq(in)), None)).response.get

        val pubCmts1 = TransactionHintsBag(cmts1.publicHints)

        val ptx = await(wallet.signTransaction(utx, Seq(es2), pubCmts1, Some(Seq(in)), None)).get

        val eh = wallet.extractHints(ptx, Seq(secret1.publicImage, secret2.publicImage), Seq(secret3.publicImage), Some(Seq(in)), None)
        val hintsExtracted = await(eh).transactionHintsBag

        val hints = hintsExtracted.addHintsForInput(0, cmts1.allHintsForInput(0))

        val txSigned = await(wallet.signTransaction(utx, Seq(es1), hints, Some(Seq(in)), None)).get
        txSigned.statelessValidity().isSuccess shouldBe true
      }
    }
  }

}
