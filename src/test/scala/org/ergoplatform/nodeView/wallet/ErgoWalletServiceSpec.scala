package org.ergoplatform.nodeView.wallet

import org.ergoplatform.ErgoBox.NonMandatoryRegisterId
import org.ergoplatform._
import org.ergoplatform.db.DBSpec
import org.ergoplatform.modifiers.mempool.ErgoTransaction
import org.ergoplatform.nodeView.wallet.WalletScanLogic.ScanResults
import org.ergoplatform.nodeView.wallet.persistence.{OffChainRegistry, WalletRegistry, WalletStorage}
import org.ergoplatform.nodeView.wallet.requests.{AssetIssueRequest, PaymentRequest}
import org.ergoplatform.utils.fixtures.WalletFixture
import org.ergoplatform.utils.generators.ErgoTransactionGenerators
import org.ergoplatform.utils.{ErgoPropertyTest, WalletTestOps}
import org.ergoplatform.wallet.Constants.PaymentsScanId
import org.ergoplatform.wallet.boxes.BoxSelector.BoxSelectionResult
import org.ergoplatform.wallet.boxes.{ErgoBoxSerializer, ReplaceCompactCollectBoxSelector, TrackedBox}
import org.ergoplatform.wallet.crypto.ErgoSignature
import org.ergoplatform.wallet.mnemonic.Mnemonic
import org.ergoplatform.wallet.secrets.ExtendedSecretKey
import org.scalacheck.Gen
import org.scalatest.BeforeAndAfterAll
import scorex.db.{LDBKVStore, LDBVersionedStore}
import scorex.util.encode.Base16
import sigmastate.Values.{ByteArrayConstant, EvaluatedValue}
import sigmastate.helpers.TestingHelpers.testBox
import sigmastate.{SType, Values}

import scala.util.Random

class ErgoWalletServiceSpec extends ErgoPropertyTest with WalletTestOps with ErgoWalletSupport with ErgoTransactionGenerators with DBSpec with BeforeAndAfterAll {

  private implicit val x: WalletFixture = new WalletFixture(settings, getCurrentView(_).vault)
  implicit override val generatorDrivenConfig: PropertyCheckConfiguration = PropertyCheckConfiguration(minSuccessful = 4, sizeRange = 4)
  private lazy val pks = getPublicKeys.toList
  private val masterKey = ExtendedSecretKey.deriveMasterKey(Mnemonic.toSeed("edge talent poet tortoise trumpet dose"))

  override def afterAll(): Unit = try super.afterAll() finally x.stop()

  private def initialState(store: LDBKVStore, versionedStore: LDBVersionedStore) = {
    ErgoWalletState(
      new WalletStorage(store, settings),
      secretStorageOpt = Option.empty,
      new WalletRegistry(versionedStore)(settings.walletSettings),
      OffChainRegistry.empty,
      outputsFilter = Option.empty,
      WalletVars(Some(defaultProver), Seq.empty, None),
      stateReaderOpt = Option.empty,
      mempoolReaderOpt = Option.empty,
      utxoStateReaderOpt = Option.empty,
      parameters
    )
  }

  property("it should prepare unsigned transaction") {
    val inputBoxes = {
      Seq(
        TrackedBox(
          ErgoLikeTransaction(IndexedSeq(), IndexedSeq()),
          creationOutIndex = 0,
          None,
          testBox(1L, Values.TrueLeaf.toSigmaProp, 0),
          Set(PaymentsScanId)
        )
      )
    }

    forAll(ergoBoxCandidateGen, ergoBoxCandidateGen, validErgoTransactionGen, proveDlogGen) {
      case (outputCandidate, outputChangeCandidate, (ergoBoxes, _), proveDlog) =>
        val selectionResult = BoxSelectionResult(inputBoxes, Seq(outputChangeCandidate))
        val tx = prepareUnsignedTransaction(Seq(outputCandidate), startHeight, selectionResult, ergoBoxes, Option(proveDlog)).get
        tx.inputs shouldBe inputBoxes.map(_.box.id).map(id => new UnsignedInput(id))
        tx.dataInputs shouldBe ergoBoxes.map(dataInputBox => DataInput(dataInputBox.id))
        tx.outputCandidates.size shouldBe 2
        tx.outputCandidates.map(_.value).sum shouldBe outputCandidate.value + outputChangeCandidate.value

        val txWithChangeBoxesButNoChangeAddress =
          prepareUnsignedTransaction(Seq(outputCandidate), startHeight, selectionResult, ergoBoxes, Option.empty)
        txWithChangeBoxesButNoChangeAddress.isFailure shouldBe true
    }
  }

  property("it should generate valid box candidates from payment request") {
    forAll(validErgoTransactionGen) {
      case (ergoBoxes, _) =>
        val paymentRequest = PaymentRequest(pks.head, 1, Seq.empty, Map.empty)
        val paymentCandidates = requestsToBoxCandidates(Seq(paymentRequest), ergoBoxes.head.id, startHeight, parameters, pks).get
        paymentCandidates shouldBe List(new ErgoBoxCandidate(value = 1, ergoTree = pks.head.script, startHeight))
    }
  }

  property("it should generate valid box candidates from asset issue requests") {
    forAll(validErgoTransactionGen) {
      case (ergoBoxes, _) =>
        val ergoBox = ergoBoxes.head

        val registers: Option[Map[NonMandatoryRegisterId, EvaluatedValue[_ <: SType]]] = Option(Map(ErgoBox.R4 -> sigmastate.Values.FalseLeaf))
        val illegalAssetIssueRequest = AssetIssueRequest(address = pks.head, Some(1), amount = 1, "test", "test", 4, registers)
        val invalidCandidates = requestsToBoxCandidates(Seq(illegalAssetIssueRequest), ergoBox.id, startHeight, parameters, pks)
        invalidCandidates.failed.get.getMessage shouldBe "Additional registers contain R0...R6"

        val assetIssueRequestWithoutAddress = AssetIssueRequest(addressOpt = Option.empty, Some(1), amount = 1, "test", "test", 4, Option.empty)
        val missingAddressCandidates = requestsToBoxCandidates(Seq(assetIssueRequestWithoutAddress), ergoBox.id, startHeight, parameters, Seq.empty)
        missingAddressCandidates.failed.get.getMessage shouldBe "No address available for box locking"

        val assetIssueRequestWithoutValue = AssetIssueRequest(address = pks.head, valueOpt = Option.empty, amount = 1, "test", "test", 4, Option.empty)
        val missingValueCandidates = requestsToBoxCandidates(Seq(assetIssueRequestWithoutValue), ergoBox.id, startHeight, parameters, Seq.empty).get.head
        missingValueCandidates.value > 0 shouldBe true

        val assetIssueRequest = AssetIssueRequest(address = pks.head, Some(1), amount = 1, "test-name", "test-description", 4, Option.empty)
        val validCandidate = requestsToBoxCandidates(Seq(assetIssueRequest), ergoBox.id, startHeight, parameters, Seq.empty).get.head
        validCandidate.value shouldBe 1
        validCandidate.additionalRegisters shouldBe
          Map(
            ErgoBox.R4 -> ByteArrayConstant("test-name".getBytes("UTF-8")),
            ErgoBox.R5 -> ByteArrayConstant("test-description".getBytes("UTF-8")),
            ErgoBox.R6 -> ByteArrayConstant("4".getBytes("UTF-8")),
          )
        validCandidate.additionalTokens.toMap shouldBe Map(ergoBox.id -> 1)
        validCandidate.creationHeight shouldBe startHeight
        validCandidate.ergoTree shouldBe pks.head.script
    }
  }

  property("it should get spent and unspent wallet boxes") {
    forAll(Gen.nonEmptyListOf(trackedBoxGen), modifierIdGen) { case (boxes, txId) =>
      withVersionedStore(10) { versionedStore =>
        withStore { store =>
          val wState = initialState(store, versionedStore)
          val blockId = modifierIdGen.sample.get
          val unspentBoxes = boxes.map(bx => bx.copy(spendingHeightOpt = None, spendingTxIdOpt = None, scans = Set(PaymentsScanId)))
          val spentBox = boxes.head.copy(spendingHeightOpt = Some(10000), spendingTxIdOpt = Some(txId), scans = Set(PaymentsScanId))
          val allBoxes = unspentBoxes :+ spentBox
          wState.registry.updateOnBlock(ScanResults(allBoxes, Seq.empty, Seq.empty), blockId, 100)

          val walletService = new ErgoWalletServiceImpl
          val actualUnspentOnlyWalletBoxes = walletService.getWalletBoxes(wState, unspentOnly = true, considerUnconfirmed = false).toList
          val expectedUnspentOnlyWalletBoxes = unspentBoxes.map(x => WalletBox(x, wState.fullHeight)).sortBy(_.trackedBox.inclusionHeightOpt)
          actualUnspentOnlyWalletBoxes should contain theSameElementsAs expectedUnspentOnlyWalletBoxes

          val actualWalletBoxes = walletService.getWalletBoxes(wState, unspentOnly = false, considerUnconfirmed = false).toList
          val expectedWalletBoxes = allBoxes.map(x => WalletBox(x, wState.fullHeight)).sortBy(_.trackedBox.inclusionHeightOpt)
          actualWalletBoxes should contain theSameElementsAs expectedWalletBoxes
        }
      }
    }
  }

  property("it should generate signed and unsigned transaction") {
    withVersionedStore(2) { versionedStore =>
      withStore { store =>
        val wState = initialState(store, versionedStore)

        val encodedBoxes =
          boxesAvailable(makeGenesisBlock(pks.head.pubkey, randomNewAsset), pks.head.pubkey)
            .map { box =>
              Base16.encode(ErgoBoxSerializer.toBytes(box))
            }
        val paymentRequest = PaymentRequest(pks.head, 50000, Seq.empty, Map.empty)
        val boxSelector = new ReplaceCompactCollectBoxSelector(settings.walletSettings.maxInputs, settings.walletSettings.optimalInputs)

        val (tx, inputs, dataInputs) = generateUnsignedTransaction(wState, boxSelector, Seq(paymentRequest), inputsRaw = encodedBoxes, dataInputsRaw = Seq.empty).get
        dataInputs shouldBe empty
        inputs.size shouldBe 1
        tx.inputs.size shouldBe 1
        tx.outputs.size shouldBe 2
        tx.outputs.map(_.value).sum shouldBe inputs.map(_.value).sum

        val walletService = new ErgoWalletServiceImpl
        val signedTx = walletService.generateTransaction(wState, boxSelector, Seq(paymentRequest), inputsRaw = encodedBoxes, dataInputsRaw = Seq.empty, sign = true).get.asInstanceOf[ErgoTransaction]

        ErgoSignature.verify(signedTx.messageToSign, signedTx.inputs.head.spendingProof.proof, pks.head.pubkey.h) shouldBe true
        signedTx.inputs.size shouldBe 1
        signedTx.outputs.size shouldBe 2

      }
    }
  }

  property("it should process unlock using preEip3Derivation") {
    withVersionedStore(2) { versionedStore =>
      withStore { store =>
        val walletState = initialState(store, versionedStore)
        val unlockedWalletState = processUnlock(walletState, masterKey, usePreEip3Derivation = true).get
        unlockedWalletState.storage.readAllKeys().size shouldBe 1
        unlockedWalletState.storage.readAllKeys() should contain(masterKey.publicKey)
        unlockedWalletState.walletVars.proverOpt shouldNot be(empty)
      }
    }
  }

  property("it should process unlock without preEip3Derivation") {
    withVersionedStore(2) { versionedStore =>
      withStore { store =>
        val walletState = initialState(store, versionedStore)
        val unlockedWalletState = processUnlock(walletState, masterKey, usePreEip3Derivation = false).get
        unlockedWalletState.storage.readAllKeys().size shouldBe 1
        unlockedWalletState.storage.readChangeAddress shouldNot be(empty)
        unlockedWalletState.walletVars.proverOpt shouldNot be(empty)
      }
    }
  }

  property("it should lock/unlock wallet") {
    withVersionedStore(2) { versionedStore =>
      withStore { store =>
        val walletState = initialState(store, versionedStore)
        val walletService = new ErgoWalletServiceImpl
        val pass = Random.nextString(10)
        val (mnemonic, initializedState) = walletService.initWallet(walletState, settings, pass, Option.empty).get

        // Wallet unlocked after init, so we're locking it
        val initLockedWalletState = walletService.lockWallet(initializedState)
        initLockedWalletState.secretStorageOpt.get.isLocked shouldBe true
        initLockedWalletState.walletVars.proverOpt shouldBe empty

        val unlockedWalletState = walletService.unlockWallet(initLockedWalletState, pass, usePreEip3Derivation = true).get
        unlockedWalletState.secretStorageOpt.get.isLocked shouldBe false
        unlockedWalletState.storage.readAllKeys().size shouldBe 1
        unlockedWalletState.walletVars.proverOpt shouldNot be(empty)

        val lockedWalletState = walletService.lockWallet(unlockedWalletState)
        lockedWalletState.secretStorageOpt.get.isLocked shouldBe true
        lockedWalletState.walletVars.proverOpt shouldBe empty

        val finalUnlockedState = walletService.unlockWallet(lockedWalletState, pass, usePreEip3Derivation = true).get
        finalUnlockedState.secretStorageOpt.get.isLocked shouldBe false
        finalUnlockedState.storage.readAllKeys().size shouldBe 1
        finalUnlockedState.walletVars.proverOpt shouldNot be(empty)
      }
    }
  }
}
