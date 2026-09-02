package org.ergoplatform.nodeView.wallet

import org.ergoplatform.ErgoBox.{NonMandatoryRegisterId, R1}
import org.ergoplatform._
import org.ergoplatform.db.DBSpec
import org.ergoplatform.modifiers.mempool.{ErgoTransaction, UnconfirmedTransaction}
import org.ergoplatform.nodeView.mempool.ErgoMemPoolReader
import org.ergoplatform.nodeView.wallet.WalletScanLogic.ScanResults
import org.ergoplatform.nodeView.wallet.persistence.{OffChainRegistry, WalletRegistry, WalletStorage}
import org.ergoplatform.nodeView.wallet.requests.{AssetIssueRequest, BurnTokensRequest, PaymentRequest}
import org.ergoplatform.nodeView.wallet.scanning.{EqualsScanningPredicate, ScanRequest, ScanWalletInteraction}
import org.ergoplatform.sdk.SecretString
import org.ergoplatform.sdk.wallet.secrets.{DerivationPath, ExtendedSecretKey}
import org.ergoplatform.settings.Constants.TrueTree
import org.ergoplatform.settings.ErgoSettings
import org.ergoplatform.utils.fixtures.WalletFixture
import org.ergoplatform.utils.generators.ErgoNodeTransactionGenerators.validErgoTransactionGen
import org.ergoplatform.utils.{ErgoCorePropertyTest, MempoolTestHelpers, WalletTestOps}
import org.ergoplatform.wallet.Constants.{PaymentsScanId, ScanId}
import org.ergoplatform.wallet.boxes.BoxSelector.BoxSelectionResult
import org.ergoplatform.wallet.boxes.{ErgoBoxSerializer, ReplaceCompactCollectBoxSelector, TrackedBox}
import org.ergoplatform.wallet.crypto.{ErgoSignature, MessageSigning}
import org.ergoplatform.wallet.interpreter.ErgoProvingInterpreter
import org.ergoplatform.wallet.mnemonic.Mnemonic
import org.scalacheck.Gen
import org.scalatest.BeforeAndAfterAll
import scorex.db.{LDBKVStore, LDBVersionedStore}
import scorex.util.encode.Base16
import sigma.Extensions.ArrayOps
import sigma.ast.{ByteArrayConstant, EvaluatedValue, FalseLeaf, SType}
import sigmastate.crypto.DLogProtocol.DLogProverInput
import sigmastate.eval.Extensions._
import sigmastate.helpers.TestingHelpers.testBox

import scala.collection.compat.immutable.ArraySeq
import scala.util.Random

class ErgoWalletServiceSpec
  extends ErgoCorePropertyTest
    with MempoolTestHelpers
    with WalletTestOps
    with ErgoWalletSupport
    with DBSpec
    with BeforeAndAfterAll {
  import org.ergoplatform.utils.ErgoCoreTestConstants._
  import org.ergoplatform.utils.ErgoNodeTestConstants._
  import org.ergoplatform.utils.generators.ErgoNodeWalletGenerators._
  import org.ergoplatform.utils.generators.CoreObjectGenerators._
  import org.ergoplatform.utils.generators.ErgoCoreGenerators._
  import org.ergoplatform.utils.generators.ErgoCoreTransactionGenerators._

  override val ergoSettings: ErgoSettings = settings

  private implicit val x: WalletFixture = new WalletFixture(settings, parameters, getCurrentView(_).vault)
  implicit override val generatorDrivenConfig: PropertyCheckConfiguration = PropertyCheckConfiguration(minSuccessful = 4, sizeRange = 4)
  private lazy val pks = getPublicKeys.toList
  private val masterKey = ExtendedSecretKey.deriveMasterKey(Mnemonic.toSeed(SecretString.create("edge talent poet tortoise trumpet dose")), usePre1627KeyDerivation = false)

  override def afterAll(): Unit = try super.afterAll() finally x.stop()

  private def initialState(store: LDBKVStore, versionedStore: LDBVersionedStore, mempool: Option[ErgoMemPoolReader] = None) = {
    ErgoWalletState(
      new WalletStorage(store, settings),
      secretStorageOpt = Option.empty,
      new WalletRegistry(versionedStore)(settings.walletSettings),
      OffChainRegistry.empty,
      outputsFilter = Option.empty,
      WalletVars(Some(defaultProver), Seq.empty, None),
      stateReaderOpt = Option.empty,
      mempoolReaderOpt = mempool,
      utxoStateReaderOpt = Option.empty,
      parameters,
      maxInputsToUse = 1000,
      rescanInProgress = false
    )
  }

  property("restoring wallet should fail if pruning is enabled") {
    withVersionedStore(2) { versionedStore =>
      withStore { store =>
        val walletState = initialState(store, versionedStore)
        val walletService = new ErgoWalletServiceImpl(settings)
        val settingsWithPruning = settings.copy(nodeSettings = settings.nodeSettings.copy(blocksToKeep = 0))
        walletService.restoreWallet(
          walletState,
          settingsWithPruning,
          mnemonic = SecretString.create("x"),
          mnemonicPassOpt = None,
          walletPass = SecretString.create("y"),
          usePre1627KeyDerivation = false
        ).failed.get.getMessage shouldBe "Unable to restore wallet when pruning is enabled"
      }
    }
  }

  property("it should prepare unsigned transaction") {
    val inputBoxes = {
      Seq(
        TrackedBox(
          ErgoLikeTransaction(IndexedSeq(), IndexedSeq()),
          creationOutIndex = 0,
          None,
          testBox(1L, TrueTree, 0),
          Set(PaymentsScanId)
        )
      )
    }

    forAll(ergoBoxCandidateGen, ergoBoxCandidateGen, validErgoTransactionGen, proveDlogGen) {
      case (outputCandidate, outputChangeCandidate, (ergoBoxes, _), proveDlog) =>
        val selectionResult = new BoxSelectionResult(inputBoxes, Seq(outputChangeCandidate), None)
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
        val paymentRequest = PaymentRequest(pks.head, 1, Array.empty, Map.empty)
        val paymentCandidates = requestsToBoxCandidates(Seq(paymentRequest), ergoBoxes.head.id, startHeight, parameters, pks).get
        paymentCandidates shouldBe List(new ErgoBoxCandidate(value = 1, ergoTree = pks.head.script, startHeight))
    }
  }

  property("it should generate valid box candidates from asset issue requests") {
    forAll(validErgoTransactionGen) {
      case (ergoBoxes, _) =>
        val ergoBox = ergoBoxes.head

        val registers: Option[Map[NonMandatoryRegisterId, EvaluatedValue[_ <: SType]]] = Option(Map(ErgoBox.R4 -> FalseLeaf))
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
        validCandidate.additionalTokens.toArray.toMap shouldBe Map(ergoBox.id.toColl -> 1)
        validCandidate.creationHeight shouldBe startHeight
        validCandidate.ergoTree shouldBe pks.head.script
    }
  }

  property("it should get scan confirmed and unconfirmed transactions") {
    forAll(Gen.nonEmptyListOf(trackedBoxGen), modifierIdGen) { case (boxes, txId) =>
      withVersionedStore(10) { versionedStore =>
        withStore { store =>
          val allBoxes = {
            val unspentBoxes = boxes.map(bx => bx.copy(spendingHeightOpt = None, spendingTxIdOpt = None, scans = Set(ScanId @@ 0.shortValue())))
            val spentBox = boxes.head.copy(spendingHeightOpt = Some(100), spendingTxIdOpt = Some(txId), scans = Set(ScanId @@ 0.shortValue()))
            unspentBoxes :+ spentBox
          }
          val encodedBoxes = allBoxes.map { box =>
            Base16.encode(ErgoBoxSerializer.toBytes(box.box))
          }

          val paymentRequest = PaymentRequest(pks.head, 50000, Array.empty, Map.empty)
          val boxSelector = new ReplaceCompactCollectBoxSelector(settings.walletSettings.maxInputs, settings.walletSettings.optimalInputs, None)

          val walletService = new ErgoWalletServiceImpl(ergoSettings)
          val unconfirmedTx = UnconfirmedTransaction(
            walletService.generateTransaction(
              initialState(store, versionedStore),
              boxSelector,
              Seq(paymentRequest),
              inputsRaw = encodedBoxes,
              dataInputsRaw = Seq.empty,
              sign = true
            ).get.asInstanceOf[ErgoTransaction], None)

          // let's create wallet state with an unconfirmed transaction in mempool
          val wState = initialState(store, versionedStore, Some(new FakeMempool(Seq(unconfirmedTx))))
          val signedTx1 =
            walletService.generateTransaction(wState, boxSelector, Seq(paymentRequest), inputsRaw = encodedBoxes, dataInputsRaw = Seq.empty, sign = true)
              .get.asInstanceOf[ErgoTransaction]
          val walletTx1 = WalletTransaction(signedTx1, 100, Seq(ScanId @@ 0.shortValue()))

          // let's update wallet registry with a transaction from a block
          val genesisBlock = makeGenesisBlock(pks.head.pubkey, randomNewAsset)
          wState.registry.updateOnBlock(ScanResults(allBoxes, ArraySeq.empty, Seq(walletTx1)), genesisBlock.id, blockHeight = 100).get

          // transaction should be retrieved by only a scan id that was associated with it
          val txs1 = walletService.getScanTransactions(wState, ScanId @@ 0.shortValue(), 100)
          assert(txs1.nonEmpty)
          val txs2 = walletService.getScanTransactions(wState, ScanId @@ 1.shortValue(), 100)
          assert(txs2.isEmpty)

          // let's test that unconfirmed transaction is retrieved
          val scanId =
            walletService.addScan(wState, ScanRequest("foo", EqualsScanningPredicate(R1, ByteArrayConstant(pks.head.script.bytes)), Some(ScanWalletInteraction.Off), Some(false)))
              .get._1.scanId

          val txs3 = walletService.getScanTransactions(wState, scanId, 100, includeUnconfirmed = true)
          txs3.size shouldBe 1

          txs3.head.wtx.tx.id shouldBe unconfirmedTx.transaction.id
        }
      }
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
          wState.registry.updateOnBlock(ScanResults(allBoxes, ArraySeq.empty, ArraySeq.empty), blockId, 100).get

          val walletService = new ErgoWalletServiceImpl(settings)
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
        val paymentRequest = PaymentRequest(pks.head, 50000, Array.empty, Map.empty)
        val boxSelector = new ReplaceCompactCollectBoxSelector(settings.walletSettings.maxInputs, settings.walletSettings.optimalInputs, None)

        val (tx, inputs, dataInputs) = generateUnsignedTransaction(wState, boxSelector, Seq(paymentRequest), inputsRaw = encodedBoxes, dataInputsRaw = Seq.empty).get
        dataInputs shouldBe empty
        inputs.size shouldBe 1
        tx.inputs.size shouldBe 1
        tx.outputs.size shouldBe 2
        tx.outputs.map(_.value).sum shouldBe inputs.map(_.value).sum

        val walletService = new ErgoWalletServiceImpl(settings)
        val signedTx = walletService.generateTransaction(wState, boxSelector, Seq(paymentRequest), inputsRaw = encodedBoxes, dataInputsRaw = Seq.empty, sign = true).get.asInstanceOf[ErgoTransaction]

        ErgoSignature.verify(signedTx.messageToSign, signedTx.inputs.head.spendingProof.proof, pks.head.pubkey.value) shouldBe true
        signedTx.inputs.size shouldBe 1
        signedTx.outputs.size shouldBe 2

      }
    }
  }

  property("asset issuance should be independent of burn request order") {
    withVersionedStore(2) { versionedStore =>
      withStore { store =>
        val wState = initialState(store, versionedStore)
        val existingAssetAmount = 10L
        val burnAmount = 3L
        val issueAmount = 7L
        val inputBoxes = boxesAvailable(
          makeGenesisBlock(pks.head.pubkey, Seq(newAssetIdStub -> existingAssetAmount)),
          pks.head.pubkey
        )
        val existingTokenId = inputBoxes.flatMap(_.additionalTokens.toArray).head._1
        val encodedBoxes = inputBoxes.map(box => Base16.encode(ErgoBoxSerializer.toBytes(box)))
        val burnRequest = BurnTokensRequest(Array(existingTokenId -> burnAmount))
        val paymentRequest = PaymentRequest(pks.head, 1000000L, Array.empty, Map.empty)
        val issueRequest = AssetIssueRequest(
          address = pks.head,
          valueOpt = Some(10000000L),
          amount = issueAmount,
          name = "test-name",
          description = "test-description",
          decimals = 4,
          registers = Option.empty
        )
        val boxSelector = new ReplaceCompactCollectBoxSelector(
          settings.walletSettings.maxInputs,
          settings.walletSettings.optimalInputs,
          None
        )

        val requestOrders = Seq(
          Seq(burnRequest, issueRequest),
          Seq(issueRequest, burnRequest)
        ) ++ Seq(burnRequest, issueRequest, paymentRequest).permutations.toSeq

        requestOrders.foreach { requests =>
          val result = generateUnsignedTransaction(
            wState,
            boxSelector,
            requests,
            inputsRaw = encodedBoxes,
            dataInputsRaw = Seq.empty
          )
          val requestOrder = requests.map(_.getClass.getSimpleName).mkString(", ")
          withClue(s"request order: $requestOrder; failure: ${result.failed.map(_.getMessage).toOption}") {
            result.isSuccess shouldBe true
          }

          val (tx, selectedInputs, _) = result.get
          val issuedTokenId = selectedInputs.head.id.toTokenId
          val issueOutputs = tx.outputCandidates.filter(
            _.additionalTokens.toArray.exists { case (tokenId, _) => tokenId == issuedTokenId }
          )
          issueOutputs should have size 1
          issueOutputs.head.value shouldBe issueRequest.valueOpt.get
          issueOutputs.head.ergoTree shouldBe pks.head.script
          issueOutputs.head.additionalTokens.toArray should contain(issuedTokenId -> issueAmount)
          issueOutputs.head.additionalRegisters shouldBe Map(
            ErgoBox.R4 -> ByteArrayConstant("test-name".getBytes("UTF-8")),
            ErgoBox.R5 -> ByteArrayConstant("test-description".getBytes("UTF-8")),
            ErgoBox.R6 -> ByteArrayConstant("4".getBytes("UTF-8"))
          )

          if (requests.contains(paymentRequest)) {
            val paymentOutputs = tx.outputCandidates.filter(_.value == paymentRequest.value)
            paymentOutputs should have size 1
            paymentOutputs.head.ergoTree shouldBe pks.head.script
            paymentOutputs.head.additionalTokens.toArray shouldBe empty
            paymentOutputs.head.additionalRegisters shouldBe empty
          }

          selectedInputs
            .flatMap(_.additionalTokens.toArray)
            .collect { case (tokenId, amount) if tokenId == issuedTokenId => amount }
            .sum shouldBe 0L
          tx.outputCandidates
            .flatMap(_.additionalTokens.toArray)
            .collect { case (tokenId, amount) if tokenId == issuedTokenId => amount }
            .sum shouldBe issueAmount

          val selectedExistingAmount = selectedInputs
            .flatMap(_.additionalTokens.toArray)
            .collect { case (tokenId, amount) if tokenId == existingTokenId => amount }
            .sum
          val outputExistingAmount = tx.outputCandidates
            .flatMap(_.additionalTokens.toArray)
            .collect { case (tokenId, amount) if tokenId == existingTokenId => amount }
            .sum
          selectedExistingAmount - outputExistingAmount shouldBe burnAmount
          selectedInputs.map(_.value).sum shouldBe tx.outputCandidates.map(_.value).sum
        }
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
        val walletService = new ErgoWalletServiceImpl(settings)
        val pass = Random.nextString(10)
        val initializedState = walletService.initWallet(walletState, settings, SecretString.create(pass), Option.empty).get._2

        // Wallet unlocked after init, so we're locking it
        val initLockedWalletState = walletService.lockWallet(initializedState)
        initLockedWalletState.secretStorageOpt.get.isLocked shouldBe true
        initLockedWalletState.walletVars.proverOpt shouldBe empty

        val unlockedWalletState = walletService.unlockWallet(initLockedWalletState, SecretString.create(pass), usePreEip3Derivation = true).get
        unlockedWalletState.secretStorageOpt.get.isLocked shouldBe false
        unlockedWalletState.storage.readAllKeys().size shouldBe 1
        unlockedWalletState.walletVars.proverOpt shouldNot be(empty)

        val lockedWalletState = walletService.lockWallet(unlockedWalletState)
        lockedWalletState.secretStorageOpt.get.isLocked shouldBe true
        lockedWalletState.walletVars.proverOpt shouldBe empty

        val finalUnlockedState = walletService.unlockWallet(lockedWalletState, SecretString.create(pass), usePreEip3Derivation = true).get
        finalUnlockedState.secretStorageOpt.get.isLocked shouldBe false
        finalUnlockedState.storage.readAllKeys().size shouldBe 1
        finalUnlockedState.walletVars.proverOpt shouldNot be(empty)
      }
    }
  }

  property("it should derive private key correctly") {
    withVersionedStore(2) { versionedStore =>
      withStore { store =>

        val pass = SecretString.create(Random.nextString(10))
        val mnemonic = "edge talent poet tortoise trumpet dose"

        val walletService = new ErgoWalletServiceImpl(settings)
        val ws1 = initialState(store, versionedStore)
        val ws2 = walletService.initWallet(ws1, settings, pass, Some(SecretString.create(mnemonic))).get._2
        ws2.secretStorageOpt.get.unlock(pass)

        val path = DerivationPath.fromEncoded("m/44/1/1/0/0").get
        val sk = ws2.secretStorageOpt.get.secret.get
        val pk = sk.derive(path).publicKey

        walletService.getPrivateKeyFromPath(ws2, pk.path).get.w shouldBe sk.derive(path).privateInput.w
      }
    }
  }

  property("key derivation after init wallet") {
    withVersionedStore(2) { versionedStore =>
      withStore { store =>
        val wpass = SecretString.create("y")
        val prover = ErgoProvingInterpreter(defaultRootSecret, parameters)
        val walletState = ErgoWalletState(
          new WalletStorage(store, settings),
          secretStorageOpt = Option.empty,
          new WalletRegistry(versionedStore)(settings.walletSettings),
          OffChainRegistry.empty,
          outputsFilter = Option.empty,
          WalletVars(Some(prover), Seq.empty, None),
          stateReaderOpt = Option.empty,
          mempoolReaderOpt = None,
          utxoStateReaderOpt = Option.empty,
          parameters,
          maxInputsToUse = 1000,
          rescanInProgress = false
        )
        val s = settings.copy(nodeSettings = settings.nodeSettings.copy(blocksToKeep = -1))
        val walletService = new ErgoWalletServiceImpl(s)
        val ws = walletService.initWallet(
          walletState,
          s,
          walletPass = wpass,
          None
        ).get._2

        ws.secretStorageOpt.get.unlock(wpass)
        ws.walletVars.trackedPubKeys.size shouldBe 1
        val uws = ws

        val uws2 = walletService.deriveNextKey(uws, usePreEip3Derivation = true).get._2
        uws2.walletVars.trackedPubKeys.size shouldBe 2

        val uws3 = walletService.deriveNextKey(uws2, usePreEip3Derivation = false).get._2
        uws3.walletVars.trackedPubKeys.size shouldBe 3
      }
    }
  }

  property("key derivation after restoring wallet") {
    withVersionedStore(2) { versionedStore =>
      withStore { store =>
        val wpass = SecretString.create("y")
        val prover = ErgoProvingInterpreter(defaultRootSecret, parameters)
        val walletState = ErgoWalletState(
          new WalletStorage(store, settings),
          secretStorageOpt = Option.empty,
          new WalletRegistry(versionedStore)(settings.walletSettings),
          OffChainRegistry.empty,
          outputsFilter = Option.empty,
          WalletVars(Some(prover), Seq.empty, None),
          stateReaderOpt = Option.empty,
          mempoolReaderOpt = None,
          utxoStateReaderOpt = Option.empty,
          parameters,
          maxInputsToUse = 1000,
          rescanInProgress = false
        )
        val s = settings.copy(nodeSettings = settings.nodeSettings.copy(blocksToKeep = -1))
        val walletService = new ErgoWalletServiceImpl(s)
        val ws = walletService.restoreWallet(
          walletState,
          s,
          mnemonic = SecretString.create("x"),
          mnemonicPassOpt = None,
          walletPass = wpass,
          usePre1627KeyDerivation = false
        ).get

        ws.secretStorageOpt.get.unlock(wpass)
        ws.walletVars.trackedPubKeys.size shouldBe 1
        val uws = ws

        val uws2 = walletService.deriveNextKey(uws, false).get._2
        uws2.walletVars.trackedPubKeys.size shouldBe 2

        val uws3 = walletService.deriveNextKey(uws2, false).get._2
        uws3.walletVars.trackedPubKeys.size shouldBe 3
      }
    }
  }


  private val messageToSign = "I am the owner of this address".getBytes("UTF-8")

  property("a signed message verifies for the address it was signed with, and for nothing else") {
    withVersionedStore(2) { versionedStore =>
      withStore { store =>
        val walletService = new ErgoWalletServiceImpl(settings)
        val wState = initialState(store, versionedStore)
        val pubKey = defaultProver.hdPubKeys.head.key

        val signed = walletService.signMessage(wState, messageToSign, None).get

        signed.address shouldBe P2PKAddress(pubKey)(settings.addressEncoder)
        // the message is never signed as given, it is wrapped first
        signed.signedMessage shouldNot equal(messageToSign)
        MessageSigning.unwrap(signed.signedMessage).get shouldBe messageToSign
        MessageSigning.verify(pubKey, signed.signedMessage, signed.proof) shouldBe true

        // signing the same message twice gives two different byte strings, both valid
        val signedAgain = walletService.signMessage(wState, messageToSign, None).get
        signedAgain.signedMessage shouldNot equal(signed.signedMessage)
        MessageSigning.verify(pubKey, signedAgain.signedMessage, signedAgain.proof) shouldBe true

        // another key does not verify it
        val otherKey = defaultProver.hdPubKeys.last.key
        otherKey shouldNot equal(pubKey)
        MessageSigning.verify(otherKey, signed.signedMessage, signed.proof) shouldBe false

        // neither does a message which is not the one signed
        val tampered = signed.signedMessage.dropRight(1) :+ (signed.signedMessage.last + 1).toByte
        MessageSigning.verify(pubKey, tampered, signed.proof) shouldBe false

        // nor a proof which is not the one produced
        MessageSigning.verify(pubKey, signed.signedMessage, signedAgain.proof) shouldBe false
      }
    }
  }

  property("a message signature can not be turned into a transaction proof") {
    withVersionedStore(2) { versionedStore =>
      withStore { store =>
        val walletService = new ErgoWalletServiceImpl(settings)
        val wState = initialState(store, versionedStore)
        val pubKey = defaultProver.hdPubKeys.head.key

        // the attack this guards against: hand the wallet the bytes a transaction spending the
        // owner's boxes is signed over, dressed up as a message. A sigma proof over those bytes is
        // exactly the input proof which would make that transaction valid.
        val encodedBoxes =
          boxesAvailable(makeGenesisBlock(pks.head.pubkey, randomNewAsset), pks.head.pubkey)
            .map(box => Base16.encode(ErgoBoxSerializer.toBytes(box)))
        val boxSelector = new ReplaceCompactCollectBoxSelector(
          settings.walletSettings.maxInputs, settings.walletSettings.optimalInputs, None)
        val paymentRequest = PaymentRequest(pks.head, 50000, Array.empty, Map.empty)
        val victimTx = walletService.generateTransaction(
          wState, boxSelector, Seq(paymentRequest), encodedBoxes, Seq.empty, sign = true)
          .get.asInstanceOf[ErgoTransaction]
        val bytesTheTransactionIsSignedOver = victimTx.messageToSign

        val signed = walletService.signMessage(wState, bytesTheTransactionIsSignedOver, None).get

        // the wallet signed the wrapped string, so the proof says nothing about the transaction
        new SigmaPropVerifier()
          .verifySignature(pubKey, bytesTheTransactionIsSignedOver, signed.proof)(null)
          .shouldBe(false)

        // and the other way round: a proof over raw transaction bytes is not a message signature,
        // because verification insists on the wrapping
        MessageSigning.unwrap(bytesTheTransactionIsSignedOver) shouldBe None
        MessageSigning.verify(pubKey, bytesTheTransactionIsSignedOver,
          victimTx.inputs.head.spendingProof.proof) shouldBe false
      }
    }
  }

  property("signing a message needs an unlocked wallet holding the address asked for") {
    withVersionedStore(2) { versionedStore =>
      withStore { store =>
        val walletService = new ErgoWalletServiceImpl(settings)
        val wState = initialState(store, versionedStore)

        val foreignAddress = P2PKAddress(
          DLogProverInput(BigInt(41).bigInteger).publicImage)(settings.addressEncoder)
        walletService.signMessage(wState, messageToSign, Some(foreignAddress)) shouldBe 'failure

        val locked = wState.copy(walletVars = wState.walletVars.resetProver())
        walletService.signMessage(locked, messageToSign, None) shouldBe 'failure
      }
    }
  }

}
