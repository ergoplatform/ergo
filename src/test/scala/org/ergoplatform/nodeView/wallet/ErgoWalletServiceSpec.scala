package org.ergoplatform.nodeView.wallet

import org.ergoplatform.ErgoBox.{NonMandatoryRegisterId, R1}
import org.ergoplatform._
import org.ergoplatform.db.DBSpec
import org.ergoplatform.modifiers.mempool.{ErgoTransaction, UnconfirmedTransaction}
import org.ergoplatform.nodeView.mempool.ErgoMemPoolReader
import org.ergoplatform.nodeView.wallet.WalletScanLogic.ScanResults
import org.ergoplatform.nodeView.wallet.persistence.{OffChainRegistry, WalletRegistry, WalletStorage}
import org.ergoplatform.nodeView.wallet.requests.{AssetIssueRequest, PaymentRequest}
import org.ergoplatform.nodeView.wallet.scanning.{EqualsScanningPredicate, ScanRequest, ScanWalletInteraction}
import org.ergoplatform.sdk.SecretString
import org.ergoplatform.sdk.wallet.secrets.{DerivationPath, DlogSecretKey, ExtendedSecretKey}
import org.ergoplatform.settings.Constants.TrueTree
import org.ergoplatform.settings.ErgoSettings
import org.ergoplatform.utils.fixtures.WalletFixture
import org.ergoplatform.utils.generators.ErgoNodeTransactionGenerators.validErgoTransactionGen
import org.ergoplatform.utils.{ErgoCorePropertyTest, MempoolTestHelpers, WalletTestOps}
import org.ergoplatform.wallet.Constants.{PaymentsScanId, ScanId}
import org.ergoplatform.wallet.boxes.BoxSelector.BoxSelectionResult
import org.ergoplatform.wallet.boxes.{ErgoBoxSerializer, ReplaceCompactCollectBoxSelector, TrackedBox}
import org.ergoplatform.wallet.crypto.ErgoSignature
import org.ergoplatform.wallet.interpreter.{ErgoProvingInterpreter, TransactionHintsBag}
import org.ergoplatform.wallet.mnemonic.Mnemonic
import org.ergoplatform.wallet.secrets.Wif
import org.scalacheck.Gen
import org.scalatest.BeforeAndAfterAll
import scorex.db.{LDBKVStore, LDBVersionedStore}
import scorex.util.encode.Base16
import sigma.Extensions.ArrayOps
import sigma.crypto.BigIntegers
import sigma.ast.{ByteArrayConstant, ErgoTree, EvaluatedValue, FalseLeaf, SType}
import sigma.interpreter.ContextExtension
import sigmastate.helpers.TestingHelpers.testBox
import scorex.util.ModifierId

import scala.collection.compat.immutable.ArraySeq
import scala.util.{Random, Try}

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
      importedSecretsOpt = Option.empty,
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

  property("WIF import survives lock/unlock and round-trips through export") {
    withVersionedStore(2) { versionedStore =>
      withStore { store =>
        val pass = SecretString.create(Random.nextString(10))
        val walletService = new ErgoWalletServiceImpl(settings)
        val ws0 = initialState(store, versionedStore)

        // initWallet only creates the encrypted file; we have to unlock to get
        // a usable in-memory master + prover.
        val initialized = walletService.initWallet(ws0, settings, pass, Option.empty).get._2
        val ready = walletService.unlockWallet(
          walletService.lockWallet(initialized), pass, usePreEip3Derivation = false
        ).get

        // Use a fixed scalar so the test is deterministic
        val scalar: Array[Byte] = Base16.decode(
          "0c28fca386c7a227600b2fe50b7cae11ec86d3bf1fbe471be89827e19d72aa1d"
        ).get
        val inputWif = Wif.encode(scalar, mainnet = settings.chainSettings.isMainnet)

        val (importedAddr, afterImport) = walletService.importPrivateKeyWif(ready, inputWif).get
        afterImport.importedSecretsOpt.get.secrets.get.length shouldBe 1
        afterImport.walletVars.publicKeyAddresses should contain(importedAddr)

        // Re-export must produce the same WIF (since we encoded with the wallet's network byte)
        walletService.exportPrivateKeyWif(afterImport, importedAddr).get shouldBe inputWif

        // Lock: imported plaintext is zeroed
        val locked = walletService.lockWallet(afterImport)
        locked.importedSecretsOpt.get.isLocked shouldBe true
        locked.walletVars.proverOpt shouldBe empty

        // Unlock again: imported key reappears with the same address
        val unlocked = walletService.unlockWallet(locked, pass, usePreEip3Derivation = false).get
        unlocked.importedSecretsOpt.get.isLocked shouldBe false
        unlocked.walletVars.publicKeyAddresses should contain(importedAddr)
        walletService.exportPrivateKeyWif(unlocked, importedAddr).get shouldBe inputWif

        // The prover holds the imported scalar so it can sign with it
        val proverSecrets = unlocked.walletVars.proverOpt.get.secretKeys
        val hasImported = proverSecrets.exists {
          case dlog: DlogSecretKey =>
            BigIntegers.asUnsignedByteArray(Wif.SecretLength, dlog.privateInput.w).sameElements(scalar)
          case _ => false
        }
        hasImported shouldBe true
      }
    }
  }

  property("imported WIF key can sign a transaction before and after lock/unlock") {
    withVersionedStore(2) { versionedStore =>
      withStore { store =>
        val pass = SecretString.create(Random.nextString(10))
        val walletService = new ErgoWalletServiceImpl(settings)
        val initialized = walletService.initWallet(
          initialState(store, versionedStore), settings, pass, Option.empty
        ).get._2
        val unlocked = walletService.unlockWallet(
          walletService.lockWallet(initialized), pass, usePreEip3Derivation = false
        ).get

        val scalar = Base16.decode(
          "0c28fca386c7a227600b2fe50b7cae11ec86d3bf1fbe471be89827e19d72aa1d"
        ).get
        val wif = Wif.encode(scalar, mainnet = settings.chainSettings.isMainnet)
        val (importedAddr, withImported) = walletService.importPrivateKeyWif(unlocked, wif).get

        // Build a tx that spends a box locked to the imported P2PK and sends it to itself.
        val value = 100000000L
        val height = 10000
        val box = new ErgoBoxCandidate(value, ErgoTree.fromSigmaBoolean(importedAddr.pubkey), height)
        val fakeTxId = ModifierId @@ Base16.encode(Array.fill(32)(5: Byte))
        val inputBox = box.toBox(fakeTxId, 0.toShort)
        val unsignedInput = new UnsignedInput(inputBox.id, ContextExtension.empty)
        val utx = new UnsignedErgoLikeTransaction(IndexedSeq(unsignedInput), IndexedSeq.empty, IndexedSeq(box))

        def signWith(state: ErgoWalletState): Try[_] = state.walletVars.proverOpt.get.sign(
          utx, IndexedSeq(inputBox), IndexedSeq.empty, state.stateContext, TransactionHintsBag.empty
        )

        signWith(withImported).isSuccess shouldBe true

        // Lock and unlock; the imported key must still sign the same tx.
        val reUnlocked = walletService.unlockWallet(
          walletService.lockWallet(withImported), pass, usePreEip3Derivation = false
        ).get
        signWith(reUnlocked).isSuccess shouldBe true
      }
    }
  }

  property("WIF import rejects duplicate scalar") {
    withVersionedStore(2) { versionedStore =>
      withStore { store =>
        val pass = SecretString.create(Random.nextString(10))
        val walletService = new ErgoWalletServiceImpl(settings)
        val initialized = walletService.initWallet(
          initialState(store, versionedStore), settings, pass, Option.empty
        ).get._2
        val ready = walletService.unlockWallet(
          walletService.lockWallet(initialized), pass, usePreEip3Derivation = false
        ).get

        val scalar = Base16.decode(
          "11d28fca386c7a227600b2fe50b7cae11ec86d3bf1fbe471be89827e19d72ab2"
        ).get
        val wif = Wif.encode(scalar, mainnet = settings.chainSettings.isMainnet)

        val afterFirst = walletService.importPrivateKeyWif(ready, wif).get._2
        walletService.importPrivateKeyWif(afterFirst, wif).isFailure shouldBe true
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
          importedSecretsOpt = Option.empty,
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
          importedSecretsOpt = Option.empty,
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

}
