package org.ergoplatform.nodeView.wallet

import org.ergoplatform.ErgoBox.{NonMandatoryRegisterId, R1}
import org.ergoplatform._
import org.ergoplatform.db.DBSpec
import org.ergoplatform.modifiers.mempool.{ErgoTransaction, UnconfirmedTransaction}
import org.ergoplatform.nodeView.mempool.ErgoMemPoolReader
import org.ergoplatform.nodeView.wallet.WalletScanLogic.ScanResults
import org.ergoplatform.nodeView.wallet.persistence.{OffChainRegistry, WalletDigest, WalletRegistry, WalletStorage}
import org.ergoplatform.nodeView.wallet.requests.{AssetIssueRequest, PaymentRequest}
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
import org.ergoplatform.wallet.crypto.ErgoSignature
import org.ergoplatform.wallet.interpreter.ErgoProvingInterpreter
import org.ergoplatform.wallet.mnemonic.Mnemonic
import org.scalacheck.Gen
import org.scalatest.BeforeAndAfterAll
import scorex.db.{LDBKVStore, LDBVersionedStore}
import scorex.util.encode.Base16
import sigma.Extensions.ArrayOps
import sigma.ast.{ByteArrayConstant, EvaluatedValue, FalseLeaf, SType}
import sigmastate.helpers.TestingHelpers.testBox

import java.io.{File, IOException}
import java.nio.file.{Files, LinkOption, Path}
import java.util.UUID
import scala.collection.JavaConverters._
import scala.collection.compat.immutable.ArraySeq
import scala.collection.mutable.ArrayBuffer
import scala.util.{Failure, Random, Try}

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

  private def initialState(store: LDBKVStore,
                           versionedStore: LDBVersionedStore,
                           mempool: Option[ErgoMemPoolReader] = None): ErgoWalletState = {
    initialState(store, new WalletRegistry(versionedStore)(settings.walletSettings), mempool)
  }

  private def initialState(store: LDBKVStore,
                           registry: WalletRegistry,
                           mempool: Option[ErgoMemPoolReader]): ErgoWalletState = {
    ErgoWalletState(
      new WalletStorage(store, settings),
      secretStorageOpt = Option.empty,
      registry,
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

  private def isolatedSettings(): ErgoSettings = {
    val directory = createTempDir.getAbsolutePath
    settings.copy(
      directory = directory,
      walletSettings = settings.walletSettings.copy(
        secretStorage = settings.walletSettings.secretStorage.copy(
          secretDir = s"$directory/wallet/keystore"
        )
      )
    )
  }

  private def retiredRegistryFolders(settings: ErgoSettings): Seq[Path] = {
    val registryPath = WalletRegistry.registryFolder(settings).toPath
    val parent = registryPath.getParent
    val prefix = s"${registryPath.getFileName}.retired-"
    if (parent == null || Files.notExists(parent)) {
      Seq.empty
    } else {
      val stream = Files.list(parent)
      try stream.iterator().asScala.filter { path =>
        val name = path.getFileName.toString
        name.startsWith(prefix) && {
          val suffix = name.substring(prefix.length)
          Try(UUID.fromString(suffix)).toOption.exists { uuid =>
            uuid.toString == suffix && uuid.version() == 4 && uuid.variant() == 2
          } && Files.isDirectory(path, LinkOption.NOFOLLOW_LINKS)
        }
      }.toList
      finally stream.close()
    }
  }

  private def openTestRegistry(
    settings: ErgoSettings,
    closeFailure: Option[Throwable],
    onClose: () => Unit,
    fetchFailure: () => Option[Throwable] = () => None,
    onFetch: () => Unit = () => ()
  ): WalletRegistry = {
    val registryFolder = WalletRegistry.registryFolder(settings)
    registryFolder.mkdirs()
    val versionedStore = new LDBVersionedStore(
      registryFolder,
      settings.nodeSettings.keepVersions
    )
    if (!versionedStore.versionIdExists(WalletRegistry.PreGenesisStateVersion)) {
      versionedStore
        .update(WalletRegistry.PreGenesisStateVersion, Seq.empty, Seq.empty)
        .get
    }
    new WalletRegistry(versionedStore)(settings.walletSettings) {
      override def fetchDigest(): WalletDigest = {
        onFetch()
        fetchFailure() match {
          case Some(error) => throw error
          case None => super.fetchDigest()
        }
      }

      override def close(): Unit = {
        onClose()
        super.close()
        closeFailure.foreach(throw _)
      }
    }
  }

  property("recovery-specific registry reset should defer after close failure when canonical fallback is readable") {
    withStore { store =>
      val isolatedSettings = this.isolatedSettings()
      val closeFailure = new IOException("injected registry close failure")
      var inputCloseCount = 0
      val registry = openTestRegistry(
        isolatedSettings,
        closeFailure = Some(closeFailure),
        onClose = () => inputCloseCount += 1
      )
      registry.updateOnBlock(
        ScanResults(Seq.empty, ArraySeq.empty, Seq.empty),
        modifierIdGen.sample.get,
        blockHeight = 7
      ).get
      val originalDigest = registry.fetchDigest()
      originalDigest should not be WalletDigest.empty
      val walletState = initialState(store, registry, None)
      var moveCount = 0
      var openCount = 0
      val walletService = new ErgoWalletServiceImpl(isolatedSettings) {
        override protected[wallet] def moveRegistryToTombstone(
          registryFolder: File
        ): Try[Option[Path]] = {
          moveCount += 1
          super.moveRegistryToTombstone(registryFolder)
        }

        override protected[wallet] def openRegistry(settings: ErgoSettings): Try[WalletRegistry] = {
          openCount += 1
          super.openRegistry(settings)
        }
      }

      walletService.recreateRegistryForUtxoSnapshotRecovery(walletState, isolatedSettings) match {
        case ErgoWalletService.RegistryResetDeferred(recoveredState, cause) =>
          try {
            (cause eq closeFailure) shouldBe true
            (recoveredState.registry eq registry) shouldBe false
            recoveredState.registry.fetchDigest() shouldBe originalDigest
          } finally recoveredState.registry.close()
        case other =>
          fail(s"Expected deferred registry reset, got $other")
      }

      inputCloseCount shouldBe 1
      moveCount shouldBe 0
      openCount shouldBe 1
    }
  }

  property("recovery-specific registry reset should retain close and fallback-open failures") {
    withStore { store =>
      val isolatedSettings = this.isolatedSettings()
      val closeFailure = new IOException("injected registry close failure")
      val openFailure = new IOException("injected canonical fallback open failure")
      var inputCloseCount = 0
      val registry = openTestRegistry(
        isolatedSettings,
        closeFailure = Some(closeFailure),
        onClose = () => inputCloseCount += 1
      )
      val walletState = initialState(store, registry, None)
      var moveCount = 0
      var openCount = 0
      val walletService = new ErgoWalletServiceImpl(isolatedSettings) {
        override protected[wallet] def moveRegistryToTombstone(
          registryFolder: File
        ): Try[Option[Path]] = {
          moveCount += 1
          super.moveRegistryToTombstone(registryFolder)
        }

        override protected[wallet] def openRegistry(settings: ErgoSettings): Try[WalletRegistry] = {
          openCount += 1
          Failure(openFailure)
        }
      }

      walletService.recreateRegistryForUtxoSnapshotRecovery(walletState, isolatedSettings) match {
        case ErgoWalletService.RegistryResetUnavailable(cause) =>
          (cause eq closeFailure) shouldBe true
          cause.getSuppressed.exists(_ eq openFailure) shouldBe true
        case other =>
          fail(s"Expected unavailable registry reset, got $other")
      }

      inputCloseCount shouldBe 1
      moveCount shouldBe 0
      openCount shouldBe 1
    }
  }

  property("recovery-specific registry reset should defer after move failure without cleanup") {
    withStore { store =>
      val isolatedSettings = this.isolatedSettings()
      val moveFailure = new IOException("injected registry quarantine move failure")
      var inputCloseCount = 0
      val registry = openTestRegistry(
        isolatedSettings,
        closeFailure = None,
        onClose = () => inputCloseCount += 1
      )
      registry.updateOnBlock(
        ScanResults(Seq.empty, ArraySeq.empty, Seq.empty),
        modifierIdGen.sample.get,
        blockHeight = 8
      ).get
      val originalDigest = registry.fetchDigest()
      val walletState = initialState(store, registry, None)
      var moveCount = 0
      var openCount = 0
      var cleanupCount = 0
      val walletService = new ErgoWalletServiceImpl(isolatedSettings) {
        override protected[wallet] def moveRegistryToTombstone(
          registryFolder: File
        ): Try[Option[Path]] = {
          moveCount += 1
          Failure(moveFailure)
        }

        override protected[wallet] def openRegistry(settings: ErgoSettings): Try[WalletRegistry] = {
          openCount += 1
          super.openRegistry(settings)
        }

        override protected[wallet] def deleteRegistryTombstone(path: Path): Try[Unit] = {
          cleanupCount += 1
          super.deleteRegistryTombstone(path)
        }
      }

      walletService.recreateRegistryForUtxoSnapshotRecovery(walletState, isolatedSettings) match {
        case ErgoWalletService.RegistryResetDeferred(recoveredState, cause) =>
          try {
            (cause eq moveFailure) shouldBe true
            (recoveredState.registry eq registry) shouldBe false
            recoveredState.registry.fetchDigest() shouldBe originalDigest
          } finally recoveredState.registry.close()
        case other =>
          fail(s"Expected deferred registry reset, got $other")
      }

      inputCloseCount shouldBe 1
      moveCount shouldBe 1
      openCount shouldBe 1
      cleanupCount shouldBe 0
    }
  }

  property("recovery-specific registry reset should retain move and fallback-open failures") {
    withStore { store =>
      val isolatedSettings = this.isolatedSettings()
      val moveFailure = new IOException("injected registry quarantine move failure")
      val openFailure = new IOException("injected canonical fallback open failure")
      var inputCloseCount = 0
      val registry = openTestRegistry(
        isolatedSettings,
        closeFailure = None,
        onClose = () => inputCloseCount += 1
      )
      val walletState = initialState(store, registry, None)
      var moveCount = 0
      var openCount = 0
      val walletService = new ErgoWalletServiceImpl(isolatedSettings) {
        override protected[wallet] def moveRegistryToTombstone(
          registryFolder: File
        ): Try[Option[Path]] = {
          moveCount += 1
          Failure(moveFailure)
        }

        override protected[wallet] def openRegistry(settings: ErgoSettings): Try[WalletRegistry] = {
          openCount += 1
          Failure(openFailure)
        }
      }

      walletService.recreateRegistryForUtxoSnapshotRecovery(walletState, isolatedSettings) match {
        case ErgoWalletService.RegistryResetUnavailable(cause) =>
          (cause eq moveFailure) shouldBe true
          cause.getSuppressed.exists(_ eq openFailure) shouldBe true
        case other =>
          fail(s"Expected unavailable registry reset, got $other")
      }

      inputCloseCount shouldBe 1
      moveCount shouldBe 1
      openCount shouldBe 1
    }
  }

  property("recovery-specific registry reset should recover a fresh registry after the first open fails") {
    withStore { store =>
      val isolatedSettings = this.isolatedSettings()
      val firstOpenFailure = new IOException("injected first fresh registry open failure")
      var inputCloseCount = 0
      val registry = openTestRegistry(
        isolatedSettings,
        closeFailure = None,
        onClose = () => inputCloseCount += 1
      )
      registry.updateOnBlock(
        ScanResults(Seq.empty, ArraySeq.empty, Seq.empty),
        modifierIdGen.sample.get,
        blockHeight = 9
      ).get
      val walletState = initialState(store, registry, None)
      val events = ArrayBuffer.empty[String]
      var openCount = 0
      val walletService = new ErgoWalletServiceImpl(isolatedSettings) {
        override protected[wallet] def openRegistry(settings: ErgoSettings): Try[WalletRegistry] = {
          openCount += 1
          events += s"open-$openCount"
          if (openCount == 1) Failure(firstOpenFailure)
          else Try(openTestRegistry(
            settings,
            closeFailure = None,
            onClose = () => (),
            onFetch = () => events += "validate"
          ))
        }

        override protected[wallet] def deleteRegistryTombstone(path: Path): Try[Unit] = {
          events += "cleanup"
          super.deleteRegistryTombstone(path)
        }
      }

      walletService.recreateRegistryForUtxoSnapshotRecovery(walletState, isolatedSettings) match {
        case ErgoWalletService.RegistryResetReady(recoveredState, recoveredFrom) =>
          try {
            recoveredFrom.exists(_ eq firstOpenFailure) shouldBe true
            (recoveredState.registry eq registry) shouldBe false
            recoveredState.registry.fetchDigest() shouldBe WalletDigest.empty
            (recoveredState.storage eq walletState.storage) shouldBe true
          } finally recoveredState.registry.close()
        case other =>
          fail(s"Expected ready registry reset, got $other")
      }

      inputCloseCount shouldBe 1
      openCount shouldBe 2
      events.take(4) shouldBe Seq("open-1", "open-2", "validate", "cleanup")
      retiredRegistryFolders(isolatedSettings) shouldBe empty
    }
  }

  property("recovery-specific registry reset should retain the tombstone when both fresh opens fail") {
    withStore { store =>
      val isolatedSettings = this.isolatedSettings()
      val firstOpenFailure = new IOException("injected first fresh registry open failure")
      val fallbackOpenFailure = new IOException("injected fallback registry open failure")
      val registry = openTestRegistry(
        isolatedSettings,
        closeFailure = None,
        onClose = () => ()
      )
      val walletState = initialState(store, registry, None)
      var tombstone: Option[Path] = None
      var openCount = 0
      var cleanupCount = 0
      val walletService = new ErgoWalletServiceImpl(isolatedSettings) {
        override protected[wallet] def moveRegistryToTombstone(
          registryFolder: File
        ): Try[Option[Path]] = super.moveRegistryToTombstone(registryFolder).map { moved =>
          tombstone = moved
          moved
        }

        override protected[wallet] def openRegistry(settings: ErgoSettings): Try[WalletRegistry] = {
          openCount += 1
          if (openCount == 1) Failure(firstOpenFailure) else Failure(fallbackOpenFailure)
        }

        override protected[wallet] def deleteRegistryTombstone(path: Path): Try[Unit] = {
          cleanupCount += 1
          super.deleteRegistryTombstone(path)
        }
      }

      walletService.recreateRegistryForUtxoSnapshotRecovery(walletState, isolatedSettings) match {
        case ErgoWalletService.RegistryResetUnavailable(cause) =>
          (cause eq firstOpenFailure) shouldBe true
          cause.getSuppressed.exists(_ eq fallbackOpenFailure) shouldBe true
        case other =>
          fail(s"Expected unavailable registry reset, got $other")
      }

      openCount shouldBe 2
      cleanupCount shouldBe 0
      tombstone.isDefined shouldBe true
      Files.exists(tombstone.get) shouldBe true
    }
  }

  property("recovery-specific registry reset should defer with a non-empty fallback after rejecting an unreadable candidate") {
    withStore { store =>
      val isolatedSettings = this.isolatedSettings()
      val validationFailure = new IOException("injected first candidate digest failure")
      val inputRegistry = openTestRegistry(
        isolatedSettings,
        closeFailure = None,
        onClose = () => ()
      )
      val walletState = initialState(store, inputRegistry, None)
      var firstCandidateCloseCount = 0
      var firstCandidate: Option[WalletRegistry] = None
      var failValidation = false
      var openCount = 0
      var tombstone: Option[Path] = None
      var cleanupCount = 0
      val walletService = new ErgoWalletServiceImpl(isolatedSettings) {
        override protected[wallet] def moveRegistryToTombstone(
          registryFolder: File
        ): Try[Option[Path]] = super.moveRegistryToTombstone(registryFolder).map { moved =>
          tombstone = moved
          moved
        }

        override protected[wallet] def openRegistry(settings: ErgoSettings): Try[WalletRegistry] = {
          openCount += 1
          if (openCount == 1) Try {
            val candidate = openTestRegistry(
              settings,
              closeFailure = None,
              onClose = () => firstCandidateCloseCount += 1,
              fetchFailure = () => if (failValidation) Some(validationFailure) else None
            )
            candidate.updateOnBlock(
              ScanResults(Seq.empty, ArraySeq.empty, Seq.empty),
              modifierIdGen.sample.get,
              blockHeight = 10
            ).get
            firstCandidate = Some(candidate)
            failValidation = true
            candidate
          } else super.openRegistry(settings)
        }

        override protected[wallet] def deleteRegistryTombstone(path: Path): Try[Unit] = {
          cleanupCount += 1
          super.deleteRegistryTombstone(path)
        }
      }

      walletService.recreateRegistryForUtxoSnapshotRecovery(walletState, isolatedSettings) match {
        case ErgoWalletService.RegistryResetDeferred(recoveredState, cause) =>
          try {
            (cause eq validationFailure) shouldBe true
            (recoveredState.registry eq inputRegistry) shouldBe false
            (recoveredState.registry eq firstCandidate.get) shouldBe false
            recoveredState.registry.fetchDigest() should not be WalletDigest.empty
          } finally recoveredState.registry.close()
        case other =>
          fail(s"Expected deferred registry reset, got $other")
      }

      openCount shouldBe 2
      firstCandidateCloseCount shouldBe 1
      cleanupCount shouldBe 0
      tombstone.isDefined shouldBe true
      Files.exists(tombstone.get) shouldBe true
    }
  }

  property("recovery-specific registry reset should return a fresh ready registry and clean up after validation") {
    withStore { store =>
      val isolatedSettings = this.isolatedSettings()
      var inputCloseCount = 0
      val registry = openTestRegistry(
        isolatedSettings,
        closeFailure = None,
        onClose = () => inputCloseCount += 1
      )
      registry.updateOnBlock(
        ScanResults(Seq.empty, ArraySeq.empty, Seq.empty),
        modifierIdGen.sample.get,
        blockHeight = 11
      ).get
      val walletState = initialState(store, registry, None)
      val events = ArrayBuffer.empty[String]
      var openCount = 0
      val walletService = new ErgoWalletServiceImpl(isolatedSettings) {
        override protected[wallet] def moveRegistryToTombstone(
          registryFolder: File
        ): Try[Option[Path]] = {
          events += "move"
          super.moveRegistryToTombstone(registryFolder)
        }

        override protected[wallet] def openRegistry(settings: ErgoSettings): Try[WalletRegistry] = {
          openCount += 1
          events += "open"
          Try(openTestRegistry(
            settings,
            closeFailure = None,
            onClose = () => (),
            onFetch = () => events += "validate"
          ))
        }

        override protected[wallet] def deleteRegistryTombstone(path: Path): Try[Unit] = {
          events += "cleanup"
          super.deleteRegistryTombstone(path)
        }
      }

      walletService.recreateRegistryForUtxoSnapshotRecovery(walletState, isolatedSettings) match {
        case ErgoWalletService.RegistryResetReady(recoveredState, recoveredFrom) =>
          try {
            recoveredFrom shouldBe None
            (recoveredState.registry eq registry) shouldBe false
            recoveredState.registry.fetchDigest() shouldBe WalletDigest.empty
            (recoveredState.storage eq walletState.storage) shouldBe true
          } finally recoveredState.registry.close()
        case other =>
          fail(s"Expected ready registry reset, got $other")
      }

      inputCloseCount shouldBe 1
      openCount shouldBe 1
      events.take(4) shouldBe Seq("move", "open", "validate", "cleanup")
      retiredRegistryFolders(isolatedSettings) shouldBe empty
    }
  }

  property("recovery-specific registry reset should reject and close an unreadable fallback candidate") {
    withStore { store =>
      val isolatedSettings = this.isolatedSettings()
      val moveFailure = new IOException("injected registry quarantine move failure")
      val digestFailure = new IOException("injected fallback digest failure")
      val candidateCloseFailure = new IOException("injected fallback close failure")
      var inputCloseCount = 0
      val registry = openTestRegistry(
        isolatedSettings,
        closeFailure = None,
        onClose = () => inputCloseCount += 1
      )
      val walletState = initialState(store, registry, None)
      var openCount = 0
      var candidateCloseCount = 0
      val walletService = new ErgoWalletServiceImpl(isolatedSettings) {
        override protected[wallet] def moveRegistryToTombstone(
          registryFolder: File
        ): Try[Option[Path]] = Failure(moveFailure)

        override protected[wallet] def openRegistry(settings: ErgoSettings): Try[WalletRegistry] = {
          openCount += 1
          Try(openTestRegistry(
            settings,
            closeFailure = Some(candidateCloseFailure),
            onClose = () => candidateCloseCount += 1,
            fetchFailure = () => Some(digestFailure)
          ))
        }
      }

      walletService.recreateRegistryForUtxoSnapshotRecovery(walletState, isolatedSettings) match {
        case ErgoWalletService.RegistryResetUnavailable(cause) =>
          (cause eq moveFailure) shouldBe true
          cause.getSuppressed.exists(_ eq digestFailure) shouldBe true
          cause.getSuppressed.exists(_ eq candidateCloseFailure) shouldBe true
        case other =>
          fail(s"Expected unavailable registry reset, got $other")
      }

      inputCloseCount shouldBe 1
      openCount shouldBe 1
      candidateCloseCount shouldBe 1
    }
  }

  property("recreateRegistry should fail before opening a replacement when quarantine move fails") {
    withStore { store =>
      val isolatedSettings = this.isolatedSettings()
      val registry = WalletRegistry(isolatedSettings).get
      val walletState = initialState(store, registry, None)
      val moveFailure = new IOException("injected registry quarantine move failure")
      var replacementOpenAttempts = 0
      val walletService = new ErgoWalletServiceImpl(isolatedSettings) {
        override protected[wallet] def moveRegistryToTombstone(
          registryFolder: File
        ): Try[Option[Path]] = Failure(moveFailure)

        override protected[wallet] def openRegistry(settings: ErgoSettings): Try[WalletRegistry] = {
          replacementOpenAttempts += 1
          WalletRegistry(settings)
        }
      }

      val result = walletService.recreateRegistry(walletState, isolatedSettings)

      result.failed.get shouldBe moveFailure
      replacementOpenAttempts shouldBe 0
      Files.exists(WalletRegistry.registryFolder(isolatedSettings).toPath) shouldBe true
    }
  }

  property("recreateRegistry should remove its tombstone after opening the replacement") {
    withStore { store =>
      val isolatedSettings = this.isolatedSettings()
      val registry = WalletRegistry(isolatedSettings).get
      val walletState = initialState(store, registry, None)
      registry.updateOnBlock(
        ScanResults(Seq.empty, ArraySeq.empty, Seq.empty),
        modifierIdGen.sample.get,
        blockHeight = 1
      ).get
      var tombstone: Option[Path] = None
      val walletService = new ErgoWalletServiceImpl(isolatedSettings) {
        override protected[wallet] def moveRegistryToTombstone(
          registryFolder: File
        ): Try[Option[Path]] = {
          super.moveRegistryToTombstone(registryFolder).map { moved =>
            tombstone = moved
            moved
          }
        }
      }

      val recreated = walletService.recreateRegistry(walletState, isolatedSettings).get

      try {
        recreated.registry.fetchDigest() shouldBe WalletDigest.empty
        tombstone.isDefined shouldBe true
        Files.notExists(tombstone.get) shouldBe true
        retiredRegistryFolders(isolatedSettings) shouldBe empty
      } finally recreated.registry.close()
    }
  }

  property("recreateRegistry should preserve operator backups while removing UUID tombstones") {
    withStore { store =>
      val isolatedSettings = this.isolatedSettings()
      val registry = WalletRegistry(isolatedSettings).get
      val walletState = initialState(store, registry, None)
      val registryPath = WalletRegistry.registryFolder(isolatedSettings).toPath
      val operatorBackup = registryPath.resolveSibling("registry.retired-backup")
      val uuidTombstone = registryPath.resolveSibling(s"registry.retired-${UUID.randomUUID()}")
      Files.createDirectory(operatorBackup)
      Files.createDirectory(uuidTombstone)

      val recreated = new ErgoWalletServiceImpl(isolatedSettings)
        .recreateRegistry(walletState, isolatedSettings)
        .get

      try {
        Files.exists(operatorBackup) shouldBe true
        Files.notExists(uuidTombstone) shouldBe true
      } finally {
        recreated.registry.close()
        Files.deleteIfExists(operatorBackup)
        Files.deleteIfExists(uuidTombstone)
      }
    }
  }

  property("recreateRegistry should retry a transiently failed tombstone cleanup on the next reset") {
    withStore { store =>
      val isolatedSettings = this.isolatedSettings()
      val registry = WalletRegistry(isolatedSettings).get
      val walletState = initialState(store, registry, None)
      registry.updateOnBlock(
        ScanResults(Seq.empty, ArraySeq.empty, Seq.empty),
        modifierIdGen.sample.get,
        blockHeight = 1
      ).get
      var orphan: Option[Path] = None
      val failingCleanupService = new ErgoWalletServiceImpl(isolatedSettings) {
        override protected[wallet] def deleteRegistryTombstone(path: Path): Try[Unit] = {
          orphan = Some(path)
          Failure(new IOException("injected transient tombstone cleanup failure"))
        }
      }

      val firstReset = failingCleanupService.recreateRegistry(walletState, isolatedSettings).get
      var activeState = firstReset

      try {
        firstReset.registry.fetchDigest() shouldBe WalletDigest.empty
        orphan.isDefined shouldBe true
        Files.exists(orphan.get) shouldBe true

        activeState = new ErgoWalletServiceImpl(isolatedSettings)
          .recreateRegistry(firstReset, isolatedSettings)
          .get

        activeState.registry.fetchDigest() shouldBe WalletDigest.empty
        Files.notExists(orphan.get) shouldBe true
        retiredRegistryFolders(isolatedSettings) shouldBe empty
      } finally activeState.registry.close()
    }
  }

  property("recreateRegistry should retain the tombstone when replacement opening fails and clean it on retry") {
    withStore { store =>
      val isolatedSettings = this.isolatedSettings()
      val registry = WalletRegistry(isolatedSettings).get
      val walletState = initialState(store, registry, None)
      registry.updateOnBlock(
        ScanResults(Seq.empty, ArraySeq.empty, Seq.empty),
        modifierIdGen.sample.get,
        blockHeight = 1
      ).get
      val openFailure = new IOException("injected replacement open failure")
      val failingOpenService = new ErgoWalletServiceImpl(isolatedSettings) {
        override protected[wallet] def openRegistry(
          settings: ErgoSettings
        ): Try[WalletRegistry] = Failure(openFailure)
      }

      val failedReset = failingOpenService.recreateRegistry(walletState, isolatedSettings)

      failedReset.failed.get shouldBe openFailure
      Files.notExists(WalletRegistry.registryFolder(isolatedSettings).toPath) shouldBe true
      retiredRegistryFolders(isolatedSettings).size shouldBe 1

      val recovered = new ErgoWalletServiceImpl(isolatedSettings)
        .recreateRegistry(walletState, isolatedSettings)
        .get

      try {
        recovered.registry.fetchDigest() shouldBe WalletDigest.empty
        retiredRegistryFolders(isolatedSettings) shouldBe empty
      } finally recovered.registry.close()
    }
  }

  property("recreateRegistry should open a fresh registry without moving when the canonical folder is absent") {
    withVersionedStore(2) { versionedStore =>
      withStore { store =>
        val isolatedSettings = this.isolatedSettings()
        val walletState = initialState(store, versionedStore)
        var observedMove: Option[Option[Path]] = None
        val walletService = new ErgoWalletServiceImpl(isolatedSettings) {
          override protected[wallet] def moveRegistryToTombstone(
            registryFolder: File
          ): Try[Option[Path]] = {
            super.moveRegistryToTombstone(registryFolder).map { moved =>
              observedMove = Some(moved)
              moved
            }
          }
        }

        val recreated = walletService.recreateRegistry(walletState, isolatedSettings).get

        try {
          observedMove shouldBe Some(None)
          recreated.registry.fetchDigest() shouldBe WalletDigest.empty
          Files.exists(WalletRegistry.registryFolder(isolatedSettings).toPath) shouldBe true
          retiredRegistryFolders(isolatedSettings) shouldBe empty
        } finally recreated.registry.close()
      }
    }
  }

  property("recreateRegistry should not accumulate tombstones across two successful resets") {
    withStore { store =>
      val isolatedSettings = this.isolatedSettings()
      val registry = WalletRegistry(isolatedSettings).get
      val walletState = initialState(store, registry, None)
      val walletService = new ErgoWalletServiceImpl(isolatedSettings)

      val firstReset = walletService.recreateRegistry(walletState, isolatedSettings).get
      retiredRegistryFolders(isolatedSettings) shouldBe empty
      val secondReset = walletService.recreateRegistry(firstReset, isolatedSettings).get

      try {
        secondReset.registry.fetchDigest() shouldBe WalletDigest.empty
        retiredRegistryFolders(isolatedSettings) shouldBe empty
      } finally secondReset.registry.close()
    }
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

  property("it should scan UTXO snapshot chunks without creating wallet transactions") {
    forAll(modifierIdGen) { snapshotBlockId =>
      withVersionedStore(10) { versionedStore =>
        withStore { store =>
          val walletService = new ErgoWalletServiceImpl(settings)
          val wState = initialState(store, versionedStore)
          val boxes = boxesAvailable(makeGenesisBlock(pks.head.pubkey, randomNewAsset), pks.head.pubkey)
          val scanResults = WalletScanLogic.scanSnapshotBoxes(boxes, wState.walletVars, None)

          val updatedState = walletService.scanUtxoSnapshotChunk(
            wState,
            boxes,
            snapshotBlockId,
            snapshotHeight = 100,
            subtreeIndex = 0,
            finalChunk = true,
            dustLimit = None
          ).get

          updatedState.registry.walletUnspentBoxes().toList should contain theSameElementsAs scanResults.outputs
          updatedState.registry.allWalletTxs() shouldBe empty
          updatedState.registry.fetchDigest().height shouldBe 100
          updatedState.outputsFilter shouldBe empty
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
        val testSettings = isolatedSettings()
        val walletService = new ErgoWalletServiceImpl(testSettings)
        val pass = Random.nextString(10)
        val initializedState = walletService
          .initWallet(walletState, testSettings, SecretString.create(pass), Option.empty)
          .get
          ._2

        try {
          // Wallet unlocked after init, so we're locking it
          val initLockedWalletState = walletService.lockWallet(initializedState)
          initLockedWalletState.secretStorageOpt.get.isLocked shouldBe true
          initLockedWalletState.walletVars.proverOpt shouldBe empty

          val unlockedWalletState = walletService
            .unlockWallet(initLockedWalletState, SecretString.create(pass), usePreEip3Derivation = true)
            .get
          unlockedWalletState.secretStorageOpt.get.isLocked shouldBe false
          unlockedWalletState.storage.readAllKeys().size shouldBe 1
          unlockedWalletState.walletVars.proverOpt shouldNot be(empty)

          val lockedWalletState = walletService.lockWallet(unlockedWalletState)
          lockedWalletState.secretStorageOpt.get.isLocked shouldBe true
          lockedWalletState.walletVars.proverOpt shouldBe empty

          val finalUnlockedState = walletService
            .unlockWallet(lockedWalletState, SecretString.create(pass), usePreEip3Derivation = true)
            .get
          finalUnlockedState.secretStorageOpt.get.isLocked shouldBe false
          finalUnlockedState.storage.readAllKeys().size shouldBe 1
          finalUnlockedState.walletVars.proverOpt shouldNot be(empty)
        } finally {
          initializedState.registry.close()
          initializedState.storage.close()
        }
      }
    }
  }

  property("it should derive private key correctly") {
    withVersionedStore(2) { versionedStore =>
      withStore { store =>

        val pass = SecretString.create(Random.nextString(10))
        val mnemonic = "edge talent poet tortoise trumpet dose"

        val testSettings = isolatedSettings()
        val walletService = new ErgoWalletServiceImpl(testSettings)
        val ws1 = initialState(store, versionedStore)
        val ws2 = walletService
          .initWallet(ws1, testSettings, pass, Some(SecretString.create(mnemonic)))
          .get
          ._2

        try {
          ws2.secretStorageOpt.get.unlock(pass)
          val path = DerivationPath.fromEncoded("m/44/1/1/0/0").get
          val sk = ws2.secretStorageOpt.get.secret.get
          val pk = sk.derive(path).publicKey

          walletService.getPrivateKeyFromPath(ws2, pk.path).get.w shouldBe sk.derive(path).privateInput.w
        } finally {
          ws2.registry.close()
          ws2.storage.close()
        }
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
        val s = isolatedSettings().copy(
          nodeSettings = settings.nodeSettings.copy(blocksToKeep = -1)
        )
        val walletService = new ErgoWalletServiceImpl(s)
        val ws = walletService.initWallet(
          walletState,
          s,
          walletPass = wpass,
          None
        ).get._2

        try {
          ws.secretStorageOpt.get.unlock(wpass)
          ws.walletVars.trackedPubKeys.size shouldBe 1
          val uws = ws

          val uws2 = walletService.deriveNextKey(uws, usePreEip3Derivation = true).get._2
          uws2.walletVars.trackedPubKeys.size shouldBe 2

          val uws3 = walletService.deriveNextKey(uws2, usePreEip3Derivation = false).get._2
          uws3.walletVars.trackedPubKeys.size shouldBe 3
        } finally {
          ws.registry.close()
          ws.storage.close()
        }
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
        val s = isolatedSettings().copy(
          nodeSettings = settings.nodeSettings.copy(blocksToKeep = -1)
        )
        val walletService = new ErgoWalletServiceImpl(s)
        val ws = walletService.restoreWallet(
          walletState,
          s,
          mnemonic = SecretString.create("x"),
          mnemonicPassOpt = None,
          walletPass = wpass,
          usePre1627KeyDerivation = false
        ).get

        try {
          ws.secretStorageOpt.get.unlock(wpass)
          ws.walletVars.trackedPubKeys.size shouldBe 1
          val uws = ws

          val uws2 = walletService.deriveNextKey(uws, false).get._2
          uws2.walletVars.trackedPubKeys.size shouldBe 2

          val uws3 = walletService.deriveNextKey(uws2, false).get._2
          uws3.walletVars.trackedPubKeys.size shouldBe 3
        } finally {
          ws.registry.close()
          ws.storage.close()
        }
      }
    }
  }

}
