package org.ergoplatform.nodeView.wallet

import java.io.{File, IOException}
import java.nio.file.{Files, LinkOption, Path}
import java.nio.file.attribute.{BasicFileAttributes, PosixFileAttributeView}
import java.nio.charset.StandardCharsets.UTF_8
import java.util.concurrent.atomic.AtomicInteger

import akka.actor.{ActorSystem, Props}
import akka.testkit.TestProbe

import org.ergoplatform.{ErgoBox, P2PKAddress}
import org.ergoplatform.nodeView.state.ErgoStateContext
import org.ergoplatform.nodeView.wallet.persistence.{Balance, OffChainRegistry, WalletRegistry, WalletStorage}
import org.ergoplatform.nodeView.wallet.scanning.{EqualsScanningPredicate, ScanRequest, ScanWalletInteraction}
import org.ergoplatform.sdk.SecretString
import org.ergoplatform.sdk.wallet.secrets.ExtendedPublicKeySerializer
import org.ergoplatform.settings.{ErgoSettings, Parameters}
import org.ergoplatform.utils.ErgoCoreTestConstants.{defaultRootSecret, parameters}
import org.ergoplatform.utils.ErgoNodeTestConstants
import org.ergoplatform.wallet.secrets.JsonSecretStorage
import org.ergoplatform.wallet.mnemonic.Mnemonic
import org.ergoplatform.wallet.settings.SecretStorageSettings
import org.scalatest.matchers.should.Matchers
import org.scalatest.propspec.AnyPropSpec
import org.ergoplatform.wallet.Constants.PaymentsScanId
import sigma.ast.LongConstant

import scala.concurrent.Await
import scala.concurrent.duration._
import scala.util.{Failure, Success, Try}

class WalletInitializationSpec extends AnyPropSpec with Matchers {
  private def password: SecretString = SecretString.create("wallet-generation-test-password")

  private def withSettings(test: ErgoSettings => Unit): Unit = {
    val root = Files.createTempDirectory("wallet-initialization").toFile
    val original = ErgoNodeTestConstants.settings
    val settings = original.copy(directory = new File(root, "node").getPath,
      walletSettings = original.walletSettings.copy(testMnemonic = None,
        secretStorage = original.walletSettings.secretStorage.copy(secretDir = new File(root, "keystore").getPath)))
    try test(settings) finally {
      def remove(file: File): Unit = {
        Option(file.listFiles()).foreach(_.foreach(remove))
        if (!file.delete()) file.deleteOnExit()
      }
      remove(root)
    }
  }

  private def close(state: ErgoWalletState): Unit = {
    state.registry.close()
    state.storage.close()
  }

  private def createSecret(settings: SecretStorageSettings): JsonSecretStorage =
    JsonSecretStorage.init(Array.fill[Byte](32)(1), password, usePre1627KeyDerivation = false)(settings)

  private def populated(settings: ErgoSettings): ErgoWalletState = {
    val state = ErgoWalletState.initial(settings, parameters).get
    try {
      val publicKey = defaultRootSecret.publicKey
      state.storage.addPublicKey(publicKey).get
      state.storage.updateChangeAddress(P2PKAddress(publicKey.key)(settings.addressEncoder)).get
      val box = ErgoNodeTestConstants.genesisBoxes.head
      val scan = state.storage.addScan(ScanRequest("retained initialization fixture",
        EqualsScanningPredicate(ErgoBox.R0, LongConstant(box.value)), Some(ScanWalletInteraction.Shared), Some(true))).get
      state.registry.updateScans(Set(PaymentsScanId, scan.scanId), box).get
      val tracked = state.registry.getBox(box.id).get
      val context = ErgoStateContext.empty(settings.chainSettings,
        new Parameters(7, parameters.parametersTable, parameters.proposedUpdate))
      state.storage.updateStateContext(context).get
      val outputs = WalletCache.emptyFilter(settings.walletSettings.walletProfile.outputsFilterSize)
      outputs.put(box.id)
      state.copy(walletVars = WalletVars(state.storage, settings),
        offChainRegistry = OffChainRegistry(5, Seq(tracked.copy(inclusionHeightOpt = None)), Seq(Balance(tracked))),
        outputsFilter = Some(outputs), error = Some("previous initialization fixture status"), rescanInProgress = true)
    } catch {
      case error: Throwable =>
        close(state)
        throw error
    }
  }

  private def assertPopulated(state: ErgoWalletState): Unit = {
    val box = ErgoNodeTestConstants.genesisBoxes.head
    state.storage.readAllKeys().map(_.path) shouldBe Seq(defaultRootSecret.publicKey.path)
    state.storage.readAllKeys().map(key => ExtendedPublicKeySerializer.toBytes(key).toSeq) shouldBe
      Seq(ExtendedPublicKeySerializer.toBytes(defaultRootSecret.publicKey).toSeq)
    state.storage.readChangeAddress.isDefined shouldBe true
    state.storage.allScans.map(_.scanName) shouldBe Seq("retained initialization fixture")
    state.storage.allScans.map(_.trackingRule) shouldBe Seq(EqualsScanningPredicate(ErgoBox.R0, LongConstant(box.value)))
    state.storage.allScans.forall(scan => scan.removeOffchain && scan.walletInteraction == ScanWalletInteraction.Shared) shouldBe true
    state.registry.getBox(box.id).get.box.id.toSeq shouldBe box.id.toSeq
    state.registry.getBox(box.id).get.scans shouldBe Set(PaymentsScanId, state.storage.allScans.head.scanId)
    state.registry.walletUnspentBoxes().map(_.box.id.toSeq) shouldBe Seq(box.id.toSeq)
    state.registry.fetchDigest().walletBalance shouldBe box.value
    state.walletVars.trackedPubKeys.map(_.path) shouldBe Seq(defaultRootSecret.publicKey.path)
    state.walletVars.externalScans.map(_.scanName) shouldBe Seq("retained initialization fixture")
    state.walletVars.externalScans shouldBe state.storage.allScans
    state.walletVars.scriptsFilter.mightContain(state.walletVars.trackedBytes.head) shouldBe true
    state.offChainRegistry.height shouldBe 5
    state.offChainRegistry.offChainBoxes.map(_.box.id.toSeq) shouldBe Seq(box.id.toSeq)
    state.offChainRegistry.onChainBalances.map(_.value) shouldBe Seq(box.value)
    state.outputsFilter.get.mightContain(box.id) shouldBe true
    state.stateContext.currentParameters.height shouldBe 7
    state.error shouldBe Some("previous initialization fixture status")
    state.rescanInProgress shouldBe true
  }

  private class Calls(failureAt: Option[String] = None, activation: String = "success") extends WalletInitialization {
    val error = new IOException("initialization fixture operation failed")
    val closeError = new IOException("initialization fixture cleanup failed")
    var registriesClosed = Vector.empty[WalletRegistry]
    var storagesClosed = Vector.empty[WalletStorage]
    var failOldCleanup = false
    var failPreparedCleanup = false
    var old: Option[ErgoWalletState] = None
    var moveAttempts = 0

    override protected def openRegistry(settings: ErgoSettings, folder: File): WalletRegistry = {
      if (failureAt.contains("registry")) throw error
      super.openRegistry(settings, folder)
    }
    override protected def openStorage(settings: ErgoSettings, folder: File): WalletStorage = {
      if (failureAt.contains("storage")) throw error
      super.openStorage(settings, folder)
    }
    override protected def buildState(state: ErgoWalletState, settings: ErgoSettings,
                                      generation: WalletGeneration, registry: WalletRegistry,
                                      storage: WalletStorage, secret: JsonSecretStorage): ErgoWalletState = {
      if (failureAt.contains("state")) throw error
      super.buildState(state, settings, generation, registry, storage, secret)
    }
    override protected def closeRegistry(registry: WalletRegistry): Unit = {
      registriesClosed :+= registry
      super.closeRegistry(registry)
      if ((failOldCleanup && old.exists(_.registry eq registry)) ||
        (failPreparedCleanup && !old.exists(_.registry eq registry))) throw closeError
    }
    override protected def closeStorage(storage: WalletStorage): Unit = {
      storagesClosed :+= storage
      super.closeStorage(storage)
      if ((failOldCleanup && old.exists(_.storage eq storage)) ||
        (failPreparedCleanup && !old.exists(_.storage eq storage))) throw closeError
    }
    override protected def moveDescriptor(staging: Path, target: Path): Unit = {
      moveAttempts += 1
      activation match {
        case "before" | "unknown" => throw error
        case "after" =>
          super.moveDescriptor(staging, target)
          throw error
        case "unknown-after" =>
          super.moveDescriptor(staging, target)
          throw error
        case "conflict" =>
          val fields = new String(Files.readAllBytes(staging), UTF_8).split("\n", -1)
          fields(1) = java.util.UUID.randomUUID().toString
          Files.write(target, fields.mkString("\n").getBytes(UTF_8))
          throw error
        case _ => super.moveDescriptor(staging, target)
      }
    }
    override protected def readDescriptor(path: Path): Array[Byte] = {
      if (activation == "read-before") throw error
      if (moveAttempts > 0 && activation.startsWith("unknown")) throw new IOException("descriptor read unavailable")
      super.readDescriptor(path)
    }
  }

  property("initialization selects a complete generation under the configured roots and survives reopening") {
    withSettings { settings =>
      val old = populated(settings)
      assertPopulated(old)
      val oldContext = old.stateContext.bytes.toSeq
      val calls = new Calls
      val current = calls.initialize(old, settings, createSecret).get
      try {
        current.generation.isDefined shouldBe true
        current.secretStorageOpt.get.secretFile.toPath.startsWith(new File(settings.walletSettings.secretStorage.secretDir).toPath) shouldBe true
        calls.registriesClosed should contain(old.registry)
        calls.storagesClosed should contain(old.storage)
        WalletRegistry.registryFolder(settings).isDirectory shouldBe true
        WalletStorage.storageFolder(settings).isDirectory shouldBe true
        current.walletVars.trackedPubKeys shouldBe empty
        current.walletVars.externalScans shouldBe empty
        current.walletVars.stateCacheOpt shouldBe None
        current.walletVars.scriptsFilter.mightContain(old.walletVars.trackedBytes.head) shouldBe false
        current.storage.readAllKeys() shouldBe empty
        current.storage.allScans shouldBe empty
        current.storage.readChangeAddress shouldBe None
        current.registry.walletUnspentBoxes() shouldBe empty
        current.registry.fetchDigest().walletBalance shouldBe 0L
        current.offChainRegistry.offChainBoxes shouldBe empty
        current.offChainRegistry.onChainBalances shouldBe empty
        current.outputsFilter shouldBe None
        current.error shouldBe None
        current.rescanInProgress shouldBe false
        current.stateContext.bytes.toSeq shouldBe oldContext
        val retainedRegistry = WalletRegistry(settings).get
        val retainedStorage = WalletStorage.readOrCreate(settings)
        try assertPopulated(old.copy(registry = retainedRegistry, storage = retainedStorage))
        finally {
          retainedRegistry.close()
          retainedStorage.close()
        }
      } finally close(current)
      val reopened = ErgoWalletState.initial(settings, parameters).get
      try {
        reopened.generation shouldBe current.generation
        reopened.stateContext.bytes.toSeq shouldBe oldContext
        val loaded = new ErgoWalletServiceImpl(settings).readWallet(reopened, None, None, settings.walletSettings.secretStorage)
        loaded.secretStorageOpt.isDefined shouldBe true
        loaded.secretStorageOpt.get.unlock(password).get
        loaded.secretStorageOpt.get.lock()
      } finally close(reopened)
    }
  }

  for (inventory <- Seq("ambiguous", "mixed-legacy", "not-directory", "unrecognized")) {
    property(s"initialization rejects $inventory secret inventory before preparing any generation") {
      withSettings { settings =>
        val old = populated(settings)
        val root = new File(settings.walletSettings.secretStorage.secretDir).toPath
        val bytes = "retained wallet material".getBytes(UTF_8)
        val retained = if (inventory == "not-directory") {
          Files.createDirectories(root.getParent)
          Seq(Files.write(root, bytes))
        } else {
          Files.createDirectories(root)
          val names = inventory match {
            case "ambiguous" => Seq("first.json", "second.json")
            case "mixed-legacy" => Seq("legacy-wallet", "current.json")
            case _ => Seq("first", "second")
          }
          names.map(name => Files.write(root.resolve(name), bytes))
        }
        var openedRegistries = 0
        var openedStores = 0
        var createdSecrets = 0
        val calls = new WalletInitialization {
          override protected def openRegistry(s: ErgoSettings, folder: File): WalletRegistry = {
            openedRegistries += 1
            super.openRegistry(s, folder)
          }
          override protected def openStorage(s: ErgoSettings, folder: File): WalletStorage = {
            openedStores += 1
            super.openStorage(s, folder)
          }
        }
        try {
          val result = calls.initialize(old, settings, s => {
            createdSecrets += 1
            createSecret(s)
          })
          result shouldBe 'failure
          result.failed.get shouldBe a[IOException]
          openedRegistries shouldBe 0
          openedStores shouldBe 0
          createdSecrets shouldBe 0
          WalletInitialization.selected(settings) shouldBe None
          retained.foreach(path => Files.readAllBytes(path) shouldBe bytes)
          assertPopulated(old)
        } finally close(old)
      }
    }
  }

  property("initialization rejects a dangling secret directory link before preparing stores") {
    withSettings { settings =>
      val root = new File(settings.walletSettings.secretStorage.secretDir).toPath
      if (Files.getFileAttributeView(root.getParent, classOf[PosixFileAttributeView]) != null) {
        val old = populated(settings)
        val missing = root.getParent.resolve("missing-secret-directory")
        Files.createSymbolicLink(root, missing)
        var opened = false
        var created = false
        val calls = new WalletInitialization {
          override protected def openRegistry(s: ErgoSettings, folder: File): WalletRegistry = {
            opened = true
            super.openRegistry(s, folder)
          }
        }
        try {
          val result = calls.initialize(old, settings, s => { created = true; createSecret(s) })
          result shouldBe 'failure
          result.failed.get should not be a[JsonSecretStorage.SecretFileNotFoundException]
          opened shouldBe false
          created shouldBe false
          Files.isSymbolicLink(root) shouldBe true
          Files.exists(missing) shouldBe false
          WalletInitialization.selected(settings) shouldBe None
          assertPopulated(old)
        } finally close(old)
      }
    }
  }

  for (phase <- Seq("registry", "storage", "secret", "state")) {
    property(s"failure preparing $phase preserves the previous usable stores and keeps generation material inactive") {
      withSettings { settings =>
        val old = populated(settings)
        assertPopulated(old)
        val oldContext = old.stateContext.bytes.toSeq
        val calls = new Calls(Some(phase))
        try {
          val result = calls.initialize(old, settings, s => if (phase == "secret") throw calls.error else createSecret(s))
          result.failed.get shouldBe calls.error
          WalletInitialization.selected(settings) shouldBe None
          assertPopulated(old)
          old.stateContext.bytes.toSeq shouldBe oldContext
          calls.registriesClosed should not contain old.registry
          calls.storagesClosed should not contain old.storage
          JsonSecretStorage.readFile(settings.walletSettings.secretStorage).isFailure shouldBe true
          if (phase == "state") {
            val root = new File(settings.walletSettings.secretStorage.secretDir)
            root.listFiles().exists(_.getName.startsWith(".ergo-secret-staging-wallet-")) shouldBe true
          }
        } finally close(old)
      }
    }
  }

  for (outcome <- Seq("before", "after", "unknown", "unknown-after", "conflict")) {
    property(s"activation classifies its $outcome result without deleting recovery material") {
    withSettings { settings =>
      val old = ErgoWalletState.initial(settings, parameters).get
      val calls = new Calls(activation = outcome)
      val result = calls.initialize(old, settings, createSecret)
      outcome match {
        case "after" =>
          val current = result.get
          try WalletInitialization.selected(settings) shouldBe current.generation finally close(current)
        case "before" =>
          try {
            result.failed.get shouldBe calls.error
            WalletInitialization.selected(settings) shouldBe None
            old.registry.fetchDigest()
          } finally close(old)
        case _ =>
          try {
            result.failed.get shouldBe a[WalletInitialization.OutcomeUnknown]
            result.failed.get.getCause shouldBe calls.error
            old.registry.fetchDigest()
          } finally close(old)
      }
      new File(settings.walletSettings.secretStorage.secretDir).listFiles().nonEmpty shouldBe true
      if (outcome == "unknown-after") {
        val reconciled = ErgoWalletState.initial(settings, parameters).get
        try reconciled.generation.isDefined shouldBe true finally close(reconciled)
      }
    }
    }
  }

  property("old-handle cleanup errors do not change a committed initialization result") {
    withSettings { settings =>
      val old = ErgoWalletState.initial(settings, parameters).get
      val calls = new Calls
      calls.old = Some(old)
      calls.failOldCleanup = true
      val current = calls.initialize(old, settings, createSecret).get
      try {
        calls.registriesClosed should contain(old.registry)
        calls.storagesClosed should contain(old.storage)
        WalletInitialization.selected(settings) shouldBe current.generation
      } finally close(current)
    }
  }

  property("preparation failure preserves its original error when closing a candidate handle also fails") {
    withSettings { settings =>
      val old = ErgoWalletState.initial(settings, parameters).get
      val calls = new Calls(Some("storage"))
      calls.old = Some(old)
      calls.failPreparedCleanup = true
      try {
        calls.initialize(old, settings, createSecret).failed.get shouldBe calls.error
        calls.error.getSuppressed.toSeq should contain(calls.closeError)
        old.registry.fetchDigest()
        WalletInitialization.selected(settings) shouldBe None
      } finally close(old)
    }
  }

  for (resource <- Seq("main", "undo", "storage", "secret")) {
    property(s"startup refuses a missing selected $resource without creating replacement data") {
      withSettings { settings =>
        val old = ErgoWalletState.initial(settings, parameters).get
        val current = new Calls().initialize(old, settings, createSecret).get
        val generation = current.generation.get
        close(current)
        val required = resource match {
          case "main" => new File(generation.registryFolder(settings), "ldb_main/CURRENT")
          case "undo" => new File(generation.registryFolder(settings), "ldb_undo/CURRENT")
          case "storage" => new File(generation.storageFolder(settings), "CURRENT")
          case _ => generation.secret(settings.walletSettings.secretStorage)
        }
        val preserved = required.toPath.resolveSibling(required.getName + ".preserved")
        Files.move(required.toPath, preserved)
        val rejection = ErgoWalletState.initial(settings, parameters).failed.get
        rejection shouldBe a[IllegalArgumentException]
        rejection.getMessage shouldBe "requirement failed: Selected wallet generation is incomplete; " +
          "retain its files and restore the complete generation before restarting"
        required.exists() shouldBe false
        Files.exists(preserved) shouldBe true
      }
    }
  }

  private val descriptorRejections = Seq(
    "format" -> "Unsupported wallet generation descriptor",
    "generation" -> "Invalid wallet generation id",
    "secret traversal" -> "Invalid wallet generation secret filename",
    "secret extension" -> "Invalid wallet generation secret filename",
    "empty secret" -> "Invalid wallet generation secret filename",
    "extra field" -> "Unsupported wallet generation descriptor",
    "missing field" -> "Unsupported wallet generation descriptor",
    "missing terminator" -> "Unsupported wallet generation descriptor"
  )

  for ((field, expected) <- descriptorRejections) property(s"startup rejects an invalid descriptor $field without legacy fallback") {
    withSettings { settings =>
      val old = ErgoWalletState.initial(settings, parameters).get
      val current = new Calls().initialize(old, settings, createSecret).get
      close(current)
      val descriptor = WalletInitialization.descriptor(settings)
      val fields = new String(Files.readAllBytes(descriptor), UTF_8).split("\n", -1)
      val altered = field match {
        case "format" => fields.updated(0, "ergo-wallet-generation-v2")
        case "generation" => fields.updated(1, "../other-generation")
        case "secret traversal" => fields.updated(2, "../secret.json")
        case "secret extension" => fields.updated(2, fields(2).stripSuffix(".json") + ".txt")
        case "empty secret" => fields.updated(2, "")
        case "extra field" => fields :+ "extra"
        case "missing field" => fields.patch(2, Nil, 1)
        case _ => fields.dropRight(1)
      }
      Files.write(descriptor, altered.mkString("\n").getBytes(UTF_8))
      val rejection = ErgoWalletState.initial(settings, parameters).failed.get
      rejection shouldBe a[IllegalArgumentException]
      rejection.getMessage shouldBe s"requirement failed: $expected"
    }
  }

  property("a nonabsence descriptor read error prevents publication and preserves populated prior state") {
    withSettings { settings =>
      val old = populated(settings)
      val calls = new Calls(activation = "read-before")
      try {
        calls.initialize(old, settings, createSecret).failed.get shouldBe calls.error
        calls.moveAttempts shouldBe 0
        WalletInitialization.selected(settings) shouldBe None
        assertPopulated(old)
        calls.registriesClosed should not contain old.registry
        calls.storagesClosed should not contain old.storage
        JsonSecretStorage.readFile(settings.walletSettings.secretStorage).isFailure shouldBe true
      } finally close(old)
    }
  }

  property("legacy startup continues using the configured secret root when no descriptor exists") {
    withSettings { settings =>
      val secret = createSecret(settings.walletSettings.secretStorage)
      val state = ErgoWalletState.initial(settings, parameters).get
      try {
        state.generation shouldBe None
        val loaded = new ErgoWalletServiceImpl(settings).readWallet(state, None, None, settings.walletSettings.secretStorage)
        loaded.secretStorageOpt.get.secretFile shouldBe secret.secretFile
      } finally close(state)
    }
  }

  property("service initialization returns its generated mnemonic with the committed state") {
    withSettings { settings =>
      val old = ErgoWalletState.initial(settings, parameters).get
      val service = new ErgoWalletServiceImpl(settings)
      val (mnemonic, current) = service.initWallet(old, settings, password, None).get
      try {
        mnemonic.getData().nonEmpty shouldBe true
        current.generation shouldBe WalletInitialization.selected(settings)
        current.secretStorageOpt.isDefined shouldBe true
      } finally {
        mnemonic.erase()
        close(current)
      }
    }
  }

  property("service restoration and later recreation remain bound to their selected generation") {
    withSettings { configured =>
      val settings = configured.copy(nodeSettings = configured.nodeSettings.copy(blocksToKeep = -1))
      val old = ErgoWalletState.initial(settings, parameters).get
      val service = new ErgoWalletServiceImpl(settings)
      val entropy = Array.fill[Byte](settings.walletSettings.seedStrengthBits / 8)(2)
      val mnemonic = new Mnemonic(settings.walletSettings.mnemonicPhraseLanguage, settings.walletSettings.seedStrengthBits)
        .toMnemonic(entropy).get
      var current = old
      try {
        current = service.restoreWallet(old, settings, mnemonic, None, password, usePre1627KeyDerivation = false).get
        val selected = current.generation
        val prepared = service.recreateRegistry(current, settings).get
        prepared.generation should not be selected
        WalletInitialization.selected(settings) shouldBe selected
        current = service.publishRegistryRescan(current, prepared, settings).get
        current = service.recreateStorage(current, settings).get
        current.generation.get.id shouldBe selected.get.id
        current.generation.get.registryId.isDefined shouldBe true
        WalletInitialization.selected(settings) shouldBe current.generation
        WalletInitialization.validateReferences(current.generation.get, settings)
      } finally {
        mnemonic.erase()
        java.util.Arrays.fill(entropy, 0.toByte)
        close(current)
      }
      val reopened = ErgoWalletState.initial(settings, parameters).get
      try reopened.generation shouldBe current.generation finally close(reopened)
    }
  }

  property("service rescan refuses a changed descriptor before creating a candidate") {
    withSettings { settings =>
      val old = ErgoWalletState.initial(settings, parameters).get
      val active = new WalletInitialization().initialize(old, settings, createSecret).get
      val service = new ErgoWalletServiceImpl(settings)
      val generation = active.generation.get
      val folder = WalletInitialization.dataFolder(settings, generation.id)
      val descriptor = WalletInitialization.descriptor(settings)
      val previousBytes = Files.readAllBytes(descriptor)
      val other = generation.copy(id = java.util.UUID.randomUUID().toString)
      val otherBytes = s"ergo-wallet-generation-v1\n${other.id}\n${other.secretFile}\n".getBytes(UTF_8)
      def candidateCount: Int = Option(folder.listFiles()).toSeq.flatten.count(_.getName.startsWith("registry-"))
      try {
        WalletInitialization.matchesSelectedDescriptor(generation, settings).get shouldBe true
        val before = candidateCount
        Files.write(descriptor, otherBytes)
        WalletInitialization.matchesSelectedDescriptor(generation, settings).get shouldBe false
        val error = service.recreateRegistry(active, settings).failed.get
        error shouldBe a[WalletInitialization.OutcomeUnknown]
        error.getCause.getMessage shouldBe "Wallet selection changed before rescan"
        candidateCount shouldBe before
      } finally {
        Files.write(descriptor, previousBytes)
        close(active)
      }
    }
  }

  property("rescan does not serve a candidate when descriptor replacement reports success without selecting it") {
    withSettings { settings =>
      val old = ErgoWalletState.initial(settings, parameters).get
      val initialization = new WalletInitialization {
        override protected def replaceDescriptor(staging: Path, target: Path): Unit = ()
      }
      val active = initialization.initialize(old, settings, createSecret).get
      val previous = active.generation
      val candidate = initialization.prepareRescan(active, settings).get
      val candidateFolder = candidate.generation.get.registryFolder(settings).toPath
      try {
        initialization.publishRescan(active, candidate, settings).isFailure shouldBe true
        Files.exists(candidateFolder) shouldBe false
        WalletInitialization.selected(settings) shouldBe previous
        active.registry.fetchDigest()
      } finally close(active)
      val reopened = ErgoWalletState.initial(settings, parameters).get
      try reopened.generation shouldBe previous finally close(reopened)
    }
  }

  property("preflight descriptor read failure cleans a closed candidate while v1 remains selected") {
    withSettings { settings =>
      val old = ErgoWalletState.initial(settings, parameters).get
      val readError = new IOException("preflight descriptor read unavailable")
      var failPreflight = false
      val initialization = new WalletInitialization {
        override protected def readDescriptor(path: Path): Array[Byte] =
          if (failPreflight) throw readError else super.readDescriptor(path)
      }
      val active = initialization.initialize(old, settings, createSecret).get
      val candidate = initialization.prepareRescan(active, settings).get
      val folder = candidate.generation.get.registryFolder(settings).toPath
      failPreflight = true
      try {
        initialization.publishRescan(active, candidate, settings).failed.get shouldBe readError
        Files.exists(folder) shouldBe false
        WalletInitialization.selected(settings) shouldBe active.generation
      } finally close(active)
    }
  }

  property("preflight descriptor change to another generation has unknown outcome and retains the candidate") {
    withSettings { settings =>
      val old = ErgoWalletState.initial(settings, parameters).get
      val initialization = new WalletInitialization
      val active = initialization.initialize(old, settings, createSecret).get
      val candidate = initialization.prepareRescan(active, settings).get
      val folder = candidate.generation.get.registryFolder(settings).toPath
      val descriptor = WalletInitialization.descriptor(settings)
      val previousBytes = Files.readAllBytes(descriptor)
      val other = active.generation.get.copy(id = java.util.UUID.randomUUID().toString)
      val otherBytes = s"ergo-wallet-generation-v1\n${other.id}\n${other.secretFile}\n".getBytes(UTF_8)
      try {
        Files.write(descriptor, otherBytes)
        val error = initialization.publishRescan(active, candidate, settings).failed.get
        error shouldBe a[WalletInitialization.OutcomeUnknown]
        error.getCause.getMessage shouldBe "requirement failed: Wallet selection changed during rescan"
        Files.isDirectory(folder) shouldBe true
      } finally {
        Files.write(descriptor, previousBytes)
        close(active)
      }
    }
  }

  property("pre-move descriptor change has unknown outcome and retains the candidate") {
    withSettings { settings =>
      val old = ErgoWalletState.initial(settings, parameters).get
      var armed = false
      var reads = 0
      var otherBytes = Array.emptyByteArray
      val initialization = new WalletInitialization {
        override protected def readDescriptor(path: Path): Array[Byte] = {
          if (armed) {
            reads += 1
            if (reads == 2) Files.write(path, otherBytes)
          }
          super.readDescriptor(path)
        }
      }
      val active = initialization.initialize(old, settings, createSecret).get
      val candidate = initialization.prepareRescan(active, settings).get
      val folder = candidate.generation.get.registryFolder(settings).toPath
      val descriptor = WalletInitialization.descriptor(settings)
      val previousBytes = Files.readAllBytes(descriptor)
      val other = active.generation.get.copy(id = java.util.UUID.randomUUID().toString)
      otherBytes = s"ergo-wallet-generation-v1\n${other.id}\n${other.secretFile}\n".getBytes(UTF_8)
      armed = true
      try {
        val error = initialization.publishRescan(active, candidate, settings).failed.get
        reads shouldBe 2
        error shouldBe a[WalletInitialization.OutcomeUnknown]
        error.getCause.getMessage shouldBe "requirement failed: Wallet selection changed during rescan"
        Files.isDirectory(folder) shouldBe true
      } finally {
        Files.write(descriptor, previousBytes)
        close(active)
      }
    }
  }

  property("failed reopened registry close retains the candidate and original publication error") {
    withSettings { settings =>
      val old = ErgoWalletState.initial(settings, parameters).get
      val closeError = new IOException("reopened registry close failed")
      var candidateRegistry: Option[WalletRegistry] = None
      val initialization = new WalletInitialization {
        override protected def replaceDescriptor(staging: Path, target: Path): Unit = ()
        override protected def closeRegistry(registry: WalletRegistry): Unit = {
          super.closeRegistry(registry)
          if (candidateRegistry.exists(_ ne registry)) throw closeError
        }
      }
      val active = initialization.initialize(old, settings, createSecret).get
      val candidate = initialization.prepareRescan(active, settings).get
      candidateRegistry = Some(candidate.registry)
      val folder = candidate.generation.get.registryFolder(settings).toPath
      try {
        val error = initialization.publishRescan(active, candidate, settings).failed.get
        error.getMessage shouldBe "Wallet rescan selection was not published"
        error.getSuppressed.toSeq should contain(closeError)
        Files.isDirectory(folder) shouldBe true
        WalletInitialization.selected(settings) shouldBe active.generation
      } finally close(active)
    }
  }

  property("unknown rescan selection retains its candidate when descriptor readback fails") {
    withSettings { settings =>
      val old = ErgoWalletState.initial(settings, parameters).get
      val moveError = new IOException("descriptor replacement result unavailable")
      val readError = new IOException("descriptor readback unavailable")
      var moveAttempted = false
      val initialization = new WalletInitialization {
        override protected def replaceDescriptor(staging: Path, target: Path): Unit = {
          moveAttempted = true
          throw moveError
        }
        override protected def readDescriptor(path: Path): Array[Byte] =
          if (moveAttempted) throw readError else super.readDescriptor(path)
      }
      val active = initialization.initialize(old, settings, createSecret).get
      val candidate = initialization.prepareRescan(active, settings).get
      val folder = candidate.generation.get.registryFolder(settings).toPath
      try {
        val error = initialization.publishRescan(active, candidate, settings).failed.get
        error shouldBe a[WalletInitialization.OutcomeUnknown]
        error.getCause shouldBe moveError
        error.getSuppressed.toSeq should contain(readError)
        Files.isDirectory(folder) shouldBe true
        WalletInitialization.selected(settings) shouldBe active.generation
      } finally close(active)
    }
  }

  property("a closed unselected rescan candidate is removed without affecting the selected registry") {
    withSettings { settings =>
      val old = ErgoWalletState.initial(settings, parameters).get
      val initialization = new WalletInitialization
      val active = initialization.initialize(old, settings, createSecret).get
      val candidate = initialization.prepareRescan(active, settings).get
      val generation = candidate.generation.get
      val folder = generation.registryFolder(settings).toPath
      candidate.registry.close()
      try {
        Files.isDirectory(folder) shouldBe true
        WalletInitialization.discardUnselectedRescanCandidate(generation, settings).get
        Files.exists(folder) shouldBe false
        WalletInitialization.selected(settings) shouldBe active.generation
        active.registry.fetchDigest()
      } finally close(active)
    }
  }

  property("rescan candidate cleanup retains data when selection cannot be read or identity is invalid") {
    withSettings { settings =>
      val old = ErgoWalletState.initial(settings, parameters).get
      val initialization = new WalletInitialization
      val active = initialization.initialize(old, settings, createSecret).get
      val candidate = initialization.prepareRescan(active, settings).get
      val generation = candidate.generation.get
      val folder = generation.registryFolder(settings).toPath
      candidate.registry.close()
      val descriptor = WalletInitialization.descriptor(settings)
      val previousBytes = Files.readAllBytes(descriptor)
      try {
        Files.delete(descriptor)
        WalletInitialization.discardUnselectedRescanCandidate(generation, settings).isFailure shouldBe true
        Files.isDirectory(folder) shouldBe true
        Files.write(descriptor, previousBytes)
        WalletInitialization.discardUnselectedRescanCandidate(
          generation.copy(registryId = Some("../outside")), settings).isFailure shouldBe true
        Files.isDirectory(folder) shouldBe true
      } finally {
        if (!Files.exists(descriptor)) Files.write(descriptor, previousBytes)
        close(active)
      }
    }
  }

  property("rescan candidate cleanup rejects a symbolic link inside its registry") {
    withSettings { settings =>
      val old = ErgoWalletState.initial(settings, parameters).get
      val initialization = new WalletInitialization
      val active = initialization.initialize(old, settings, createSecret).get
      val candidate = initialization.prepareRescan(active, settings).get
      val generation = candidate.generation.get
      val folder = generation.registryFolder(settings).toPath
      candidate.registry.close()
      val outside = Files.createTempFile("wallet-rescan-outside", ".txt")
      try {
        val link = folder.resolve("outside-link")
        val created = Try(Files.createSymbolicLink(link, outside))
        assume(created.isSuccess, "Symbolic links are unavailable in this test environment")
        WalletInitialization.discardUnselectedRescanCandidate(generation, settings).isFailure shouldBe true
        Files.isDirectory(folder) shouldBe true
        Files.exists(outside) shouldBe true
      } finally {
        Files.deleteIfExists(outside)
        close(active)
      }
    }
  }

  property("rescan candidate cleanup rejects a Windows junction without visiting its target") {
    withSettings { settings =>
      val old = ErgoWalletState.initial(settings, parameters).get
      val initialization = new WalletInitialization
      val active = initialization.initialize(old, settings, createSecret).get
      val candidate = initialization.prepareRescan(active, settings).get
      val generation = candidate.generation.get
      val folder = generation.registryFolder(settings).toPath
      candidate.registry.close()
      val outside = Files.createDirectories(new File(settings.directory, "outside-junction-target").toPath)
      val sentinel = Files.write(outside.resolve("sentinel.txt"), Array[Byte](1))
      val junction = folder.resolve("outside-junction")
      try {
        assume(System.getProperty("os.name").startsWith("Windows"), "Junction test requires Windows")
        val command = "mklink /J \"" + junction + "\" \"" + outside + "\""
        val process = new ProcessBuilder("cmd", "/c", command).start()
        assume(process.waitFor() == 0, "Could not create a disposable NTFS junction")
        val attrs = Files.readAttributes(junction, classOf[BasicFileAttributes], LinkOption.NOFOLLOW_LINKS)
        attrs.isOther shouldBe true
        WalletInitialization.discardUnselectedRescanCandidate(generation, settings).isFailure shouldBe true
        Files.isRegularFile(sentinel) shouldBe true
        Files.isDirectory(folder) shouldBe true
      } finally {
        Files.deleteIfExists(junction)
        close(active)
      }
    }
  }

  property("a failure after rescan selection has an unknown outcome and cold reopens the candidate") {
    withSettings { settings =>
      val old = ErgoWalletState.initial(settings, parameters).get
      val fault = new IOException("post-selection fault")
      val initialization = new WalletInitialization {
        override protected def afterRescanSelection(): Unit = throw fault
      }
      val active = initialization.initialize(old, settings, createSecret).get
      val previous = active.generation.get
      val candidate = initialization.prepareRescan(active, settings).get
      val candidateFolder = candidate.generation.get.registryFolder(settings).toPath
      candidate.registry.updateScans(Set(PaymentsScanId), ErgoNodeTestConstants.genesisBoxes.head).get
      val outcome = initialization.publishRescan(active, candidate, settings).failed.get
      outcome shouldBe a[WalletInitialization.OutcomeUnknown]
      outcome.getCause shouldBe fault
      WalletInitialization.selected(settings) shouldBe candidate.generation
      Files.isDirectory(candidateFolder) shouldBe true
      WalletInitialization.discardUnselectedRescanCandidate(candidate.generation.get, settings).isFailure shouldBe true
      Files.isDirectory(candidateFolder) shouldBe true
      candidate.generation should not be Some(previous)
      close(active)
      val reopened = ErgoWalletState.initial(settings, parameters).get
      try {
        reopened.generation shouldBe candidate.generation
        reopened.registry.getBox(ErgoNodeTestConstants.genesisBoxes.head.id).isDefined shouldBe true
      } finally close(reopened)
    }
  }

  property("successful actor initialization returns its backup, installs state, and supports later unlock and key reads") {
    withSettings { settings =>
      import ErgoWalletActorMessages._
      val system = ActorSystem("wallet-initialization-success")
      val probe = TestProbe()(system)
      val service = new ErgoWalletServiceImpl(settings)
      val actor = system.actorOf(Props(new ErgoWalletActor(settings, parameters, service, null, null)))
      probe.watch(actor)
      var backup: Option[SecretString] = None
      try {
        probe.send(actor, InitWallet(password, None))
        val mnemonic = probe.expectMsgType[Success[SecretString]](10.seconds).value
        backup = Some(mnemonic)
        mnemonic.getData().nonEmpty shouldBe true
        WalletInitialization.selected(settings).isDefined shouldBe true
        probe.awaitAssert({
          probe.send(actor, GetWalletStatus)
          val status = probe.expectMsgType[WalletStatus](5.seconds)
          status.initialized shouldBe true
          status.unlocked shouldBe true
          status.error shouldBe None
        }, 5.seconds, 100.millis)
        probe.send(actor, LockWallet)
        probe.send(actor, GetWalletStatus)
        probe.expectMsgType[WalletStatus](5.seconds).unlocked shouldBe false
        probe.send(actor, UnlockWallet(password))
        probe.expectMsgType[Success[Unit]](5.seconds).value shouldBe (())
        probe.send(actor, CheckSeed(mnemonic, None))
        probe.expectMsg(true)
        probe.send(actor, ReadPublicKeys(0, 100))
        probe.expectMsgType[Seq[P2PKAddress]](5.seconds).nonEmpty shouldBe true
      } finally {
        backup.foreach(_.erase())
        probe.send(actor, CloseWallet)
        probe.expectTerminated(actor, 5.seconds)
        Await.result(system.terminate(), 10.seconds)
      }
    }
  }

  property("an initialized generation rejects replacement without recommending deletion of only its secret file") {
    withSettings { settings =>
      import ErgoWalletActorMessages._
      val initial = ErgoWalletState.initial(settings, parameters).get
      val current = new Calls().initialize(initial, settings, createSecret).get
      close(current)
      val system = ActorSystem("wallet-generation-replacement")
      val probe = TestProbe()(system)
      val service = new ErgoWalletServiceImpl(settings)
      val actor = system.actorOf(Props(new ErgoWalletActor(settings, parameters, service, null, null)))
      probe.watch(actor)
      try {
        probe.send(actor, InitWallet(password, None))
        val error = probe.expectMsgType[Failure[_]](5.seconds).exception
        error.getMessage shouldBe "Wallet is already initialized; use a separate wallet data directory to initialize another wallet."
        WalletInitialization.selected(settings) shouldBe current.generation
      } finally {
        probe.send(actor, CloseWallet)
        probe.expectTerminated(actor, 5.seconds)
        Await.result(system.terminate(), 10.seconds)
      }
    }
  }

  property("an unknown activation result is exposed in status and blocks further initialization, unlock and rescan") {
    withSettings { settings =>
      import ErgoWalletActorMessages._
      val system = ActorSystem("wallet-initialization-guard")
      val probe = TestProbe()(system)
      val error = new WalletInitialization.OutcomeUnknown(new IOException("activation result unavailable"))
      val initCalls = new AtomicInteger()
      val otherCalls = new AtomicInteger()
      val service = new ErgoWalletServiceImpl(settings) {
        override def initWallet(state: ErgoWalletState, settings: ErgoSettings, walletPass: SecretString,
                                mnemonicPassOpt: Option[SecretString]): Try[(SecretString, ErgoWalletState)] = {
          initCalls.incrementAndGet()
          Failure(error)
        }
        override def restoreWallet(state: ErgoWalletState, settings: ErgoSettings, mnemonic: SecretString,
                                   mnemonicPassOpt: Option[SecretString], walletPass: SecretString,
                                   usePre1627KeyDerivation: Boolean): Try[ErgoWalletState] = {
          otherCalls.incrementAndGet()
          Failure(error)
        }
        override def unlockWallet(state: ErgoWalletState, walletPass: SecretString,
                                  usePreEip3Derivation: Boolean): Try[ErgoWalletState] = {
          otherCalls.incrementAndGet()
          Failure(error)
        }
        override def recreateRegistry(state: ErgoWalletState, settings: ErgoSettings): Try[ErgoWalletState] = {
          otherCalls.incrementAndGet()
          Failure(error)
        }
      }
      val actor = system.actorOf(Props(new ErgoWalletActor(settings, parameters, service, null, null)))
      probe.watch(actor)
      try {
        probe.send(actor, InitWallet(password, None))
        probe.expectMsgType[Failure[_]](5.seconds).exception shouldBe error
        probe.send(actor, GetWalletStatus)
        probe.expectMsgType[WalletStatus](5.seconds).error shouldBe Some(error.getMessage)
        Seq(InitWallet(password, None), RestoreWallet(SecretString.create("unused"), None, password, false),
          UnlockWallet(password), RescanWallet(0)).foreach { message =>
          probe.send(actor, message)
          probe.expectMsgType[Failure[_]](5.seconds).exception shouldBe error
        }
        initCalls.get() shouldBe 1
        otherCalls.get() shouldBe 0
      } finally {
        probe.send(actor, CloseWallet)
        probe.expectTerminated(actor, 5.seconds)
        Await.result(system.terminate(), 10.seconds)
      }
    }
  }
}
