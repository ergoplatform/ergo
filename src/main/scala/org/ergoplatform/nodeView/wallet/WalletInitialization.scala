package org.ergoplatform.nodeView.wallet

import java.io.{File, FileOutputStream, IOException}
import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.attribute.BasicFileAttributes
import java.nio.file.{FileVisitResult, Files, LinkOption, NoSuchFileException, Path, SimpleFileVisitor, StandardCopyOption}
import java.util.UUID

import org.ergoplatform.nodeView.wallet.persistence.{OffChainRegistry, WalletRegistry, WalletStorage}
import org.ergoplatform.settings.{ErgoSettings, Parameters}
import org.ergoplatform.wallet.secrets.JsonSecretStorage
import org.ergoplatform.wallet.settings.SecretStorageSettings
import scorex.util.ScorexLogging

import scala.util.control.NonFatal
import scala.util.{Failure, Success, Try}

/** Identifies a prepared wallet without persisting machine-specific directory paths. */
final case class WalletGeneration(id: String, secretFile: String, registryId: Option[String] = None) {
  def registryFolder(settings: ErgoSettings): File =
    new File(WalletInitialization.dataFolder(settings, id), registryId.fold("registry")(r => s"registry-$r"))
  def storageFolder(settings: ErgoSettings): File = new File(WalletInitialization.dataFolder(settings, id), "storage")
  def secret(settings: SecretStorageSettings): File = new File(WalletInitialization.secretFolder(settings, id), secretFile)
}

/**
  * Prepares a complete generation before publishing its selection. Prior stores and encrypted
  * recovery material are retained; closing old handles is post-commit maintenance.
  *
  * This is a method-level initialization boundary, not an HTTP-delivery or power-loss guarantee.
  * A failed atomic-move result is reconciled by exact read-back or reported as explicitly unknown.
  * After adoption, older binaries must not write this wallet. Generation-aware backups include
  * the selection descriptor, selected data generation and selected configured-keystore generation.
  */
private[wallet] class WalletInitialization extends ScorexLogging {
  import WalletInitialization._

  protected def openRegistry(settings: ErgoSettings, folder: File): WalletRegistry = WalletRegistry.openAt(settings, folder).get
  protected def openStorage(settings: ErgoSettings, folder: File): WalletStorage = WalletStorage.openAt(settings, folder)
  protected def closeRegistry(registry: WalletRegistry): Unit = registry.close()
  protected def closeStorage(storage: WalletStorage): Unit = storage.close()
  protected def readDescriptor(path: Path): Array[Byte] = Files.readAllBytes(path)
  protected def moveDescriptor(staging: Path, target: Path): Unit = {
    Files.move(staging, target, StandardCopyOption.ATOMIC_MOVE)
    ()
  }
  protected def replaceDescriptor(staging: Path, target: Path): Unit = {
    Files.move(staging, target, StandardCopyOption.ATOMIC_MOVE, StandardCopyOption.REPLACE_EXISTING)
    ()
  }
  protected def afterRescanSelection(): Unit = ()

  protected def buildState(state: ErgoWalletState, settings: ErgoSettings,
                           generation: WalletGeneration, registry: WalletRegistry,
                           storage: WalletStorage, secret: JsonSecretStorage): ErgoWalletState = {
    storage.updateStateContext(state.stateContext).get
    state.copy(storage = storage, registry = registry, secretStorageOpt = Some(secret),
      offChainRegistry = OffChainRegistry.init(registry), outputsFilter = None,
      walletVars = WalletVars(storage, settings), error = None, rescanInProgress = false,
      generation = Some(generation))
  }

  def initialize(state: ErgoWalletState, settings: ErgoSettings,
                  createSecret: SecretStorageSettings => JsonSecretStorage): Try[ErgoWalletState] = Try {
    require(state.secretStorageOpt.isEmpty && selected(settings).isEmpty, "Wallet is already initialized")
    JsonSecretStorage.readFile(settings.walletSettings.secretStorage) match {
      case Failure(_: JsonSecretStorage.SecretFileNotFoundException) => ()
      case Failure(error) => throw error
      case Success(_) => throw new IllegalStateException("Wallet secret already exists")
    }
    val id = UUID.randomUUID().toString
    val folder = dataFolder(settings, id)
    var registry: Option[WalletRegistry] = None
    var storage: Option[WalletStorage] = None
    try {
      val preparedRegistry = openRegistry(settings, new File(folder, "registry"))
      registry = Some(preparedRegistry)
      val preparedStorage = openStorage(settings, new File(folder, "storage"))
      storage = Some(preparedStorage)
      val secretSettings = settings.walletSettings.secretStorage.copy(
        secretDir = secretFolder(settings.walletSettings.secretStorage, id).getPath)
      val preparedSecret = createSecret(secretSettings)
      val generation = WalletGeneration(id, preparedSecret.secretFile.getName)
      validateReferences(generation, settings)
      val candidate = buildState(state, settings, generation, preparedRegistry, preparedStorage, preparedSecret)
      publish(generation, settings)
      // Publication is complete. Cleanup cannot change the result or remove recovery material.
      Seq[() => Unit](() => closeRegistry(state.registry), () => closeStorage(state.storage)).foreach { close =>
        Try(close()).failed.foreach(error => log.warn("Wallet initialized; prior store handle could not close", error))
      }
      candidate
    } catch {
      case t: Throwable =>
        closeWithFailure(storage.map(s => () => closeStorage(s)).toSeq ++ registry.map(r => () => closeRegistry(r)).toSeq, t)
        throw t
    }
  }

  private def publish(generation: WalletGeneration, settings: ErgoSettings): Unit = {
    val target = descriptor(settings)
    // Read errors are never interpreted as absence.
    Try(readDescriptor(target)) match {
      case Failure(_: NoSuchFileException) =>
      case Failure(error) => throw error
      case Success(_) => throw new IllegalStateException("Wallet generation is already selected")
    }
    Files.createDirectories(target.getParent)
    val staging = Files.createTempFile(target.getParent, ".wallet-generation-", ".tmp")
    val bytes = encode(generation)
    val writer = new FileOutputStream(staging.toFile)
    var writeFailure: Throwable = null
    try {
      writer.write(bytes)
      writer.getFD.sync()
    } catch {
      case t: Throwable =>
        writeFailure = t
        throw t
    } finally {
      try writer.close() catch {
        case NonFatal(error) if writeFailure != null =>
          if (error ne writeFailure) writeFailure.addSuppressed(error)
      }
    }
    try moveDescriptor(staging, target) catch {
      case NonFatal(moveError) =>
        Try(readDescriptor(target)) match {
          case Success(observed) if observed.sameElements(bytes) =>
            log.warn("Wallet generation publication completed despite a reported move error", moveError)
          case Failure(_: NoSuchFileException) => throw moveError
          case outcome =>
            val unknown = new OutcomeUnknown(moveError)
            outcome.failed.foreach { readError => if (readError ne moveError) unknown.addSuppressed(readError) }
            throw unknown
        }
    }
  }

  /** Prepare an independent registry. The selected registry and descriptor remain untouched. */
  def prepareRescan(state: ErgoWalletState, settings: ErgoSettings): Try[ErgoWalletState] = Try {
    val selectedGeneration = state.generation.getOrElse(
      throw new IllegalStateException("Staged rescan requires a selected wallet generation"))
    matchesSelectedDescriptor(selectedGeneration, settings) match {
      case Success(true) => ()
      case Success(false) =>
        throw new OutcomeUnknown(new IllegalStateException("Wallet selection changed before rescan"))
      case Failure(error) => throw new OutcomeUnknown(error)
    }
    val candidateGeneration = selectedGeneration.copy(registryId = Some(UUID.randomUUID().toString))
    val folder = candidateGeneration.registryFolder(settings)
    require(!Files.exists(folder.toPath), "Rescan registry candidate already exists")
    val registry = openRegistry(settings, folder)
    state.copy(registry = registry, offChainRegistry = OffChainRegistry.init(registry),
      outputsFilter = None, rescanInProgress = true, generation = Some(candidateGeneration))
  }

  private def reconcileFailedRescan(failure: Throwable, target: Path, previousBytes: Array[Byte],
                                    replacement: WalletGeneration, settings: ErgoSettings,
                                    candidateClosed: Boolean, reopenedClosed: Boolean,
                                    published: Boolean): Throwable = {
    // Returning to the active wallet is safe only while its exact descriptor remains selected.
    val observed = Try(Files.readAllBytes(target))
    val priorSelected = observed.toOption.exists(_.sameElements(previousBytes))
    val reported = if (published || !priorSelected || failure.isInstanceOf[OutcomeUnknown]) {
      failure match {
        case unknown: OutcomeUnknown => unknown
        case _ => new OutcomeUnknown(failure)
      }
    } else failure
    observed.failed.foreach { readError =>
      if (readError ne reported) reported.addSuppressed(readError)
    }
    if (!reported.isInstanceOf[OutcomeUnknown] && candidateClosed && reopenedClosed) {
      discardUnselectedRescanCandidate(replacement, settings).failed.foreach { cleanupError =>
        if (cleanupError ne reported) reported.addSuppressed(cleanupError)
      }
    }
    reported
  }

  /** Reopen the complete candidate before atomically changing the selected descriptor. */
  def publishRescan(active: ErgoWalletState, candidate: ErgoWalletState,
                    settings: ErgoSettings): Try[ErgoWalletState] = Try {
    val previous = active.generation.getOrElse(
      throw new IllegalStateException("Staged rescan requires a selected wallet generation"))
    val replacement = candidate.generation.getOrElse(
      throw new IllegalStateException("Rescan candidate has no generation"))
    require(replacement.id == previous.id && replacement.secretFile == previous.secretFile &&
      replacement.registryId.isDefined && replacement != previous, "Invalid rescan registry candidate")
    val target = descriptor(settings)
    val previousBytes = encode(previous)
    val candidateHeight = try {
      require(readDescriptor(target).sameElements(previousBytes), "Wallet selection changed during rescan")
      candidate.getWalletHeight
    } catch {
      case t: Throwable =>
        val candidateClose = Try(closeRegistry(candidate.registry))
        candidateClose.failed.foreach { closeError =>
          if (closeError ne t) t.addSuppressed(closeError)
        }
        throw reconcileFailedRescan(t, target, previousBytes, replacement, settings,
          candidateClose.isSuccess, reopenedClosed = true, published = false)
    }
    try closeRegistry(candidate.registry) catch {
      case t: Throwable =>
        throw reconcileFailedRescan(t, target, previousBytes, replacement, settings,
          candidateClosed = false, reopenedClosed = true, published = false)
    }
    val reopened = try openRegistry(settings, replacement.registryFolder(settings)) catch {
      case t: Throwable =>
        throw reconcileFailedRescan(t, target, previousBytes, replacement, settings,
          candidateClosed = true, reopenedClosed = false, published = false)
    }
    var published = false
    try {
      validateReferences(replacement, settings)
      require(reopened.fetchDigest().height == candidateHeight, "Rescan registry changed before publication")
      val bytes = encode(replacement)
      val staging = Files.createTempFile(target.getParent, ".wallet-rescan-", ".tmp")
      try {
        val writer = new FileOutputStream(staging.toFile)
        var writeFailure: Throwable = null
        try {
          writer.write(bytes)
          writer.getFD.sync()
        } catch {
          case t: Throwable =>
            writeFailure = t
            throw t
        } finally {
          try writer.close() catch {
            case NonFatal(error) if writeFailure != null =>
              if (error ne writeFailure) writeFailure.addSuppressed(error)
          }
        }
        require(readDescriptor(target).sameElements(previousBytes), "Wallet selection changed during rescan")
        try replaceDescriptor(staging, target) catch {
          case NonFatal(moveError) =>
            Try(readDescriptor(target)) match {
              case Success(observed) if observed.sameElements(bytes) =>
                published = true
                log.warn("Wallet rescan selection completed despite a reported move error", moveError)
              case Success(observed) if observed.sameElements(previousBytes) => throw moveError
              case outcome =>
                val unknown = new OutcomeUnknown(moveError)
                outcome.failed.foreach { readError =>
                  if (readError ne moveError) unknown.addSuppressed(readError)
                }
                throw unknown
            }
        }
        Try(readDescriptor(target)) match {
          case Success(observed) if observed.sameElements(bytes) => published = true
          case Success(observed) if observed.sameElements(previousBytes) =>
            throw new IllegalStateException("Wallet rescan selection was not published")
          case outcome =>
            val unknown = new OutcomeUnknown(new IOException("Wallet rescan selection could not be confirmed"))
            outcome.failed.foreach(unknown.addSuppressed)
            throw unknown
        }
      } finally {
        Try(Files.deleteIfExists(staging)).failed.foreach { error =>
          Try(log.warn("Could not remove temporary rescan descriptor", error))
        }
      }
      afterRescanSelection()
      Try(closeRegistry(active.registry)).failed.foreach { error =>
        log.warn("Wallet rescan selected; prior registry handle could not close", error)
      }
      candidate.copy(registry = reopened, rescanInProgress = false, error = None)
    } catch {
      case t: Throwable =>
        val reopenedClose = Try(closeRegistry(reopened))
        reopenedClose.failed.foreach { closeError =>
          if (closeError ne t) t.addSuppressed(closeError)
        }
        throw reconcileFailedRescan(t, target, previousBytes, replacement, settings,
          candidateClosed = true, reopenedClosed = reopenedClose.isSuccess, published = published)
    }
  }
}

private[wallet] object WalletInitialization {
  private val Format = "ergo-wallet-generation-v1"
  private val RescanFormat = "ergo-wallet-generation-v2"
  // #2507 excludes this prefix from legacy secret discovery, including inactive directories.
  private val SecretPrefix = ".ergo-secret-staging-wallet-"

  final class OutcomeUnknown(cause: Throwable) extends IOException(
    "Wallet initialization outcome is uncertain. Restart the node before retrying; wallet recovery material has been retained.", cause)

  def descriptor(settings: ErgoSettings): Path = new File(s"${settings.directory}/wallet/active-generation").toPath
  def dataFolder(settings: ErgoSettings, id: String): File = new File(s"${settings.directory}/wallet/generations/$id")
  def secretFolder(settings: SecretStorageSettings, id: String): File = new File(settings.secretDir, SecretPrefix + id)

  private def canonicalId(id: String): Boolean = Try(UUID.fromString(id).toString == id).getOrElse(false)

  private def encode(generation: WalletGeneration): Array[Byte] = {
    require(canonicalId(generation.id), "Invalid wallet generation id")
    require(generation.secretFile.endsWith(".json") && canonicalId(generation.secretFile.stripSuffix(".json")),
      "Invalid wallet generation secret filename")
    generation.registryId match {
      case None => s"$Format\n${generation.id}\n${generation.secretFile}\n".getBytes(UTF_8)
      case Some(registryId) =>
        require(canonicalId(registryId), "Invalid wallet registry id")
        s"$RescanFormat\n${generation.id}\n${generation.secretFile}\n$registryId\n".getBytes(UTF_8)
    }
  }

  private def decode(bytes: Array[Byte]): WalletGeneration = {
    val fields = new String(bytes, UTF_8).split("\n", -1)
    val v1 = fields.length == 4 && fields(0) == Format && fields(3).isEmpty
    val v2 = fields.length == 5 && fields(0) == RescanFormat && fields(4).isEmpty
    require(v1 || v2, "Unsupported wallet generation descriptor")
    val generation = WalletGeneration(fields(1), fields(2), if (v2) Some(fields(3)) else None)
    require(encode(generation).sameElements(bytes), "Noncanonical wallet generation descriptor")
    generation
  }

  def selected(settings: ErgoSettings): Option[WalletGeneration] = {
    try Some(decode(Files.readAllBytes(descriptor(settings)))) catch {
      case _: NoSuchFileException => None
    }
  }

  def matchesSelectedDescriptor(generation: WalletGeneration, settings: ErgoSettings): Try[Boolean] =
    Try(Files.readAllBytes(descriptor(settings)).sameElements(encode(generation)))

  /** Called only after every handle on the failed candidate registry has closed successfully. */
  def discardUnselectedRescanCandidate(candidate: WalletGeneration, settings: ErgoSettings): Try[Unit] = Try {
    require(candidate.registryId.isDefined, "Only a rescan registry candidate may be discarded")
    encode(candidate)
    val root = new File(settings.directory).toPath.toAbsolutePath.normalize
    val generations = root.resolve("wallet").resolve("generations")
    val generationFolder = generations.resolve(candidate.id)
    val folder = candidate.registryFolder(settings).toPath.toAbsolutePath.normalize
    require(folder == generationFolder.resolve(s"registry-${candidate.registryId.get}") &&
      folder.startsWith(generationFolder), "Rescan registry candidate escapes its generation")
    val realRoot = root.toRealPath()
    Seq(root.resolve("wallet"), generations, generationFolder, folder).foreach { path =>
      val attrs = Files.readAttributes(path, classOf[BasicFileAttributes], LinkOption.NOFOLLOW_LINKS)
      require(attrs.isDirectory && !attrs.isSymbolicLink && !attrs.isOther,
        "Rescan registry candidate has a missing or linked path component")
    }
    require(folder.toRealPath().startsWith(
      realRoot.resolve("wallet").resolve("generations").resolve(candidate.id)),
      "Rescan registry candidate escapes the wallet data root")

    // Windows junctions are directories with isOther=true, not symbolic links.
    // Refuse links and other reparse points before descending in either walk.
    Files.walkFileTree(folder, new SimpleFileVisitor[Path] {
      override def preVisitDirectory(dir: Path, attrs: BasicFileAttributes): FileVisitResult = {
        require(!attrs.isSymbolicLink && !attrs.isOther && !Files.isSymbolicLink(dir),
          "Rescan registry candidate contains a link or reparse point")
        FileVisitResult.CONTINUE
      }
      override def visitFile(file: Path, attrs: BasicFileAttributes): FileVisitResult = {
        require(!attrs.isSymbolicLink && !attrs.isOther && !Files.isSymbolicLink(file),
          "Rescan registry candidate contains a link or reparse point")
        FileVisitResult.CONTINUE
      }
    })
    val observed = decode(Files.readAllBytes(descriptor(settings)))
    require(observed.id == candidate.id && observed.secretFile == candidate.secretFile &&
      observed.registryId != candidate.registryId,
      "Rescan registry candidate is selected or wallet selection changed")
    Files.walkFileTree(folder, new SimpleFileVisitor[Path] {
      override def preVisitDirectory(dir: Path, attrs: BasicFileAttributes): FileVisitResult = {
        require(!attrs.isSymbolicLink && !attrs.isOther && !Files.isSymbolicLink(dir),
          "Rescan registry candidate contains a link or reparse point")
        FileVisitResult.CONTINUE
      }
      override def visitFile(file: Path, attrs: BasicFileAttributes): FileVisitResult = {
        require(!attrs.isSymbolicLink && !attrs.isOther && !Files.isSymbolicLink(file),
          "Rescan registry candidate contains a link or reparse point")
        Files.delete(file)
        FileVisitResult.CONTINUE
      }
      override def postVisitDirectory(dir: Path, error: IOException): FileVisitResult = {
        if (error != null) throw error
        val attrs = Files.readAttributes(dir, classOf[BasicFileAttributes], LinkOption.NOFOLLOW_LINKS)
        require(attrs.isDirectory && !attrs.isSymbolicLink && !attrs.isOther,
          "Rescan registry candidate contains a link or reparse point")
        Files.delete(dir)
        FileVisitResult.CONTINUE
      }
    })
  }

  def validateReferences(generation: WalletGeneration, settings: ErgoSettings): Unit = {
    val registry = generation.registryFolder(settings)
    val files = Seq(new File(registry, "ldb_main/CURRENT"), new File(registry, "ldb_undo/CURRENT"),
      new File(generation.storageFolder(settings), "CURRENT"), generation.secret(settings.walletSettings.secretStorage))
    require(files.forall(file => Files.isRegularFile(file.toPath, LinkOption.NOFOLLOW_LINKS)),
      "Selected wallet generation is incomplete; retain its files and restore the complete generation before restarting")
  }

  def registryFolder(state: ErgoWalletState, settings: ErgoSettings): File =
    state.generation.map(_.registryFolder(settings)).getOrElse(WalletRegistry.registryFolder(settings))

  def storageFolder(state: ErgoWalletState, settings: ErgoSettings): File =
    state.generation.map(_.storageFolder(settings)).getOrElse(WalletStorage.storageFolder(settings))

  private def closeWithFailure(closes: Seq[() => Unit], failure: Throwable): Unit = closes.foreach { close =>
    try close() catch {
      case NonFatal(error) => if (error ne failure) failure.addSuppressed(error)
    }
  }

  def initial(settings: ErgoSettings, parameters: Parameters): Try[ErgoWalletState] = Try {
    val generation = selected(settings)
    generation.foreach(validateReferences(_, settings))
    val registryFolder = generation.map(_.registryFolder(settings)).getOrElse(WalletRegistry.registryFolder(settings))
    val storageFolder = generation.map(_.storageFolder(settings)).getOrElse(WalletStorage.storageFolder(settings))
    val registry = WalletRegistry.openAt(settings, registryFolder).get
    var storage: Option[WalletStorage] = None
    try {
      val openedStorage = WalletStorage.openAt(settings, storageFolder)
      storage = Some(openedStorage)
      ErgoWalletState(openedStorage, None, registry, OffChainRegistry.init(registry), None,
        WalletVars(openedStorage, settings), None, None, None, parameters, settings.walletSettings.maxInputs,
        rescanInProgress = false, generation = generation)
    } catch {
      case t: Throwable =>
        closeWithFailure(storage.map(s => () => s.close()).toSeq :+ (() => registry.close()), t)
        throw t
    }
  }
}
