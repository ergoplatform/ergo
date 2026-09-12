package org.ergoplatform.nodeView.wallet

import java.io.{File, FileOutputStream, IOException}
import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{Files, LinkOption, NoSuchFileException, Path, StandardCopyOption}
import java.util.UUID

import org.ergoplatform.nodeView.wallet.persistence.{OffChainRegistry, WalletRegistry, WalletStorage}
import org.ergoplatform.settings.{ErgoSettings, Parameters}
import org.ergoplatform.wallet.secrets.JsonSecretStorage
import org.ergoplatform.wallet.settings.SecretStorageSettings
import scorex.util.ScorexLogging

import scala.util.control.NonFatal
import scala.util.{Failure, Success, Try}

/** Identifies a prepared wallet without persisting machine-specific directory paths. */
final case class WalletGeneration(id: String, secretFile: String) {
  def registryFolder(settings: ErgoSettings): File = new File(WalletInitialization.dataFolder(settings, id), "registry")
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
}

private[wallet] object WalletInitialization {
  private val Format = "ergo-wallet-generation-v1"
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
    s"$Format\n${generation.id}\n${generation.secretFile}\n".getBytes(UTF_8)
  }

  private def decode(bytes: Array[Byte]): WalletGeneration = {
    val fields = new String(bytes, UTF_8).split("\n", -1)
    require(fields.length == 4 && fields(0) == Format && fields(3).isEmpty, "Unsupported wallet generation descriptor")
    val generation = WalletGeneration(fields(1), fields(2))
    require(encode(generation).sameElements(bytes), "Noncanonical wallet generation descriptor")
    generation
  }

  def selected(settings: ErgoSettings): Option[WalletGeneration] = {
    try Some(decode(Files.readAllBytes(descriptor(settings)))) catch {
      case _: NoSuchFileException => None
    }
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
