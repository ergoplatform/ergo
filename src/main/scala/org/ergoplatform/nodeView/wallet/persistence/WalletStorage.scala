package org.ergoplatform.nodeView.wallet.persistence

import com.google.common.primitives.{Ints, Shorts}
import org.ergoplatform.P2PKAddress
import org.ergoplatform.nodeView.state.{ErgoStateContext, ErgoStateContextSerializer}
import org.ergoplatform.nodeView.wallet.scanning.{Scan, ScanRequest, ScanSerializer}
import org.ergoplatform.sdk.wallet.secrets.{DerivationPath, DerivationPathSerializer, ExtendedPublicKey, ExtendedPublicKeySerializer}
import org.ergoplatform.settings.{Constants, ErgoSettings, Parameters}
import org.ergoplatform.wallet.Constants.{PaymentsScanId, ScanId}
import scorex.crypto.hash.Blake2b256
import scorex.db.{LDBFactory, LDBKVStore}
import scorex.util.ScorexLogging
import scorex.util.serialization.VLQByteBufferReader
import sigma.serialization.SigmaSerializer

import java.io.File
import java.nio.ByteBuffer
import scala.util.{Failure, Success, Try}

/**
  * Persists version-agnostic wallet actor's mutable state (which is not a subject to rollbacks in case of forks)
  *  (so data which do not have different versions unlike blockchain-related objects):
  *
  * * tracked addresses
  * * derivation paths
  * * changed addresses
  * * ErgoStateContext (not version-agnostic, but state changes including rollbacks it is updated externally)
  * * external scans
  */
final class WalletStorage(store: LDBKVStore, settings: ErgoSettings) extends ScorexLogging {

  import WalletStorage._

  private var cachedStateContext: Option[ErgoStateContext] = None

  //todo: used now only for importing pre-3.3.0 wallet database, remove after while
  def readPaths(): Seq[DerivationPath] = store
    .get(SecretPathsKey)
    .toSeq
    .flatMap { r =>
      // TODO refactor: read using Reader
      val qty = Ints.fromByteArray(r.take(4))
      (0 until qty).foldLeft((Seq.empty[DerivationPath], r.drop(4))) { case ((acc, bytes), _) =>
        val length = Ints.fromByteArray(bytes.take(4))
        val r = SigmaSerializer.startReader(bytes.slice(4, 4 + length))
        val pathTry = DerivationPathSerializer.parseTry(r)
        val newAcc = pathTry.map(acc :+ _).getOrElse(acc)
        val bytesTail = bytes.drop(4 + length)
        newAcc -> bytesTail
      }._1
    }

  /**
    * Remove pre-3.3.0 derivation paths
    */
  def removePaths(): Try[Unit] = store.remove(Array(SecretPathsKey))

  /**
    * Store wallet-related public key in the database
    *
    * @param publicKey - public key to store
    */
  def addPublicKey(publicKey: ExtendedPublicKey): Try[Unit] = {
    store.insert(pubKeyPrefixKey(publicKey), ExtendedPublicKeySerializer.toBytes(publicKey))
  }

  /**
    * Read public key corresponding to a provided derivation path
    */
  def getPublicKey(path: DerivationPath): Option[ExtendedPublicKey] = {
    store
      .get(pubKeyPrefixKey(path))
      .flatMap { bytes =>
        val r = SigmaSerializer.startReader(bytes)
        ExtendedPublicKeySerializer.parseTry(r) match {
          case Success(key) =>
            Some(key)
          case Failure(t) =>
            log.error(s"Corrupted data when reading public key data for $path : ", t)
            None
        }
      }
  }

  def containsPublicKey(path: DerivationPath): Boolean = {
    getPublicKey(path).isDefined
  }

  /**
    * Read wallet-related public keys from the database
    * @return wallet public keys
    */
  def readAllKeys(): Seq[ExtendedPublicKey] = {
    store.getRange(FirstPublicKeyId, LastPublicKeyId).map { case (_, v) =>
      ExtendedPublicKeySerializer.fromBytes(v)
    }
  }

  def getStateContext(parameters: Parameters): ErgoStateContext = cachedStateContext.getOrElse(readStateContext(parameters))

  /**
    * Write state context into the database
    * @param ctx - state context
    */
  def updateStateContext(ctx: ErgoStateContext): Try[Unit] = {
    cachedStateContext = Some(ctx)
    store.insert(StateContextKey, ctx.bytes)
  }

  /**
    * Read state context from the database
    * @return state context read
    */
  def readStateContext(parameters: Parameters): ErgoStateContext = {
    cachedStateContext = Some(store
      .get(StateContextKey)
      .flatMap(r => ErgoStateContextSerializer(settings.chainSettings).parseBytesTry(r).toOption)
      .getOrElse(ErgoStateContext.empty(settings.chainSettings, parameters))
    )
    cachedStateContext.get
  }

  /**
    * Update address used by the wallet for change outputs
    * @param address - new changed address
    */
  def updateChangeAddress(address: P2PKAddress): Try[Unit] = {
    val bytes = settings.chainSettings.addressEncoder.toString(address).getBytes(Constants.StringEncoding)
    store.insert(ChangeAddressKey, bytes)
  }

  /**
    * Read address used by the wallet for change outputs. If not set, default wallet address is used (root address)
    * @return optional change address
    */
  def readChangeAddress: Option[P2PKAddress] =
    store.get(ChangeAddressKey).flatMap { x =>
      settings.chainSettings.addressEncoder.fromString(new String(x, Constants.StringEncoding)) match {
        case Success(p2pk: P2PKAddress) => Some(p2pk)
        case _ => None
      }
    }

  /**
    * Register an scan (according to EIP-1)
    * @param scanReq - request for an scan
    * @return scan or error (e.g. if scan identifier space is exhausted)
    */
  def addScan(scanReq: ScanRequest): Try[Scan] = {
    val id = ScanId @@ (lastUsedScanId + 1).toShort
    scanReq.toScan(id).flatMap { app =>
      store.insert(
        Array(scanPrefixKey(id), lastUsedScanIdKey),
        Array(ScanSerializer.toBytes(app), Shorts.toByteArray(id))
      ).map(_ => app)
    }
  }

  /**
    * Remove an scan from the database
    * @param id scan identifier
    */
  def removeScan(id: Short): Try[Unit] =
    store.remove(Array(scanPrefixKey(id)))

  /**
    * Get scan by its identifier
    * @param id scan identifier
    * @return scan stored in the database, or None
    */
  def getScan(id: Short): Option[Scan] =
    store.get(scanPrefixKey(id)).map(bytes => ScanSerializer.parseBytes(bytes))

  /**
    * Read all the scans from the database
    * @return scans stored in the database
    */
  def allScans: Seq[Scan] = {
    store.getRange(SmallestPossibleScanId, BiggestPossibleScanId)
      .map { case (_, v) => ScanSerializer.parseBytes(v) }
  }

  /**
    * Last inserted scan identifier (as they are growing sequentially)
    * @return identifier of last inserted scan
    */
  def lastUsedScanId: Short = {
    // pre-3.3.7 method to get last used scan id, now useful to read pre-3.3.7 databases
    def oldScanId: Option[Short] =
      store.lastKeyInRange(SmallestPossibleScanId, BiggestPossibleScanId)
        .map(bs => Shorts.fromByteArray(bs.takeRight(2)))

    store.get(lastUsedScanIdKey)
      .map(bs => Shorts.fromByteArray(bs))
      .orElse(oldScanId)
      .getOrElse(PaymentsScanId)
  }

  /** Read scan progress without treating corrupt durable bytes as an absent status. */
  def readUtxoSnapshotScanStatusTry(): Try[Option[UtxoSnapshotScanStatus]] =
    Try(store.get(UtxoSnapshotScanStatusKey)).flatMap {
      case Some(bytes) =>
        Try {
          val reader = new VLQByteBufferReader(ByteBuffer.wrap(bytes))
          val status = UtxoSnapshotScanStatusSerializer.parse(reader)
          require(reader.remaining == 0, s"Unexpected trailing UTXO snapshot scan status bytes: ${reader.remaining}")
          validateUtxoSnapshotScanStatus(status).get
          require(java.util.Arrays.equals(bytes, UtxoSnapshotScanStatusSerializer.toBytes(status)),
            "Non-canonical UTXO snapshot scan status encoding")
          Some(status)
        }
      case None => Success(None)
    }

  def readUtxoSnapshotScanStatus(): Option[UtxoSnapshotScanStatus] =
    readUtxoSnapshotScanStatusTry() match {
      case Success(status) => status
      case Failure(t) =>
        log.error("Corrupted UTXO snapshot scan status", t)
        None
    }

  def writeUtxoSnapshotScanStatus(status: UtxoSnapshotScanStatus): Try[Unit] =
    validateUtxoSnapshotScanStatus(status)
      .flatMap(_ => store.insert(UtxoSnapshotScanStatusKey, UtxoSnapshotScanStatusSerializer.toBytes(status)))

  def removeUtxoSnapshotScanStatus(): Try[Unit] =
    store.remove(Array(UtxoSnapshotScanStatusKey))

  /** Read rollback recovery evidence without treating corrupt bytes as an absent intent. */
  def readWalletRollbackIntentTry(): Try[Option[WalletRollbackIntent]] =
    Try(store.get(WalletRollbackIntentKey)).flatMap {
      case Some(bytes) =>
        Try {
          val reader = new VLQByteBufferReader(ByteBuffer.wrap(bytes))
          val intent = WalletRollbackIntentSerializer.parse(reader)
          require(reader.remaining == 0,
            s"Unexpected trailing wallet rollback intent bytes: ${reader.remaining}")
          validateWalletRollbackIntent(intent).get
          require(java.util.Arrays.equals(bytes, WalletRollbackIntentSerializer.toBytes(intent)),
            "Non-canonical wallet rollback intent encoding")
          Some(intent)
        }
      case None => Success(None)
    }

  /**
    * Persist rollback evidence before mutating the versioned wallet registry.
    * Repeating the same intent is safe; a different active lifecycle is never overwritten.
    */
  def writeWalletRollbackIntent(intent: WalletRollbackIntent): Try[Unit] =
    validateWalletRollbackIntent(intent).flatMap { _ =>
      readWalletRollbackIntentTry().flatMap {
        case None =>
          store.insert(WalletRollbackIntentKey, WalletRollbackIntentSerializer.toBytes(intent))
        case Some(current) if current == intent =>
          Success(())
        case Some(current) =>
          Failure(new IllegalStateException(
            s"Conflicting wallet rollback intent: current=$current, requested=$intent"))
      }
    }

  /**
    * Replace rollback evidence only when the durable record still identifies the
    * expected lifecycle. The wallet actor serializes this read-check-write sequence.
    */
  def replaceWalletRollbackIntent(
    expected: WalletRollbackIntent,
    replacement: WalletRollbackIntent): Try[Boolean] =
    validateWalletRollbackIntent(expected).flatMap { _ =>
      validateWalletRollbackIntent(replacement)
    }.flatMap { _ =>
      readWalletRollbackIntentTry().flatMap {
        case Some(current) if current == expected =>
          store.insert(
            WalletRollbackIntentKey,
            WalletRollbackIntentSerializer.toBytes(replacement)).map(_ => true)
        case _ => Success(false)
      }
    }

  /** Remove rollback evidence only when it still identifies the completed lifecycle exactly. */
  def clearWalletRollbackIntent(expected: WalletRollbackIntent): Try[Boolean] =
    validateWalletRollbackIntent(expected).flatMap { _ =>
      readWalletRollbackIntentTry().flatMap {
        case Some(current) if current == expected =>
          store.remove(Array(WalletRollbackIntentKey)).map(_ => true)
        case _ => Success(false)
      }
    }

  /** Read completed snapshot provenance without treating corrupt durable bytes as absent. */
  def readUtxoSnapshotWalletOriginTry(): Try[Option[UtxoSnapshotWalletOrigin]] =
    Try(store.get(UtxoSnapshotWalletOriginKey)).flatMap {
      case Some(bytes) =>
        Try {
          val reader = new VLQByteBufferReader(ByteBuffer.wrap(bytes))
          val origin = UtxoSnapshotWalletOriginSerializer.parse(reader)
          require(reader.remaining == 0,
            s"Unexpected trailing UTXO snapshot wallet origin bytes: ${reader.remaining}")
          validateUtxoSnapshotWalletOrigin(origin).get
          require(java.util.Arrays.equals(bytes, UtxoSnapshotWalletOriginSerializer.toBytes(origin)),
            "Non-canonical UTXO snapshot wallet origin encoding")
          Some(origin)
        }
      case None => Success(None)
    }

  /**
    * Commit completed progress and its immutable snapshot provenance in one LevelDB batch.
    * An identical origin is replay-safe; corrupt or conflicting provenance is never overwritten.
    */
  def completeUtxoSnapshotScan(status: UtxoSnapshotScanStatus): Try[Unit] =
    validateUtxoSnapshotScanStatus(status).flatMap { _ =>
      Try(require(status.completed && status.nextSubtreeIndex == status.totalSubtrees,
        "UTXO snapshot completion requires canonical completed progress"))
    }.flatMap { _ =>
      val origin = UtxoSnapshotWalletOrigin(
        status.snapshotHeight, status.snapshotBlockId, status.scanDefinition)
      readUtxoSnapshotWalletOriginTry().flatMap {
        case None | Some(`origin`) =>
          store.update(
            Array(UtxoSnapshotScanStatusKey, UtxoSnapshotWalletOriginKey),
            Array(
              UtxoSnapshotScanStatusSerializer.toBytes(status),
              UtxoSnapshotWalletOriginSerializer.toBytes(origin)),
            Array.empty[Array[Byte]])
        case Some(current) =>
          Failure(new IllegalStateException(
            s"Conflicting UTXO snapshot wallet origin: current=$current, requested=$origin"))
      }
    }

  /** Read the durable invalidation fence without treating corrupt bytes as an absent marker. */
  def readUtxoSnapshotScanInvalidationTry(): Try[Option[UtxoSnapshotScanInvalidation]] =
    Try(store.get(UtxoSnapshotScanInvalidationKey)).flatMap {
      case Some(bytes) =>
        Try {
          val reader = new VLQByteBufferReader(ByteBuffer.wrap(bytes))
          val invalidation = UtxoSnapshotScanInvalidationSerializer.parse(reader)
          require(reader.remaining == 0, s"Unexpected trailing UTXO snapshot invalidation bytes: ${reader.remaining}")
          validateUtxoSnapshotScanInvalidation(invalidation).get
          require(java.util.Arrays.equals(bytes, UtxoSnapshotScanInvalidationSerializer.toBytes(invalidation)),
            "Non-canonical UTXO snapshot invalidation encoding")
          Some(invalidation)
        }
      case None => Success(None)
    }

  /**
    * Create the durable invalidation fence, or accept an identical existing fence idempotently.
    * A different or unreadable existing fence fails closed and is never overwritten.
    * This read-check-write sequence relies on the wallet actor serializing callers; it is not a database CAS.
    */
  def writeUtxoSnapshotScanInvalidation(invalidation: UtxoSnapshotScanInvalidation): Try[Unit] =
    validateUtxoSnapshotScanInvalidation(invalidation).flatMap { _ =>
      readUtxoSnapshotScanInvalidationTry().flatMap {
        case None =>
          store.insert(UtxoSnapshotScanInvalidationKey, UtxoSnapshotScanInvalidationSerializer.toBytes(invalidation))
        case Some(current) if current == invalidation =>
          Success(())
        case Some(current) =>
          Failure(new IllegalStateException(
            s"Conflicting UTXO snapshot invalidation: current=$current, requested=$invalidation"))
      }
    }

  /**
    * Clear snapshot recovery only when the current durable fence matches the expected lifecycle exactly.
    * Both the fence and progress are removed together, so recovery never observes a cleared fence with stale progress.
    */
  def clearUtxoSnapshotScanRecovery(expected: UtxoSnapshotScanInvalidation): Try[Boolean] =
    validateUtxoSnapshotScanInvalidation(expected).flatMap { _ =>
      readUtxoSnapshotScanInvalidationTry().flatMap {
        case Some(current) if current == expected =>
          store.update(
            Array.empty[Array[Byte]],
            Array.empty[Array[Byte]],
            Array(UtxoSnapshotScanInvalidationKey, UtxoSnapshotScanStatusKey)
          ).map(_ => true)
        case _ => Success(false)
      }
    }

  /**
    * Replace recovery progress with a fresh scan obligation only when the durable fence matches exactly.
    * The status put and fence deletion share one LevelDB batch: after a successful registry reset, a crash can
    * therefore reveal either the old fence or the fresh incomplete status, but never an unfenced empty obligation.
    * This read-check-write sequence relies on the wallet actor serializing callers; it is not a database CAS.
    */
  def restartUtxoSnapshotScanRecovery(
    expected: UtxoSnapshotScanInvalidation,
    freshStatus: UtxoSnapshotScanStatus): Try[Boolean] =
    validateUtxoSnapshotScanRecoveryRestart(expected, freshStatus).flatMap { _ =>
      validateUtxoSnapshotRecoveryOrigin(expected, freshStatus).flatMap { _ =>
        readUtxoSnapshotScanInvalidationTry().flatMap {
          case Some(current) if current == expected =>
            store.update(
              Array(UtxoSnapshotScanStatusKey),
              Array(UtxoSnapshotScanStatusSerializer.toBytes(freshStatus)),
              Array(UtxoSnapshotScanInvalidationKey)
            ).map(_ => true)
          case _ => Success(false)
        }
      }
    }

  private def validateUtxoSnapshotScanStatus(status: UtxoSnapshotScanStatus): Try[Unit] = Try {
    require(status != null, "UTXO snapshot scan status must not be null")
    require(status.scanDefinition != null, "UTXO snapshot scan definition must not be null")
    require(status.snapshotHeight >= 0, s"Invalid UTXO snapshot height ${status.snapshotHeight}")
    require(status.manifestDepth >= 0, s"Invalid UTXO snapshot manifest depth ${status.manifestDepth}")
    require(status.totalSubtrees > 0, s"Invalid UTXO snapshot part count ${status.totalSubtrees}")
    require(status.nextSubtreeIndex >= 0 && status.nextSubtreeIndex <= status.totalSubtrees,
      s"Invalid UTXO snapshot cursor ${status.nextSubtreeIndex}/${status.totalSubtrees}")
    require(status.completed == (status.nextSubtreeIndex == status.totalSubtrees),
      s"Inconsistent UTXO snapshot completion at ${status.nextSubtreeIndex}/${status.totalSubtrees}")
  }

  private def validateWalletRollbackIntent(intent: WalletRollbackIntent): Try[Unit] = Try {
    require(intent != null, "Wallet rollback intent must not be null")
    require(intent.expectedHeight >= 0,
      s"Invalid wallet rollback target height ${intent.expectedHeight}")
    require(scorex.util.idToBytes(intent.targetVersionId).length == Constants.ModifierIdSize,
      "Wallet rollback target must be a modifier id")
  }

  private def validateUtxoSnapshotScanInvalidation(invalidation: UtxoSnapshotScanInvalidation): Try[Unit] = Try {
    require(invalidation.snapshotHeight >= 0, s"Invalid UTXO snapshot invalidation height ${invalidation.snapshotHeight}")
  }

  private def validateUtxoSnapshotWalletOrigin(origin: UtxoSnapshotWalletOrigin): Try[Unit] = Try {
    require(origin != null, "UTXO snapshot wallet origin must not be null")
    require(origin.scanDefinition != null, "UTXO snapshot wallet origin definition must not be null")
    require(origin.snapshotHeight >= 0, s"Invalid UTXO snapshot wallet origin height ${origin.snapshotHeight}")
  }

  private def validateUtxoSnapshotRecoveryOrigin(
    expected: UtxoSnapshotScanInvalidation,
    freshStatus: UtxoSnapshotScanStatus): Try[Unit] =
    readUtxoSnapshotWalletOriginTry().flatMap {
      case None => Success(())
      case Some(origin)
        if origin.snapshotHeight == expected.snapshotHeight &&
          origin.snapshotBlockId == expected.snapshotBlockId &&
          origin.scanDefinition == freshStatus.scanDefinition => Success(())
      case Some(origin) =>
        Failure(new IllegalStateException(
          s"UTXO snapshot recovery $expected conflicts with completed wallet origin $origin"))
    }

  private def validateUtxoSnapshotScanRecoveryRestart(
    expected: UtxoSnapshotScanInvalidation,
    freshStatus: UtxoSnapshotScanStatus): Try[Unit] =
    validateUtxoSnapshotScanInvalidation(expected)
      .flatMap(_ => validateUtxoSnapshotScanStatus(freshStatus))
      .flatMap { _ =>
        Try {
          require(freshStatus.snapshotHeight == expected.snapshotHeight,
            s"Fresh UTXO snapshot recovery height ${freshStatus.snapshotHeight} does not match ${expected.snapshotHeight}")
          require(freshStatus.snapshotBlockId == expected.snapshotBlockId,
            "Fresh UTXO snapshot recovery block id does not match the invalidation fence")
          require(freshStatus.nextSubtreeIndex == 0,
            s"Fresh UTXO snapshot recovery cursor must be zero, got ${freshStatus.nextSubtreeIndex}")
          require(!freshStatus.completed, "Fresh UTXO snapshot recovery status must be incomplete")
        }
      }

  /**
    * Close wallet storage database
    */
  def close(): Unit = {
    store.close()
  }

}

object WalletStorage {

  /**
    * Primary prefix for entities with multiple instances, where iterating over keys space would be needed.
    */
  val RangedKeyPrefix: Byte = 0: Byte

  /**
    * Secondary prefix byte for scans bucket
    */
  val ScanPrefixByte: Byte = 1: Byte

  /**
    * Secondary prefix byte for public keys bucket
    */
  val PublicKeyPrefixByte: Byte = 2: Byte

  val ScanPrefixArray: Array[Byte] = Array(RangedKeyPrefix, ScanPrefixByte)
  val PublicKeyPrefixArray: Array[Byte] = Array(RangedKeyPrefix, PublicKeyPrefixByte)

  // scans key space to iterate over all of them
  val SmallestPossibleScanId: Array[Byte] = ScanPrefixArray ++ Shorts.toByteArray(0)
  val BiggestPossibleScanId: Array[Byte] = ScanPrefixArray ++ Shorts.toByteArray(Short.MaxValue)

  def scanPrefixKey(scanId: Short): Array[Byte] = ScanPrefixArray ++ Shorts.toByteArray(scanId)
  def pubKeyPrefixKey(path: DerivationPath): Array[Byte] = PublicKeyPrefixArray ++ path.bytes
  def pubKeyPrefixKey(pk: ExtendedPublicKey): Array[Byte] = pubKeyPrefixKey(pk.path)


  // public keys space to iterate over all of them
  val FirstPublicKeyId: Array[Byte] = PublicKeyPrefixArray ++ Array.fill(33)(0: Byte)
  val LastPublicKeyId: Array[Byte] = PublicKeyPrefixArray ++ Array.fill(33)(-1: Byte)

  def noPrefixKey(keyString: String): Array[Byte] = Blake2b256.hash(keyString)

  //following keys do not start with ranged key prefix, i.e. with 8 zero bits
  val StateContextKey: Array[Byte] = noPrefixKey("state_ctx")
  val SecretPathsKey: Array[Byte] = noPrefixKey("secret_paths")
  val ChangeAddressKey: Array[Byte] = noPrefixKey("change_address")
  val lastUsedScanIdKey: Array[Byte] = noPrefixKey("last_scan_id")
  val UtxoSnapshotScanStatusKey: Array[Byte] = noPrefixKey("utxo_snapshot_scan_status")
  val UtxoSnapshotScanInvalidationKey: Array[Byte] = noPrefixKey("utxo_snapshot_scan_invalidation")
  val UtxoSnapshotWalletOriginKey: Array[Byte] = noPrefixKey("utxo_snapshot_wallet_origin")
  val WalletRollbackIntentKey: Array[Byte] = noPrefixKey("wallet_rollback_intent")


  /**
    * @return folder (as an instance of java.io.File) where wallet storage database stored
    */
  def storageFolder(settings: ErgoSettings): File = new File(s"${settings.directory}/wallet/storage")

  def readOrCreate(settings: ErgoSettings): WalletStorage = {
    val db = LDBFactory.createKvDb(storageFolder(settings).getPath)
    new WalletStorage(db, settings)
  }

}
