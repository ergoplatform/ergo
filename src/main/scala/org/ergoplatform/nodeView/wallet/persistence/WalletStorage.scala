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
import scorex.util.{ModifierId, ScorexLogging, bytesToId, idToBytes}
import sigma.serialization.SigmaSerializer

import java.io.File
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

  /** A malformed marker is an error, never an absent quarantine. */
  def deepForkQuarantine: Try[Boolean] = Try {
    store.get(DeepForkQuarantineKey) match {
      case None => false
      case Some(bytes) if java.util.Arrays.equals(bytes, DeepForkQuarantineValue) => true
      case Some(bytes) if java.util.Arrays.equals(bytes, ClearedDeepForkQuarantineValue) => false
      case Some(_) => throw new IllegalStateException("Invalid wallet deep-fork quarantine marker")
    }
  }

  /** Fence the registry durably before handling a rollback with no retained version. */
  def quarantineDeepFork(): Try[Unit] = deepForkQuarantine.flatMap {
    case true => Success(())
    case false =>
      store.insertSync(DeepForkQuarantineKey, DeepForkQuarantineValue).flatMap { _ =>
        deepForkQuarantine.flatMap {
          case true => Success(())
          case false => Failure(new IllegalStateException("Wallet deep-fork quarantine marker was not readable after write"))
        }
      }
  }

  /** Only the actor may clear this after proving its checkpoint on the holder-applied chain. */
  def clearDeepForkQuarantine(): Try[Unit] =
    store.insertSync(DeepForkQuarantineKey, ClearedDeepForkQuarantineValue).flatMap { _ =>
      deepForkQuarantine.flatMap {
        case false => Success(())
        case true => Failure(new IllegalStateException("Wallet deep-fork quarantine marker did not clear"))
      }
    }

  /** A pending rollback fences the registry even when both databases can be opened. */
  def retainedRollbackIntent: Try[Option[RetainedRollbackIntent]] = Try {
    store.get(RetainedRollbackIntentKey) match {
      case None => None
      case Some(bytes) if java.util.Arrays.equals(bytes, ClearedRollbackIntent) => None
      case Some(bytes) if bytes.length == 65 && bytes(0) == PendingRollbackIntentVersion =>
        Some(RetainedRollbackIntent(bytesToId(bytes.slice(1, 33)), bytesToId(bytes.slice(33, 65))))
      case Some(_) => throw new IllegalStateException("Invalid wallet retained-rollback intent")
    }
  }

  /** Sync and read back the exact source and target before touching the registry. */
  def beginRetainedRollback(source: ModifierId, target: ModifierId): Try[Unit] =
    retainedRollbackIntent.flatMap {
      case Some(_) => Failure(new IllegalStateException("Wallet retained-rollback intent is already pending"))
      case None =>
        Try {
          val sourceBytes = idToBytes(source)
          val targetBytes = idToBytes(target)
          require(sourceBytes.length == 32 && targetBytes.length == 32, "Invalid wallet rollback version")
          Array(PendingRollbackIntentVersion) ++ sourceBytes ++ targetBytes
        }.flatMap { value =>
          store.insertSync(RetainedRollbackIntentKey, value).flatMap { _ =>
            requireRollbackIntentValue(value)
          }
        }
    }

  /** A synced tombstone is the final step; an absent or different intent cannot be cleared. */
  def clearRetainedRollback(source: ModifierId, target: ModifierId): Try[Unit] =
    retainedRollbackIntent.flatMap {
      case Some(intent) if intent.source == source && intent.target == target =>
        store.insertSync(RetainedRollbackIntentKey, ClearedRollbackIntent).flatMap { _ =>
          requireRollbackIntentValue(ClearedRollbackIntent)
        }
      case _ => Failure(new IllegalStateException("Wallet retained-rollback intent does not match"))
    }

  private def requireRollbackIntentValue(expected: Array[Byte]): Try[Unit] = Try {
    if (!store.get(RetainedRollbackIntentKey).exists(actual => java.util.Arrays.equals(actual, expected))) {
      throw new IllegalStateException("Wallet retained-rollback intent was not readable after sync write")
    }
  }

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

  /**
    * Close wallet storage database
    */
  def close(): Unit = {
    store.close()
  }

}

object WalletStorage {

  final case class RetainedRollbackIntent(source: ModifierId, target: ModifierId)

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
  private val DeepForkQuarantineKey: Array[Byte] = noPrefixKey("deep_fork_quarantine_v1")
  private val DeepForkQuarantineValue: Array[Byte] = Array(1: Byte)
  private val ClearedDeepForkQuarantineValue: Array[Byte] = Array(0: Byte)
  private val RetainedRollbackIntentKey: Array[Byte] = noPrefixKey("retained_rollback_intent_v1")
  private val PendingRollbackIntentVersion: Byte = 1: Byte
  private val ClearedRollbackIntent: Array[Byte] = Array(0: Byte)


  /**
    * @return folder (as an instance of java.io.File) where wallet storage database stored
    */
  def storageFolder(settings: ErgoSettings): File = new File(s"${settings.directory}/wallet/storage")

  def readOrCreate(settings: ErgoSettings): WalletStorage = {
    val db = LDBFactory.createKvDb(storageFolder(settings).getPath)
    new WalletStorage(db, settings)
  }

}
