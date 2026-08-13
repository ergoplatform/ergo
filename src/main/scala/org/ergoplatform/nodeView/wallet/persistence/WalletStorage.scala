package org.ergoplatform.nodeView.wallet.persistence

import com.google.common.primitives.{Ints, Shorts}
import org.ergoplatform.P2PKAddress
import org.ergoplatform.modifiers.mempool.{ErgoTransaction, ErgoTransactionSerializer}
import org.ergoplatform.nodeView.state.{ErgoStateContext, ErgoStateContextSerializer}
import org.ergoplatform.nodeView.wallet.scanning.{Scan, ScanRequest, ScanSerializer}
import org.ergoplatform.sdk.wallet.secrets.{DerivationPath, DerivationPathSerializer, ExtendedPublicKey, ExtendedPublicKeySerializer}
import org.ergoplatform.settings.{Constants, ErgoSettings, Parameters}
import org.ergoplatform.wallet.Constants.{PaymentsScanId, ScanId}
import scorex.crypto.hash.Blake2b256
import scorex.db.{LDBFactory, LDBKVStore}
import scorex.util.{ModifierId, ScorexLogging, idToBytes}
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
  * * wallet-related transactions which are not on the blockchain yet
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

  /**
    * Heights at which wallet-related unconfirmed transactions stored in the database were first seen,
    * by transaction identifier. Kept in memory so that applying a block does not need a database
    * lookup per transaction of the block, and so that pruning does not need to read the whole bucket.
    */
  private var cachedUnconfirmedTxHeights: Option[Map[ModifierId, Int]] = None

  /**
    * @return heights at which stored unconfirmed transactions were first seen, by transaction id
    */
  def unconfirmedTransactionHeights: Map[ModifierId, Int] = cachedUnconfirmedTxHeights.getOrElse {
    val heights = readUnconfirmedTransactions().map { case (tx, height) => tx.id -> height }.toMap
    cachedUnconfirmedTxHeights = Some(heights)
    heights
  }

  /**
    * Store a wallet-related transaction which is not on the blockchain yet, so that it survives
    * a node restart.
    *
    * A transaction already stored is left alone rather than re-dated: it is re-scanned every time it
    * is put back into the memory pool, and refreshing its height there would push its expiry back on
    * every restart, so a transaction which never confirms would be kept forever.
    *
    * @param tx           - unconfirmed transaction the wallet is interested in
    * @param seenAtHeight - blockchain height at the moment the transaction was first seen
    */
  def addUnconfirmedTransaction(tx: ErgoTransaction, seenAtHeight: Int): Try[Unit] = {
    if (unconfirmedTransactionHeights.contains(tx.id)) {
      Success(())
    } else {
      store.insert(unconfirmedTxKey(tx.id), Ints.toByteArray(seenAtHeight) ++ tx.bytes).map { _ =>
        cachedUnconfirmedTxHeights = Some(unconfirmedTransactionHeights.updated(tx.id, seenAtHeight))
      }
    }
  }

  /**
    * Forget stored unconfirmed transactions, e.g. once they got on the blockchain. Identifiers of
    * transactions which are not stored are ignored.
    */
  def removeUnconfirmedTransactions(ids: Seq[ModifierId]): Try[Unit] = {
    val known = unconfirmedTransactionHeights
    val toRemove = ids.filter(known.contains)
    if (toRemove.isEmpty) {
      Success(())
    } else {
      store.remove(toRemove.map(unconfirmedTxKey).toArray).map { _ =>
        cachedUnconfirmedTxHeights = Some(known -- toRemove)
      }
    }
  }

  /**
    * Read unconfirmed transactions stored, along with the height each of them was seen at.
    * Records which can not be parsed are skipped, a corrupted record must not prevent the wallet
    * from starting.
    */
  def readUnconfirmedTransactions(): Seq[(ErgoTransaction, Int)] = {
    store.getRange(FirstUnconfirmedTxId, LastUnconfirmedTxId).flatMap { case (_, v) =>
      ErgoTransactionSerializer.parseBytesTry(v.drop(java.lang.Integer.BYTES)) match {
        case Success(tx) =>
          Some(tx -> Ints.fromByteArray(v.take(java.lang.Integer.BYTES)))
        case Failure(t) =>
          log.error("Corrupted data when reading an unconfirmed transaction: ", t)
          None
      }
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

  /**
    * Secondary prefix byte for the bucket of wallet-related transactions which are not on the
    * blockchain yet
    */
  val UnconfirmedTxPrefixByte: Byte = 3: Byte

  val ScanPrefixArray: Array[Byte] = Array(RangedKeyPrefix, ScanPrefixByte)
  val PublicKeyPrefixArray: Array[Byte] = Array(RangedKeyPrefix, PublicKeyPrefixByte)
  val UnconfirmedTxPrefixArray: Array[Byte] = Array(RangedKeyPrefix, UnconfirmedTxPrefixByte)

  // scans key space to iterate over all of them
  val SmallestPossibleScanId: Array[Byte] = ScanPrefixArray ++ Shorts.toByteArray(0)
  val BiggestPossibleScanId: Array[Byte] = ScanPrefixArray ++ Shorts.toByteArray(Short.MaxValue)

  def scanPrefixKey(scanId: Short): Array[Byte] = ScanPrefixArray ++ Shorts.toByteArray(scanId)
  def pubKeyPrefixKey(path: DerivationPath): Array[Byte] = PublicKeyPrefixArray ++ path.bytes
  def pubKeyPrefixKey(pk: ExtendedPublicKey): Array[Byte] = pubKeyPrefixKey(pk.path)


  // public keys space to iterate over all of them
  val FirstPublicKeyId: Array[Byte] = PublicKeyPrefixArray ++ Array.fill(33)(0: Byte)
  val LastPublicKeyId: Array[Byte] = PublicKeyPrefixArray ++ Array.fill(33)(-1: Byte)

  def unconfirmedTxKey(txId: ModifierId): Array[Byte] = UnconfirmedTxPrefixArray ++ idToBytes(txId)

  // unconfirmed transactions space to iterate over all of them
  val FirstUnconfirmedTxId: Array[Byte] = UnconfirmedTxPrefixArray ++ Array.fill(32)(0: Byte)
  val LastUnconfirmedTxId: Array[Byte] = UnconfirmedTxPrefixArray ++ Array.fill(32)(-1: Byte)

  /**
    * For how many blocks a wallet-related unconfirmed transaction is kept in the database before
    * being given up on. A transaction which did not get on the blockchain within this many blocks
    * is most likely never going to, and re-submitting it forever would only keep the wallet from
    * spending its inputs. Deliberately conservative (about two days at the target block rate).
    */
  val UnconfirmedTxLifetimeInBlocks: Int = 1440

  def noPrefixKey(keyString: String): Array[Byte] = Blake2b256.hash(keyString)

  //following keys do not start with ranged key prefix, i.e. with 8 zero bits
  val StateContextKey: Array[Byte] = noPrefixKey("state_ctx")
  val SecretPathsKey: Array[Byte] = noPrefixKey("secret_paths")
  val ChangeAddressKey: Array[Byte] = noPrefixKey("change_address")
  val lastUsedScanIdKey: Array[Byte] = noPrefixKey("last_scan_id")


  /**
    * @return folder (as an instance of java.io.File) where wallet storage database stored
    */
  def storageFolder(settings: ErgoSettings): File = new File(s"${settings.directory}/wallet/storage")

  def readOrCreate(settings: ErgoSettings): WalletStorage = {
    val db = LDBFactory.createKvDb(storageFolder(settings).getPath)
    new WalletStorage(db, settings)
  }

}
