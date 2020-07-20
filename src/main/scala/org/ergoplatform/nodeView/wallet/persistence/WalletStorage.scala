package org.ergoplatform.nodeView.wallet.persistence

import com.google.common.primitives.{Ints, Shorts}
import org.ergoplatform.nodeView.state.{ErgoStateContext, ErgoStateContextSerializer}
import org.ergoplatform.nodeView.wallet.scanning.{ScanRequest, Scan, ScanSerializer}
import org.ergoplatform.settings.{Constants, ErgoSettings}
import org.ergoplatform.wallet.secrets.{DerivationPath, DerivationPathSerializer, ExtendedPublicKey, ExtendedPublicKeySerializer}
import org.ergoplatform.{ErgoAddressEncoder, P2PKAddress}
import scorex.crypto.hash.Blake2b256
import org.ergoplatform.wallet.Constants.{ScanId, PaymentsScanId}
import scorex.db.{LDBFactory, LDBKVStore}

import scala.util.{Success, Try}

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
final class WalletStorage(store: LDBKVStore, settings: ErgoSettings)
                         (implicit val addressEncoder: ErgoAddressEncoder) {

  import WalletStorage._

  //todo: used now only for importing pre-3.3.0 wallet database, remove after while
  def readPaths(): Seq[DerivationPath] = store
    .get(SecretPathsKey)
    .toSeq
    .flatMap { r =>
      // TODO refactor: read using Reader
      val qty = Ints.fromByteArray(r.take(4))
      (0 until qty).foldLeft((Seq.empty[DerivationPath], r.drop(4))) { case ((acc, bytes), _) =>
        val length = Ints.fromByteArray(bytes.take(4))
        val pathTry = DerivationPathSerializer.parseBytesTry(bytes.slice(4, 4 + length))
        val newAcc = pathTry.map(acc :+ _).getOrElse(acc)
        val bytesTail = bytes.drop(4 + length)
        newAcc -> bytesTail
      }._1
    }

  /**
    * Remove pre-3.3.0 derivation paths
    */
  def removePaths(): Unit = store.remove(Seq(SecretPathsKey))

  /**
    * Store wallet-related public key in the database
    * @param publicKey - public key to store
    */
  def addKey(publicKey: ExtendedPublicKey): Unit = {
    store.insert(Seq(pubKeyPrefixKey(publicKey) -> ExtendedPublicKeySerializer.toBytes(publicKey)))
  }

  /**
    * Read wallet-related public keys from the database
    * @return wallet public keys
    */
  def readAllKeys(): Seq[ExtendedPublicKey] = store.getRange(FirstPublicKeyId, LastPublicKeyId).map { case (_, v) =>
    ExtendedPublicKeySerializer.parseBytes(v)
  }

  /**
    * Write state context into the database
    * @param ctx - state context
    */
  def updateStateContext(ctx: ErgoStateContext): Unit = store
    .insert(Seq(StateContextKey -> ctx.bytes))

  /**
    * Read state context from the database
    * @return state context read
    */
  def readStateContext: ErgoStateContext = store
    .get(StateContextKey)
    .flatMap(r => ErgoStateContextSerializer(settings.chainSettings.voting).parseBytesTry(r).toOption)
    .getOrElse(ErgoStateContext.empty(settings))

  /**
    * Update address used by the wallet for change outputs
    * @param address - new changed address
    */
  def updateChangeAddress(address: P2PKAddress): Unit = {
    val bytes = addressEncoder.toString(address).getBytes(Constants.StringEncoding)
    store.insert(Seq(ChangeAddressKey -> bytes))
  }

  /**
    * Read address used by the wallet for change outputs. If not set, default wallet address is used (root address)
    * @return optional change address
    */
  def readChangeAddress: Option[P2PKAddress] =
    store.get(ChangeAddressKey).flatMap { x =>
      addressEncoder.fromString(new String(x, Constants.StringEncoding)) match {
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
    val id = ScanId @@ (lastUsedscanId + 1).toShort
    scanReq.toScan(id).flatMap { app =>
      Try(store.insert(Seq(scanPrefixKey(id) -> ScanSerializer.toBytes(app)))).map(_ => app)
    }
  }

  /**
    * Remove an scan from the database
    * @param id scan identifier
    */
  def removeScan(id: Short): Unit =
    store.remove(Seq(scanPrefixKey(id)))

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
  def lastUsedscanId: Short = store.lastKeyInRange(SmallestPossibleScanId, BiggestPossibleScanId)
    .map(bs => Shorts.fromByteArray(bs.takeRight(2)))
    .getOrElse(PaymentsScanId)

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
  val BiggestPossibleScanId = ScanPrefixArray ++ Shorts.toByteArray(Short.MaxValue)

  def scanPrefixKey(scanId: Short): Array[Byte] = ScanPrefixArray ++ Shorts.toByteArray(scanId)
  def pubKeyPrefixKey(pk: ExtendedPublicKey): Array[Byte] = PublicKeyPrefixArray ++ pk.path.bytes


  // public keys space to iterate over all of them
  val FirstPublicKeyId: Array[Byte] = PublicKeyPrefixArray ++ Array.fill(33)(0: Byte)
  val LastPublicKeyId: Array[Byte] = PublicKeyPrefixArray ++ Array.fill(33)(-1: Byte)

  def noPrefixKey(keyString: String): Array[Byte] = Blake2b256.hash(keyString)

  //following keys do not start with ranged key prefix, i.e. with 8 zero bits
  val StateContextKey: Array[Byte] = noPrefixKey("state_ctx")
  val SecretPathsKey: Array[Byte] = noPrefixKey("secret_paths")
  val ChangeAddressKey: Array[Byte] = noPrefixKey("change_address")

  def readOrCreate(settings: ErgoSettings)
                  (implicit addressEncoder: ErgoAddressEncoder): WalletStorage = {
    new WalletStorage(LDBFactory.createKvDb(s"${settings.directory}/wallet/storage"), settings)
  }

}
