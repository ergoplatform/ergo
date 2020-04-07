package org.ergoplatform.nodeView.wallet.persistence

import com.google.common.primitives.{Ints, Shorts}
import org.ergoplatform.nodeView.state.{ErgoStateContext, ErgoStateContextSerializer}
import org.ergoplatform.nodeView.wallet.scanning.{ExternalAppRequest, ExternalApplication, ExternalApplicationSerializer}
import org.ergoplatform.settings.{Constants, ErgoSettings}
import org.ergoplatform.wallet.secrets.{DerivationPath, DerivationPathSerializer, ExtendedPublicKey, ExtendedPublicKeySerializer}
import org.ergoplatform.{ErgoAddressEncoder, P2PKAddress}
import scorex.crypto.authds.ADDigest
import scorex.crypto.hash.Blake2b256
import org.ergoplatform.wallet.Constants.{ApplicationId, PaymentsAppId}
import scorex.db.{LDBFactory, LDBKVStore}

import scala.util.{Success, Try}

/**
  * Persists version-agnostic wallet actor's mutable state:
  * * tracked addresses
  * * derivation paths
  * * changed addresses
  * * ErgoStateContext (is it version-agnostic?)
  * * external applications
  */
final class WalletStorage(store: LDBKVStore, settings: ErgoSettings)
                         (implicit val addressEncoder: ErgoAddressEncoder) {

  import WalletStorage._

  //todo: pre-3.3.0 method for storing derivation paths, not used anymore, aside of test for readPaths
  //      remove after 3.3.0 release
  private[persistence] def addPath(derivationPath: DerivationPath): Unit = {
    val updatedPaths = (readPaths :+ derivationPath).toSet
    val toInsert = Ints.toByteArray(updatedPaths.size) ++ updatedPaths
      .foldLeft(Array.empty[Byte]) { case (acc, path) =>
        val bytes = DerivationPathSerializer.toBytes(path)
        acc ++ Ints.toByteArray(bytes.length) ++ bytes
      }
    store.insert(Seq(SecretPathsKey -> toInsert))
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
    .getOrElse(ErgoStateContext.empty(ADDigest @@ Array.fill(32)(0: Byte), settings))

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
    * Register an application (according to EIP-1)
    * @param appReq - request for an application
    * @return application or error (e.g. if application identifier space is exhausted)
    */
  def addApplication(appReq: ExternalAppRequest): Try[ExternalApplication] = {
    val id = ApplicationId @@ (lastUsedAppId + 1).toShort
    appReq.toApp(id).flatMap { app =>
      Try(store.insert(Seq(appPrefixKey(id) -> ExternalApplicationSerializer.toBytes(app)))).map(_ => app)
    }
  }

  /**
    * Remove an application from the database
    * @param id application identifier
    */
  def removeApplication(id: Short): Unit =
    store.remove(Seq(appPrefixKey(id)))

  /**
    * Get application by its identifier
    * @param id application identifier
    * @return application stored in the database, or None
    */
  def getApplication(id: Short): Option[ExternalApplication] =
    store.get(appPrefixKey(id)).map(bytes => ExternalApplicationSerializer.parseBytes(bytes))

  /**
    * Read all the applications from the database
    * @return applications stored in the database
    */
  def allApplications: Seq[ExternalApplication] = {
    store.getRange(SmallestPossibleApplicationId, BiggestPossibleApplicationId)
      .map { case (_, v) => ExternalApplicationSerializer.parseBytes(v) }
  }

  /**
    * Last inserted application identifier (as they are growing sequentially)
    * @return identifier of last inserted application
    */
  def lastUsedAppId: Short = store.lastKeyInRange(SmallestPossibleApplicationId, BiggestPossibleApplicationId)
    .map(bs => Shorts.fromByteArray(bs.takeRight(2)))
    .getOrElse(PaymentsAppId)

}

object WalletStorage {
  val RangedKeyPrefix: Byte = 0: Byte

  val ApplicationPrefixByte: Byte = 1: Byte
  val PublicKeyPrefixByte: Byte = 2: Byte

  val ApplicationPrefixArray: Array[Byte] = Array(RangedKeyPrefix, ApplicationPrefixByte)
  val PublicKeyPrefixArray: Array[Byte] = Array(RangedKeyPrefix, PublicKeyPrefixByte)

  val SmallestPossibleApplicationId = ApplicationPrefixArray ++ Shorts.toByteArray(0)
  val BiggestPossibleApplicationId = ApplicationPrefixArray ++ Shorts.toByteArray(Short.MaxValue)


  val FirstPublicKeyId = PublicKeyPrefixArray ++ Array.fill(33)(0: Byte)
  val LastPublicKeyId = PublicKeyPrefixArray ++ Array.fill(33)(-1: Byte)

  def noPrefixKey(keyString: String): Array[Byte] = Blake2b256.hash(keyString)

  def appPrefixKey(appId: Short): Array[Byte] = ApplicationPrefixArray ++ Shorts.toByteArray(appId)

  def pubKeyPrefixKey(pk: ExtendedPublicKey): Array[Byte] = PublicKeyPrefixArray ++ pk.path.bytes

  //following keys do not start with ranged key prefix, i.e. with 8 zero bits
  val StateContextKey: Array[Byte] = noPrefixKey("state_ctx")
  val SecretPathsKey: Array[Byte] = noPrefixKey("secret_paths")
  val ChangeAddressKey: Array[Byte] = noPrefixKey("change_address")

  def readOrCreate(settings: ErgoSettings)
                  (implicit addressEncoder: ErgoAddressEncoder): WalletStorage = {
    new WalletStorage(LDBFactory.createKvDb(s"${settings.directory}/wallet/storage"), settings)
  }

}
