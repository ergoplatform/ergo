package org.ergoplatform.nodeView.wallet.persistence

import com.google.common.primitives.{Ints, Shorts}
import org.ergoplatform.nodeView.state.{ErgoStateContext, ErgoStateContextSerializer}
import org.ergoplatform.nodeView.wallet.scanning.{ExternalAppRequest, ExternalApplication, ExternalApplicationSerializer}
import org.ergoplatform.settings.{Constants, ErgoSettings}
import org.ergoplatform.wallet.secrets.{DerivationPath, DerivationPathSerializer, ExtendedPublicKey}
import org.ergoplatform.{ErgoAddressEncoder, P2PKAddress}
import scorex.crypto.authds.ADDigest
import scorex.crypto.hash.Blake2b256
import org.ergoplatform.wallet.Constants.PaymentsAppId
import scorex.db.{LDBFactory, LDBKVStore}

import scala.util.{Success, Try}

/**
  * Persists version-agnostic wallet actor's mutable state:
  *   * tracked addresses
  *   * derivation paths
  *   * changed addresses
  *   * ErgoStateContext (is it version-agnostic?)
  *   * external applications
  */
final class WalletStorage(store: LDBKVStore, settings: ErgoSettings)
                         (implicit val addressEncoder: ErgoAddressEncoder) {

  import WalletStorage._

  private[persistence] def addPath(derivationPath: DerivationPath) = {
    val updatedPaths = (readPaths :+ derivationPath).toSet
    val toInsert = Ints.toByteArray(updatedPaths.size) ++ updatedPaths
      .foldLeft(Array.empty[Byte]) { case (acc, path) =>
        val bytes = DerivationPathSerializer.toBytes(path)
        acc ++ Ints.toByteArray(bytes.length) ++ bytes
      }
    store.insert(Seq(SecretPathsKey -> toInsert))
  }

  def addKey(publicKey: ExtendedPublicKey): Unit = {
    val derivationPath = publicKey.path
    addPath(derivationPath)
  }

  def readPaths: Seq[DerivationPath] = store
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

  def updateStateContext(ctx: ErgoStateContext): Unit = store
    .insert(Seq(StateContextKey -> ctx.bytes))

  def readStateContext: ErgoStateContext = store
    .get(StateContextKey)
    .flatMap(r => ErgoStateContextSerializer(settings.chainSettings.voting).parseBytesTry(r).toOption)
    .getOrElse(ErgoStateContext.empty(ADDigest @@ Array.fill(32)(0: Byte), settings))

  def updateChangeAddress(address: P2PKAddress): Unit = {
    val bytes = addressEncoder.toString(address).getBytes(Constants.StringEncoding)
    store.insert(Seq(ChangeAddressKey -> bytes))
  }

  def readChangeAddress: Option[P2PKAddress] =
    store.get(ChangeAddressKey).flatMap { x =>
      addressEncoder.fromString(new String(x, Constants.StringEncoding)) match {
        case Success(p2pk: P2PKAddress) => Some(p2pk)
        case _ => None
      }
    }

  def addApplication(appReq: ExternalAppRequest): Try[ExternalApplication] = {
    val id = (lastUsedId + 1).toShort
    appReq.toApp(id).flatMap { app =>
      Try(store.insert(Seq(appPrefixKey(id) -> ExternalApplicationSerializer.toBytes(app)))).map(_ => app)
    }
  }

  def removeApplication(id: Short): Unit = store.remove(Seq(appPrefixKey(id)))

  def allApplications: Seq[ExternalApplication] = {
    store.getRange(SmallestPossibleApplicationId, BiggestPossibleApplicationId)
      .map { case (_, v) => ExternalApplicationSerializer.parseBytes(v) }
  }
  def lastUsedId: Short = store.lastKeyInRange(SmallestPossibleApplicationId, BiggestPossibleApplicationId)
    .map(bs => Shorts.fromByteArray(bs.takeRight(2)))
    .getOrElse(PaymentsAppId)
}

object WalletStorage {
  val RangedKeyPrefix = 0: Byte

  val ApplicationPrefixByte = 1: Byte
  val PublicKeyPrefixBye = 2: Byte

  val ApplicationPrefixArray: Array[Byte] = Array(RangedKeyPrefix, ApplicationPrefixByte)

  val SmallestPossibleApplicationId = ApplicationPrefixArray ++ Shorts.toByteArray(0)
  val BiggestPossibleApplicationId = ApplicationPrefixArray ++ Shorts.toByteArray(Short.MaxValue)


  def noPrefixKey(keyString: String): Array[Byte] =
    Blake2b256.hash(keyString)

  def appPrefixKey(appId: Short): Array[Byte] = ApplicationPrefixArray ++ Shorts.toByteArray(appId)

  //following keys do not start with ranged key prefix, i.e. with 8 zero bits
  val StateContextKey: Array[Byte] = noPrefixKey("state_ctx")
  val SecretPathsKey: Array[Byte] = noPrefixKey("secret_paths")
  val ChangeAddressKey: Array[Byte] = noPrefixKey("change_address")

  def readOrCreate(settings: ErgoSettings)
                  (implicit addressEncoder: ErgoAddressEncoder): WalletStorage = {
    new WalletStorage(LDBFactory.createKvDb(s"${settings.directory}/wallet/storage"), settings)
  }

}