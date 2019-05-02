package org.ergoplatform.nodeView.wallet.persistence

import io.iohk.iodb.{ByteArrayWrapper, Store}
import org.ergoplatform.{ErgoAddressEncoder, P2PKAddress}
import scorex.crypto.hash.Blake2b256
import sigmastate.basics.DLogProtocol.ProveDlog
import sigmastate.interpreter.CryptoConstants

import scala.util.Random

/**
  * Persists wallet actor's mutable state.
  */
final class WalletStorage(store: Store)(implicit val addressEncoder: ErgoAddressEncoder) {

  import WalletStorage._

  def addTrackedKeys(keys: Seq[P2PKAddress]): Unit = {
    val keys = store
      .get(TrackedPubKeysKey)
      .map { r => (r.data.grouped(PubKeyLength).map(x => P2PKAddress(key(x))) ++ keys).toSet }
      .getOrElse(Set.empty)
    val toInsert = TrackedPubKeysKey -> ByteArrayWrapper(keys.toArray.flatMap(_.pubkeyBytes))
    store.update(randomVersion, Seq.empty, Seq(toInsert))
  }

  def readTrackedKeys: Seq[P2PKAddress] = store
    .get(TrackedPubKeysKey)
    .flatMap { _.data.grouped(PubKeyLength).map(x => P2PKAddress(key(x))) }
    .toSeq

  private def key(keyBytes: Array[Byte]): ProveDlog = ProveDlog(
    CryptoConstants.dlogGroup.curve.decodePoint(keyBytes).asInstanceOf[CryptoConstants.EcPointType]
  )

  private def randomVersion = Random.nextInt()

}

object WalletStorage {

  val PubKeyLength: Int = 32

  val StateContextKey: ByteArrayWrapper =
    ByteArrayWrapper(Blake2b256.hash("state_ctx"))

  val TrackedPubKeysKey: ByteArrayWrapper =
    ByteArrayWrapper(Blake2b256.hash("tracked_pks"))

}
