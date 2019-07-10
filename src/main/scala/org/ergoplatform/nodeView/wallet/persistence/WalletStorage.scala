package org.ergoplatform.nodeView.wallet.persistence

import java.io.File

import akka.util.ByteString
import com.google.common.primitives.Ints
import org.ergoplatform.db.LDBFactory.factory
import org.ergoplatform.db.LDBKVStore
import org.ergoplatform.nodeView.state.{ErgoStateContext, ErgoStateContextSerializer}
import org.ergoplatform.settings.{Constants, ErgoSettings}
import org.ergoplatform.wallet.secrets.{DerivationPath, DerivationPathSerializer}
import org.ergoplatform.{ErgoAddress, ErgoAddressEncoder, P2PKAddress}
import org.iq80.leveldb.Options
import scorex.crypto.authds.ADDigest
import scorex.crypto.hash.Blake2b256

import scala.util.Success

/**
  * Persists version-agnostic wallet actor's mutable state.
  */
final class WalletStorage(store: LDBKVStore, settings: ErgoSettings)
                         (implicit val addressEncoder: ErgoAddressEncoder) {

  import WalletStorage._

  def addTrackedAddresses(addresses: Seq[ErgoAddress]): Unit = {
    val updatedKeys = (readTrackedAddresses ++ addresses).toSet
    val toInsert = Ints.toByteArray(updatedKeys.size) ++ updatedKeys
      .foldLeft(Array.empty[Byte]) { case (acc, address) =>
        val bytes = addressEncoder.toString(address).getBytes(Constants.StringEncoding)
        acc ++ Ints.toByteArray(bytes.length) ++ bytes
      }
    store.insert(Seq(TrackedAddressesKey -> ByteString(toInsert)))
  }

  def addTrackedAddress(address: ErgoAddress): Unit = addTrackedAddresses(Seq(address))

  def readTrackedAddresses: Seq[ErgoAddress] = store
    .get(TrackedAddressesKey)
    .toSeq
    .flatMap { r =>
      val qty = Ints.fromByteArray(r.take(4).toArray)
      (0 until qty).foldLeft(Seq.empty[ErgoAddress], r.drop(4).toArray) { case ((acc, bytes), _) =>
        val length = Ints.fromByteArray(bytes.take(4))
        val addressTry = addressEncoder.fromString(new String(bytes.slice(4, 4 + length), Constants.StringEncoding))
        addressTry.map(acc :+ _).getOrElse(acc) -> bytes.drop(4 + length)
      }._1
    }

  def addPath(derivationPath: DerivationPath): Unit = {
    val updatedPaths = (readPaths :+ derivationPath).toSet
    val toInsert = Ints.toByteArray(updatedPaths.size) ++ updatedPaths
      .foldLeft(Array.empty[Byte]) { case (acc, path) =>
        val bytes = DerivationPathSerializer.toBytes(path)
        acc ++ Ints.toByteArray(bytes.length) ++ bytes
      }
    store.insert(Seq(SecretPathsKey -> ByteString(toInsert)))
  }

  def readPaths: Seq[DerivationPath] = store
    .get(SecretPathsKey)
    .toSeq
    .flatMap { r =>
      val qty = Ints.fromByteArray(r.take(4).toArray)
      (0 until qty).foldLeft(Seq.empty[DerivationPath], r.drop(4).toArray) { case ((acc, bytes), _) =>
        val length = Ints.fromByteArray(bytes.take(4))
        val pathTry = DerivationPathSerializer.parseBytesTry(bytes.slice(4, 4 + length))
        pathTry.map(acc :+ _).getOrElse(acc) -> bytes.drop(4 + length)
      }._1
    }

  def updateStateContext(ctx: ErgoStateContext): Unit = store
    .insert(Seq(StateContextKey -> ByteString(ctx.bytes)))

  def readStateContext: ErgoStateContext = store
    .get(StateContextKey)
    .flatMap(r => ErgoStateContextSerializer(settings.chainSettings.voting).parseBytesTry(r.toArray).toOption)
    .getOrElse(ErgoStateContext.empty(ADDigest @@ Array.fill(32)(0: Byte), settings))

  def putBlock(block: PostponedBlock): Unit = {
    val toInsert = Seq(
      key(block.height) -> ByteString(PostponedBlockSerializer.toBytes(block)),
      LatestPostponedBlockHeightKey -> ByteString(Ints.toByteArray(block.height))
    )
    store.insert(toInsert)
  }

  def readBlocks(fromHeight: Int, toHeight: Int): Seq[PostponedBlock] =
    (fromHeight to toHeight).foldLeft(Seq.empty[PostponedBlock]) { case (acc, h) =>
      acc ++ store.get(key(h)).flatMap(r => PostponedBlockSerializer.parseBytesTry(r.toArray).toOption)
    }

  def removeBlock(height: Int): Unit =
    store.remove(Seq(key(height)))

  def removeBlocks(fromHeight: Int, toHeight: Int): Unit =
    store.remove((fromHeight to toHeight).map(key))

  def readLatestPostponedBlockHeight: Option[Int] = store
    .get(LatestPostponedBlockHeightKey)
    .map(r => Ints.fromByteArray(r.toArray))

  def updateChangeAddress(address: P2PKAddress): Unit = {
    val bytes = addressEncoder.toString(address).getBytes(Constants.StringEncoding)
    store.insert(Seq(ChangeAddressKey -> ByteString(bytes)))
  }

  def readChangeAddress: Option[P2PKAddress] =
    store.get(ChangeAddressKey).flatMap { x =>
      addressEncoder.fromString(new String(x.toArray, Constants.StringEncoding)) match {
        case Success(p2pk: P2PKAddress) => Some(p2pk)
        case _ => None
      }
    }

}

object WalletStorage {

  val StateContextKey: ByteString =
    ByteString(Blake2b256.hash("state_ctx"))

  val TrackedAddressesKey: ByteString =
    ByteString(Blake2b256.hash("tracked_pks"))

  val SecretPathsKey: ByteString =
    ByteString(Blake2b256.hash("secret_paths"))

  val LatestPostponedBlockHeightKey: ByteString =
    ByteString(Blake2b256.hash("latest_block"))

  val ChangeAddressKey: ByteString =
    ByteString(Blake2b256.hash("change_address"))

  def key(height: Int): ByteString =
    ByteString(Blake2b256.hash(Ints.toByteArray(height)))

  def readOrCreate(settings: ErgoSettings)
                  (implicit addressEncoder: ErgoAddressEncoder): WalletStorage = {
    val dir = new File(s"${settings.directory}/wallet/storage")
    dir.mkdirs()

    val options = new Options()
    options.createIfMissing(true)
    val db = factory.open(dir, options)

    new WalletStorage(new LDBKVStore(db), settings)
  }

}
