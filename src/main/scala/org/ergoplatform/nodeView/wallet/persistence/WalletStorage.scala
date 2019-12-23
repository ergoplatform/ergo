package org.ergoplatform.nodeView.wallet.persistence

import java.io.File

import com.google.common.primitives.Ints
import org.ergoplatform.nodeView.state.{ErgoStateContext, ErgoStateContextSerializer}
import org.ergoplatform.settings.{Constants, ErgoSettings}
import org.ergoplatform.wallet.secrets.{DerivationPath, DerivationPathSerializer}
import org.ergoplatform.{ErgoAddress, ErgoAddressEncoder, P2PKAddress}
import org.iq80.leveldb.Options
import scorex.db.LDBFactory.factory
import scorex.crypto.authds.ADDigest
import scorex.crypto.hash.{Blake2b256, Digest32}
import scorex.db.LDBKVStore

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
    store.insert(Seq(TrackedAddressesKey -> toInsert))
  }

  def addTrackedAddress(address: ErgoAddress): Unit = addTrackedAddresses(Seq(address))

  def readTrackedAddresses: Seq[ErgoAddress] = store
    .get(TrackedAddressesKey)
    .toSeq
    .flatMap { r =>
      val qty = Ints.fromByteArray(r.take(4))
      (0 until qty).foldLeft(Seq.empty[ErgoAddress], r.drop(4)) { case ((acc, bytes), _) =>
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
    store.insert(Seq(SecretPathsKey -> toInsert))
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

  def putBlock(block: PostponedBlock): Unit = {
    val toInsert = Seq(
      key(block.height) -> PostponedBlockSerializer.toBytes(block),
      LatestPostponedBlockHeightKey -> Ints.toByteArray(block.height)
    )
    store.insert(toInsert)
  }

  def readBlocks(fromHeight: Int, toHeight: Int): Seq[PostponedBlock] =
    (fromHeight to toHeight).foldLeft(Seq.empty[PostponedBlock]) { case (acc, h) =>
      acc ++ store.get(key(h)).flatMap(r => PostponedBlockSerializer.parseBytesTry(r).toOption)
    }

  def removeBlock(height: Int): Unit =
    store.remove(Seq(key(height)))

  def removeBlocks(fromHeight: Int, toHeight: Int): Unit =
    store.remove((fromHeight to toHeight).map(key))

  def readLatestPostponedBlockHeight: Option[Int] = store
    .get(LatestPostponedBlockHeightKey)
    .map(r => Ints.fromByteArray(r))

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

}

object WalletStorage {

  val StateContextKey: Array[Byte] =
    Blake2b256.hash("state_ctx")

  val TrackedAddressesKey: Array[Byte] =
    Blake2b256.hash("tracked_pks")

  val SecretPathsKey: Array[Byte] =
    Blake2b256.hash("secret_paths")

  val LatestPostponedBlockHeightKey: Array[Byte] =
    Blake2b256.hash("latest_block")

  val ChangeAddressKey: Array[Byte] =
    Blake2b256.hash("change_address")

  def key(height: Int): Digest32 =
    Blake2b256.hash(Ints.toByteArray(height))

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
