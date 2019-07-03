package org.ergoplatform.nodeView.wallet.persistence

import java.io.File

import com.google.common.primitives.Ints
import io.iohk.iodb.{ByteArrayWrapper, LSMStore, Store}
import org.ergoplatform.nodeView.state.{ErgoStateContext, ErgoStateContextSerializer}
import org.ergoplatform.settings.{Constants, ErgoSettings}
import org.ergoplatform.wallet.secrets.{DerivationPath, DerivationPathSerializer}
import org.ergoplatform.{ErgoAddress, ErgoAddressEncoder, P2PKAddress}
import scorex.crypto.authds.ADDigest
import scorex.crypto.hash.Blake2b256

import scala.util.{Random, Success}

/**
  * Persists version-agnostic wallet actor's mutable state.
  */
final class WalletStorage(store: Store, settings: ErgoSettings)
                         (implicit val addressEncoder: ErgoAddressEncoder) {

  import WalletStorage._

  def addTrackedAddresses(addresses: Seq[ErgoAddress]): Unit = {
    val updatedKeys = (readTrackedAddresses ++ addresses).toSet
    val toInsert = Ints.toByteArray(updatedKeys.size) ++ updatedKeys
      .foldLeft(Array.empty[Byte]) { case (acc, address) =>
        val bytes = addressEncoder.toString(address).getBytes(Constants.StringEncoding)
        acc ++ Ints.toByteArray(bytes.length) ++ bytes
      }
    store.update(randomVersion, Seq.empty, Seq(TrackedAddressesKey -> ByteArrayWrapper(toInsert)))
  }

  def addTrackedAddress(address: ErgoAddress): Unit = addTrackedAddresses(Seq(address))

  def readTrackedAddresses: Seq[ErgoAddress] = store
    .get(TrackedAddressesKey)
    .toSeq
    .flatMap { r =>
      val qty = Ints.fromByteArray(r.data.take(4))
      (0 until qty).foldLeft(Seq.empty[ErgoAddress], r.data.drop(4)) { case ((acc, bytes), _) =>
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
    store.update(randomVersion, Seq.empty, Seq(SecretPathsKey -> ByteArrayWrapper(toInsert)))
  }

  def readPaths: Seq[DerivationPath] = store
    .get(SecretPathsKey)
    .toSeq
    .flatMap { r =>
      val qty = Ints.fromByteArray(r.data.take(4))
      (0 until qty).foldLeft(Seq.empty[DerivationPath], r.data.drop(4)) { case ((acc, bytes), _) =>
        val length = Ints.fromByteArray(bytes.take(4))
        val pathTry = DerivationPathSerializer.parseBytesTry(bytes.slice(4, 4 + length))
        pathTry.map(acc :+ _).getOrElse(acc) -> bytes.drop(4 + length)
      }._1
    }

  def updateStateContext(ctx: ErgoStateContext): Unit = store
    .update(randomVersion, Seq.empty, Seq(StateContextKey -> ByteArrayWrapper(ctx.bytes)))

  def readStateContext: ErgoStateContext = store
    .get(StateContextKey)
    .flatMap(r => ErgoStateContextSerializer(settings.chainSettings.voting).parseBytesTry(r.data).toOption)
    .getOrElse(ErgoStateContext.empty(ADDigest @@ Array.fill(32)(0: Byte), settings))

  def putBlock(block: PostponedBlock): Unit = {
    val toInsert = Seq(
      key(block.height) -> ByteArrayWrapper(PostponedBlockSerializer.toBytes(block)),
      LatestPostponedBlockHeightKey -> ByteArrayWrapper(Ints.toByteArray(block.height))
    )
    store.update(randomVersion, Seq.empty, toInsert)
  }

  def readBlocks(fromHeight: Int, toHeight: Int): Seq[PostponedBlock] =
    (fromHeight to toHeight).foldLeft(Seq.empty[PostponedBlock]) { case (acc, h) =>
      acc ++ store.get(key(h)).flatMap(r => PostponedBlockSerializer.parseBytesTry(r.data).toOption)
    }

  def removeBlock(height: Int): Unit =
    store.update(randomVersion, Seq(key(height)), Seq.empty)

  def removeBlocks(fromHeight: Int, toHeight: Int): Unit =
    store.update(randomVersion, (fromHeight to toHeight).map(key), Seq.empty)

  def readLatestPostponedBlockHeight: Option[Int] = store
    .get(LatestPostponedBlockHeightKey)
    .map(r => Ints.fromByteArray(r.data))

  def updateChangeAddress(address: P2PKAddress): Unit = {
    val bytes = addressEncoder.toString(address).getBytes(Constants.StringEncoding)
    store.update(randomVersion, Seq.empty, Seq(ChangeAddressKey -> ByteArrayWrapper(bytes)))
  }

  def readChangeAddress: Option[P2PKAddress] =
    store.get(ChangeAddressKey).flatMap { x =>
      addressEncoder.fromString(new String(x.data, Constants.StringEncoding)) match {
        case Success(p2pk: P2PKAddress) => Some(p2pk)
        case _ => None
      }
    }

  private def randomVersion = Random.nextInt()

}

object WalletStorage {

  val StateContextKey: ByteArrayWrapper =
    ByteArrayWrapper(Blake2b256.hash("state_ctx"))

  val TrackedAddressesKey: ByteArrayWrapper =
    ByteArrayWrapper(Blake2b256.hash("tracked_pks"))

  val SecretPathsKey: ByteArrayWrapper =
    ByteArrayWrapper(Blake2b256.hash("secret_paths"))

  val LatestPostponedBlockHeightKey: ByteArrayWrapper =
    ByteArrayWrapper(Blake2b256.hash("latest_block"))

  val ChangeAddressKey: ByteArrayWrapper =
    ByteArrayWrapper(Blake2b256.hash("change_address"))

  def key(height: Int): ByteArrayWrapper =
    ByteArrayWrapper(Blake2b256.hash(Ints.toByteArray(height)))

  def readOrCreate(settings: ErgoSettings)
                  (implicit addressEncoder: ErgoAddressEncoder): WalletStorage = {
    val dir = new File(s"${settings.directory}/wallet/storage")
    dir.mkdirs()
    new WalletStorage(new LSMStore(dir), settings)
  }

}
