package org.ergoplatform.nodeView.wallet.persistence

import com.google.common.primitives.{Ints, Longs}
import org.ergoplatform.db.{LDBFactory, LDBKVStore}
import org.ergoplatform.nodeView.state.{ErgoStateContext, ErgoStateContextSerializer}
import org.ergoplatform.nodeView.wallet.scanning.{ExternalAppRequest, ExternalApplication, ExternalApplicationSerializer}
import org.ergoplatform.settings.{Constants, ErgoSettings}
import org.ergoplatform.wallet.secrets.{DerivationPath, DerivationPathSerializer}
import org.ergoplatform.{ErgoAddress, ErgoAddressEncoder, P2PKAddress}
import scorex.crypto.authds.ADDigest
import scorex.crypto.hash.Blake2b256

import scala.util.{Success, Try}

/**
  * Persists version-agnostic wallet actor's mutable state:
  *   * tracked addresses
  *   * derivation paths
  *   * changed addresses
  *   * postponed blocks
  *   * ErgoStateContext (is it version-agnostic?)
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
      val qty = Ints.fromByteArray(r.take(4))
      (0 until qty).foldLeft(Seq.empty[DerivationPath], r.drop(4)) { case ((acc, bytes), _) =>
        val length = Ints.fromByteArray(bytes.take(4))
        val pathTry = DerivationPathSerializer.parseBytesTry(bytes.slice(4, 4 + length))
        pathTry.map(acc :+ _).getOrElse(acc) -> bytes.drop(4 + length)
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
      heightPrefixKey(block.height) -> PostponedBlockSerializer.toBytes(block),
      LatestPostponedBlockHeightKey -> Ints.toByteArray(block.height)
    )
    store.insert(toInsert)
  }

  def readBlocks(fromHeight: Int, toHeight: Int): Seq[PostponedBlock] =
    (fromHeight to toHeight).foldLeft(Seq.empty[PostponedBlock]) { case (acc, h) =>
      acc ++ store.get(heightPrefixKey(h)).flatMap(r => PostponedBlockSerializer.parseBytesTry(r).toOption)
    }

  def removeBlock(height: Int): Unit =
    store.remove(Seq(heightPrefixKey(height)))

  def removeBlocks(fromHeight: Int, toHeight: Int): Unit =
    store.remove((fromHeight to toHeight).map(heightPrefixKey))

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

  def addApplication(appReq: ExternalAppRequest): Try[ExternalApplication] = {
    val id = lastUsedId + 1
    appReq.toApp(id).flatMap { app =>
      Try(store.insert(Seq(appPrefixKey(id) -> ExternalApplicationSerializer.toBytes(app)))).map(_ => app)
    }
  }

  def removeApplication(id: Long): Unit = store.remove(Seq(appPrefixKey(id)))

  def allApplications: Seq[ExternalApplication] = {
    store.getRange(SmallestPossibleApplicationId, BiggestPossibleApplicationId)
      .map { case (_, v) => ExternalApplicationSerializer.parseBytes(v) }
  }
  def lastUsedId: Long = store.lastKeyInRange(SmallestPossibleApplicationId, BiggestPossibleApplicationId)
    .map(bs => Longs.fromByteArray(bs))
    .getOrElse(Constants.DefaultAppId)
}

object WalletStorage {
  val ZeroCount = 16
  val ZeroArray = Array.fill(ZeroCount)(0: Byte)
  val AppendixCount = 32 - ZeroCount

  val HeightPrefixByte = 1: Byte
  val ApplicationPrefixByte = 2: Byte

  val HeightPrefixArray = (Array.fill(ZeroCount - 1)(0: Byte) :+ HeightPrefixByte) ++ Array.fill(12)(0: Byte)
  val ApplicationPrefixArray = (Array.fill(ZeroCount - 1)(0: Byte) :+ ApplicationPrefixByte) ++ Array.fill(8)(0: Byte)

  val SmallestPossibleApplicationId = ApplicationPrefixArray ++ Longs.toByteArray(0)
  val BiggestPossibleApplicationId = ApplicationPrefixArray ++ Longs.toByteArray(Long.MaxValue)


  def generalPrefixKey(keyString: String) = ZeroArray ++ Blake2b256.hash(keyString).takeRight(AppendixCount)

  def heightPrefixKey(height: Int) = HeightPrefixArray ++ Ints.toByteArray(height)

  def appPrefixKey(appId: Long) = ApplicationPrefixArray ++ Longs.toByteArray(appId)

  val StateContextKey: Array[Byte] = generalPrefixKey("state_ctx")

  val TrackedAddressesKey: Array[Byte] = generalPrefixKey("tracked_pks")

  val SecretPathsKey: Array[Byte] = generalPrefixKey("secret_paths")

  val LatestPostponedBlockHeightKey: Array[Byte] = generalPrefixKey("latest_block")

  val ChangeAddressKey: Array[Byte] = generalPrefixKey("change_address")

  def readOrCreate(settings: ErgoSettings)
                  (implicit addressEncoder: ErgoAddressEncoder): WalletStorage = {
    new WalletStorage(LDBFactory.createKvDb(s"${settings.directory}/wallet/storage"), settings)
  }

}
