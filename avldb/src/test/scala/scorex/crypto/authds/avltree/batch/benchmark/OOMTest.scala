package scorex.crypto.authds.avltree.batch.benchmark

import java.io.File
import java.nio.file.Files

import com.google.common.primitives.{Longs, Shorts}
import io.iohk.iodb.ByteArrayWrapper
import scorex.crypto.authds.{ADDigest, ADKey, ADValue}
import scorex.crypto.authds.avltree.batch._
import scorex.util.encode.Base16
import scorex.crypto.hash.{Blake2b256, Digest32}
import scorex.db.LDBVersionedStore
import scala.collection.immutable.SortedMap

object OOMTest extends App {

  type Box = (ADKey, ADValue)

  type HF = Blake2b256.type
  protected implicit val hf = Blake2b256

  val dir: File = Files.createTempDirectory("oom-test").toFile
  val store = new LDBVersionedStore(dir, keepVersions = 200)

  val bestVersionKey = Blake2b256("best state version")
  private lazy val np = NodeParameters(keySize = 32, valueSize = None, labelSize = 32)
  protected lazy val storage = new VersionedIODBAVLStorage(store, np)

  val afterGenesisStateDigestHex: String = "78b130095239561ecf5449a7794c0615326d1fd007cc79dcc286e46e4beb1d3f01"
  val afterGenesisStateDigest: ADDigest = ADDigest @@ Base16.decode(afterGenesisStateDigestHex).get
  val genesisStateVersion = Blake2b256(afterGenesisStateDigest.tail)

  val coinsInOneErgo: Long = 100000000
  val blocksPerHour: Int = 30
  private val blocksPerYear: Int = 365 * 24 * blocksPerHour
  val blocksTotal: Int = blocksPerYear * 8
  val fixedRate = 2250 * coinsInOneErgo / blocksPerHour
  val rewardReductionPeriod: Int = 90 * 24 * blocksPerHour
  val fixedRatePeriod = 2 * blocksPerYear - rewardReductionPeriod
  val decreasingEpochs = (blocksTotal - fixedRatePeriod) / rewardReductionPeriod

  val GeCode: Byte = 34
  val HeightCode: Byte = 51
  val LastFuncType: Byte = 255.toByte
  val LastDataType: Byte = 111
  val ConstantCode: Byte = (LastFuncType - LastDataType + 1).toByte
  val IntTypeCode: Byte = 11
  val IntCode: Byte = (ConstantCode + IntTypeCode).toByte

  def emissionAtHeight(h: Long): Long = {
    if (h <= fixedRatePeriod) {
      fixedRate
    } else if (h > blocksTotal) {
      0
    } else {
      val epoch: Int = ((h - fixedRatePeriod) / rewardReductionPeriod).toInt
      fixedRate - fixedRate * epoch / decreasingEpochs
    }
  }.ensuring(_ >= 0, s"Negative at $h")

  def rewardBoxAtHeight(h: Long): Box = {
    val value = emissionAtHeight(h)
    val propBytes: Array[Byte] = GeCode +: HeightCode +: IntCode +: Longs.toByteArray(h)
    val transactionId: Array[Byte] = Array.fill(32)(0: Byte)
    val boxId: Short = 0
    val bytes: Array[Byte] =
      Longs.toByteArray(value) ++ propBytes ++ (0.toByte +: Array.emptyByteArray) ++
        transactionId ++ Shorts.toByteArray(boxId)
    val id = Blake2b256.hash(bytes)
    ADKey @@ id -> ADValue @@ bytes
  }

  private def metadata(modId: Array[Byte], stateRoot: ADDigest): Seq[(Array[Byte], Array[Byte])] = {
    val idStateDigestIdxElem: (Array[Byte], Array[Byte]) = modId -> stateRoot
    val stateDigestIdIdxElem = Blake2b256(stateRoot) -> modId
    val bestVersion = bestVersionKey -> modId

    Seq(idStateDigestIdxElem, stateDigestIdIdxElem, bestVersion)
  }

  val boxes: SortedMap[ByteArrayWrapper, Box] = {
    val keyValues = (0 until blocksTotal).map { h =>
      if (h % 10000 == 0) println(h)
      val b = rewardBoxAtHeight(h)
      ByteArrayWrapper(b._1) -> b
    }
    SortedMap(keyValues: _*)
  }

  def sortedBoxes: Set[Box] = boxes.keySet.map(k => boxes(k))

  val p = new BatchAVLProver[Digest32, HF](keyLength = 32, valueLengthOpt = None)
  sortedBoxes.foreach(b => p.performOneOperation(Insert(b._1, b._2)).ensuring(_.isSuccess))

  val persistentProver =
    PersistentBatchAVLProver.create(
      p,
      storage,
      metadata(genesisStateVersion, p.digest),
      paranoidChecks = true
    ).get

}