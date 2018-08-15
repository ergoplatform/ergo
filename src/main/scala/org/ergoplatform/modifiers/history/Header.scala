package org.ergoplatform.modifiers.history

import com.google.common.primitives._
import io.circe.Encoder
import io.circe.syntax._
import org.bouncycastle.crypto.digests.SHA256Digest
import org.ergoplatform.crypto.Equihash
import org.ergoplatform.mining.EquihashSolution
import org.ergoplatform.mining.difficulty.RequiredDifficulty
import org.ergoplatform.modifiers.{BlockSection, ErgoPersistentModifier}
import org.ergoplatform.nodeView.history.ErgoHistory
import org.ergoplatform.nodeView.history.ErgoHistory.Difficulty
import org.ergoplatform.settings.{Algos, Constants}
import scorex.core._
import scorex.core.block.Block._
import scorex.core.serialization.Serializer
import scorex.core.utils.NetworkTimeProvider
import scorex.crypto.authds.ADDigest
import scorex.crypto.hash.Digest32

import scala.annotation.tailrec
import scala.concurrent.duration.FiniteDuration
import scala.util.Try

case class Header(version: Version,
                  override val parentId: ModifierId,
                  interlinks: Seq[ModifierId],
                  ADProofsRoot: Digest32,
                  stateRoot: ADDigest, //33 bytes! extra byte with tree height here!
                  transactionsRoot: Digest32,
                  timestamp: Timestamp,
                  nBits: Long, //actually it is unsigned int
                  height: Int,
                  extensionRoot: Digest32,
                  equihashSolution: EquihashSolution,
                  override val sizeOpt: Option[Int] = None
                 ) extends ErgoPersistentModifier {


  override type M = Header

  override val modifierTypeId: ModifierTypeId = Header.modifierTypeId

  override lazy val id: ModifierId = bytesToId(serializedId)

  lazy val serializedId: Digest32 = {
    // An implementation of a PoW function which is similar to one used in ZCash.
    // H(I||V||x_1||x_2||...|x_2^k)
    val digest = new SHA256Digest()
    val bytes = HeaderSerializer.bytesWithoutPow(this)
    digest.update(bytes, 0, bytes.length)
    Equihash.hashSolution(digest, equihashSolution)
    val h = new Array[Byte](32)
    digest.doFinal(h, 0)

    val secondDigest = new SHA256Digest()
    secondDigest.update(h, 0, h.length)
    val result = new Array[Byte](32)
    secondDigest.doFinal(result, 0)

    Digest32 @@ result
  }

  lazy val requiredDifficulty: Difficulty = RequiredDifficulty.decodeCompactBits(nBits)

  lazy val ADProofsId: ModifierId = BlockSection.computeId(ADProofs.modifierTypeId, id, ADProofsRoot)

  lazy val transactionsId: ModifierId = BlockSection.computeId(BlockTransactions.modifierTypeId, id, transactionsRoot)

  lazy val extensionId: ModifierId = BlockSection.computeId(Extension.modifierTypeId, id, extensionRoot)

  lazy val sectionIds: Seq[(ModifierTypeId, ModifierId)] = Seq((ADProofs.modifierTypeId, ADProofsId),
    (BlockTransactions.modifierTypeId, transactionsId), (Extension.modifierTypeId, extensionId))

  override lazy val toString: String = s"Header(${this.asJson.noSpaces})"

  override lazy val serializer: Serializer[Header] = HeaderSerializer

  lazy val isGenesis: Boolean = height == ErgoHistory.GenesisHeight

  /**
    * Checks, that modifier m corresponds t this header
    */
  def isCorrespondingModifier(m: ErgoPersistentModifier): Boolean =sectionIds.exists(_._2 == m.id)

  /**
    * Estimate that this Header is new enough to possibly be the best header
    */
  def isNew(timeProvider: NetworkTimeProvider, timeDiff: FiniteDuration): Boolean = {
    timeProvider.time() - timestamp < timeDiff.toMillis
  }

}

object Header {

  val CurrentVersion: Byte = 1

  val modifierTypeId: ModifierTypeId = ModifierTypeId @@ (101: Byte)

  lazy val GenesisParentId: ModifierId = bytesToId(Array.fill(Constants.hashLength)(0: Byte))

  implicit val jsonEncoder: Encoder[Header] = (h: Header) =>
    Map(
      "id" -> Algos.encode(h.id).asJson,
      "transactionsRoot" -> Algos.encode(h.transactionsRoot).asJson,
      "interlinks" -> h.interlinks.map(i => Algos.encode(i).asJson).asJson,
      "adProofsRoot" -> Algos.encode(h.ADProofsRoot).asJson,
      "stateRoot" -> Algos.encode(h.stateRoot).asJson,
      "parentId" -> Algos.encode(h.parentId).asJson,
      "timestamp" -> h.timestamp.asJson,
      "extensionHash" -> Algos.encode(h.extensionRoot).asJson,
      "equihashSolutions" -> Algos.encode(h.equihashSolution.bytes).asJson,
      "nBits" -> h.nBits.asJson,
      "height" -> h.height.asJson,
      "difficulty" -> h.requiredDifficulty.toString.asJson,
      "version" -> h.version.asJson,
      "size" -> h.size.asJson
    ).asJson
}

object HeaderSerializer extends Serializer[Header] {

  def bytesWithoutInterlinksAndPow(h: Header): Array[Byte] =
    Bytes.concat(
      Array(h.version),
      idToBytes(h.parentId),
      h.ADProofsRoot,
      h.transactionsRoot,
      h.stateRoot,
      Longs.toByteArray(h.timestamp),
      h.extensionRoot,
      RequiredDifficulty.toBytes(h.nBits),
      Ints.toByteArray(h.height))

  def bytesWithoutPow(h: Header): Array[Byte] = {
    @SuppressWarnings(Array("TraversableHead"))
    def buildInterlinkBytes(links: Seq[ModifierId], acc: Array[Byte]): Array[Byte] = {
      if (links.isEmpty) {
        acc
      } else {
        val headLink: ModifierId = links.head
        val repeating: Byte = links.count(_ == headLink).toByte
        buildInterlinkBytes(links.drop(repeating), Bytes.concat(acc, Array(repeating), idToBytes(headLink)))
      }
    }

    val interlinkBytes = buildInterlinkBytes(h.interlinks, Array[Byte]())
    val interlinkBytesSize = Chars.toByteArray(interlinkBytes.length.toChar)

    Bytes.concat(bytesWithoutInterlinksAndPow(h), interlinkBytesSize, interlinkBytes)
  }

  def solutionBytes(h: Header): Array[Byte] = {
    val equihashSolutionsSize = Chars.toByteArray(h.equihashSolution.byteLength.toChar)
    val equihashSolutionsBytes = h.equihashSolution.bytes

    Bytes.concat(equihashSolutionsSize, equihashSolutionsBytes)
  }

  def bytesWithoutInterlinks(h: Header): Array[Byte] =
    Bytes.concat(
      bytesWithoutInterlinksAndPow(h),
      Chars.toByteArray(0),
      solutionBytes(h)
    )

  override def toBytes(h: Header): Array[Version] =
    Bytes.concat(bytesWithoutPow(h), solutionBytes(h))

  @SuppressWarnings(Array("TryGet"))
  override def parseBytes(bytes: Array[Version]): Try[Header] = Try {
    val version = bytes.head
    val parentId = bytesToId(bytes.slice(1, 33))
    val ADProofsRoot = Digest32 @@ bytes.slice(33, 65)
    val transactionsRoot = Digest32 @@ bytes.slice(65, 97)
    val stateRoot = ADDigest @@ bytes.slice(97, 130)
    val timestamp = Longs.fromByteArray(bytes.slice(130, 138))
    val extensionHash = Digest32 @@ bytes.slice(138, 170)
    val nBits = RequiredDifficulty.parseBytes(bytes.slice(170, 174)).get
    val height = Ints.fromByteArray(bytes.slice(174, 178))

    @tailrec
    def parseInterlinks(index: Int, endIndex: Int, acc: Seq[ModifierId]): Seq[ModifierId] = if (endIndex > index) {
      val repeatN: Int = bytes.slice(index, index + 1).head
      val link: ModifierId = bytesToId(bytes.slice(index + 1, index + 33))
      val links: Seq[ModifierId] = Array.fill(repeatN)(link)
      parseInterlinks(index + 33, endIndex, acc ++ links)
    } else {
      acc
    }

    val interlinksSize = Chars.fromByteArray(bytes.slice(178, 180))
    val interlinks = parseInterlinks(180, 180 + interlinksSize, Seq.empty)

    val equihashSolutionsBytesSize = Chars.fromByteArray(bytes.slice(180 + interlinksSize, 182 + interlinksSize))
    val equihashSolutionsBytes = bytes.slice(182 + interlinksSize, 182 + interlinksSize + equihashSolutionsBytesSize)

    EquihashSolutionsSerializer.parseBytes(equihashSolutionsBytes) map { equihashSolution =>
      Header(version, parentId, interlinks, ADProofsRoot, stateRoot, transactionsRoot, timestamp,
        nBits, height, extensionHash, equihashSolution, Some(bytes.length))
    }
  }.flatten
}
