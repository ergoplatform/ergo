package org.ergoplatform.modifiers.history

import com.google.common.primitives.{Bytes, Ints, Longs, Shorts}
import io.circe.Json
import io.circe.syntax._
import org.ergoplatform.mining.difficulty.RequiredDifficulty
import org.ergoplatform.modifiers.{ErgoPersistentModifier, ModifierWithDigest}
import org.ergoplatform.nodeView.history.ErgoHistory.Difficulty
import org.ergoplatform.nodeView.state.ErgoState.Digest
import org.ergoplatform.settings.Algos
import scorex.core.NodeViewModifier.{ModifierId, ModifierTypeId}
import scorex.core.block.Block._
import scorex.core.serialization.Serializer
import scorex.crypto.encode.Base58

import scala.annotation.tailrec
import scala.util.Try

case class Header(version: Version,
                  override val parentId: BlockId,
                  interlinks: Seq[Array[Byte]],
                  ADProofsRoot: Array[Byte],
                  stateRoot: Array[Byte],
                  transactionsRoot: Array[Byte],
                  timestamp: Timestamp,
                  nBits: Long, //actually it is unsigned int
                  height: Int,
                  votes: Array[Byte],
                  nonce: Long,
                  equihashSolutions: Array[Byte],
                 ) extends ErgoPersistentModifier {

  override val modifierTypeId: ModifierTypeId = Header.ModifierTypeId

  override lazy val id: ModifierId = Algos.hash(bytes)

  lazy val powHash: Digest = Algos.miningHash(id)

  lazy val realDifficulty: Difficulty = Algos.blockIdDifficulty(id)

  lazy val requiredDifficulty: Difficulty = RequiredDifficulty.decodeCompactBits(nBits)

  lazy val ADProofsId: ModifierId = ModifierWithDigest.computeId(ADProof.ModifierTypeId, id, ADProofsRoot)

  lazy val transactionsId: ModifierId =
    ModifierWithDigest.computeId(BlockTransactions.ModifierTypeId, id, transactionsRoot)

  override lazy val json: Json = Map(
    "id" -> Base58.encode(id).asJson,
    "transactionsRoot" -> Base58.encode(transactionsRoot).asJson,
    "interlinks" -> interlinks.map(i => Base58.encode(i).asJson).asJson,
    "ADProofsRoot" -> Base58.encode(ADProofsRoot).asJson,
    "stateRoot" -> Base58.encode(stateRoot).asJson,
    "parentId" -> Base58.encode(parentId).asJson,
    "timestamp" -> timestamp.asJson,
    "nonce" -> nonce.asJson,
    "equihashSolutions" -> equihashSolutions.asJson,
    "nBits" -> nBits.asJson,
    "height" -> height.asJson,
    "votes" -> Base58.encode(votes).asJson
  ).asJson

  override lazy val toString: String = s"Header(${json.noSpaces})"

  override type M = Header

  override lazy val serializer: Serializer[Header] = HeaderSerializer

  lazy val isGenesis: Boolean = height == 0
}

object Header {
  val ModifierTypeId: Byte = 101: Byte
}


object HeaderSerializer extends Serializer[Header] {

  def bytesWithoutInterlinksAndNonceAndSolutions(h: Header): Array[Byte] = Bytes.concat(Array(h.version), h.parentId, h.ADProofsRoot,
    h.transactionsRoot, h.stateRoot, Longs.toByteArray(h.timestamp), h.votes,
    RequiredDifficulty.toBytes(h.nBits), Ints.toByteArray(h.height))

  def bytesWithoutNonceAndSolutions(h: Header): Array[Byte] = {
    def buildInterlinkBytes(links: Seq[Array[Byte]], acc: Array[Byte]): Array[Byte] = {
      if (links.isEmpty) {
        acc
      } else {
        val headLink: Array[Byte] = links.head
        val repeating: Byte = links.count(_ sameElements headLink).toByte
        buildInterlinkBytes(links.drop(repeating), Bytes.concat(acc, Array(repeating), headLink))
      }
    }
    val interlinkBytes = buildInterlinkBytes(h.interlinks, Array[Byte]())
    val interlinkBytesSize = Shorts.toByteArray(interlinkBytes.length.toShort)

    Bytes.concat(bytesWithoutInterlinksAndNonceAndSolutions(h), interlinkBytesSize, interlinkBytes)
  }

  def bytesWithoutSolutions(h: Header): Array[Byte] = {
    Bytes.concat(bytesWithoutNonceAndSolutions(h), Longs.toByteArray(h.nonce))
  }

  override def toBytes(h: Header): Array[Version] = {
    val equihashSolutionsSize = Shorts.toByteArray(h.equihashSolutions.length.toShort)
    val equihashSolutionsBytes = h.equihashSolutions
    Bytes.concat(bytesWithoutSolutions(h),
      equihashSolutionsSize, equihashSolutionsBytes
    )
  }

  override def parseBytes(bytes: Array[Version]): Try[Header] = Try {
    val version = bytes.head
    val parentId = bytes.slice(1, 33)
    val ADProofsRoot = bytes.slice(33, 65)
    val transactionsRoot = bytes.slice(65, 97)
    val stateRoot = bytes.slice(97, 129)
    val timestamp = Longs.fromByteArray(bytes.slice(129, 137))
    val votes = bytes.slice(137, 142)
    val nBits = RequiredDifficulty.parseBytes(bytes.slice(142, 146)).get
    val height = Ints.fromByteArray(bytes.slice(146, 150))

    @tailrec
    def parseInterlinks(index: Int, endIndex: Int, acc: Seq[Array[Byte]]): Seq[Array[Byte]] = if (endIndex > index) {
      val repeatN: Int = bytes.slice(index, index + 1).head
      val link: Array[Byte] = bytes.slice(index + 1, index + 33)
      val links: Seq[Array[Byte]] = Array.fill(repeatN)(link)
      parseInterlinks(index + 33, endIndex, acc ++ links)
    } else {
      acc
    }

    val interlinksSize = Shorts.fromByteArray(bytes.slice(150, 152))
    val interlinks = parseInterlinks(152, 152 + interlinksSize, Seq())

    val nonce = Longs.fromByteArray(bytes.slice(152 + interlinksSize, 160 + interlinksSize))

    val equihashSolutionsBytesSize = Shorts.fromByteArray(bytes.slice(160 + interlinksSize, 162 + interlinksSize))
    val equihashSolutions = bytes.slice(162 + interlinksSize, 162 + interlinksSize + equihashSolutionsBytesSize)

    Header(version, parentId, interlinks, ADProofsRoot, stateRoot, transactionsRoot, timestamp,
      nBits, height, votes, nonce, equihashSolutions)
  }
}
