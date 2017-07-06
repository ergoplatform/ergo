package org.ergoplatform.modifiers.history

import com.google.common.primitives.{Bytes, Ints, Longs}
import io.circe.Json
import io.circe.syntax._
import org.ergoplatform.settings.{Algos, Constants}
import scorex.core.NodeViewModifier.{ModifierId, ModifierTypeId}
import scorex.core.block.Block
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
                  timestamp: Block.Timestamp,
                  nonce: Int) extends HistoryModifier {

  lazy val payloadRootHash: Array[Byte] = Algos.merkleTreeRoot(Seq(Array(version),
    Algos.hash(scorex.core.utils.concatFixLengthBytes(interlinks)),
    ADProofsRoot,
    stateRoot,
    transactionsRoot,
    Longs.toByteArray(timestamp),
    parentId
  ))

  lazy val minimalHeader = MinimalHeader(payloadRootHash, nonce)

  override val modifierTypeId: ModifierTypeId = Header.ModifierTypeId

  override lazy val id: ModifierId = minimalHeader.id

  lazy val realDifficulty: BigInt = Algos.blockIdDifficulty(id)

  override lazy val json: Json = Map(
    "id" -> Base58.encode(id).asJson,
    "transactionsRoot" -> Base58.encode(transactionsRoot).asJson,
//    "interlinksRoot" -> Base58.encode(interlinksRoot).asJson,
    "ADProofsRoot" -> Base58.encode(ADProofsRoot).asJson,
    "stateRoot" -> Base58.encode(stateRoot).asJson,
    "parentId" -> Base58.encode(parentId).asJson,
    "timestamp" -> timestamp.asJson,
    "nonce" -> nonce.asJson
  ).asJson

  override lazy val toString: String = s"Header(${json.noSpaces})"

  override type M = Header

  override lazy val serializer: Serializer[Header] = HeaderSerializer

  override def equals(obj: scala.Any): Boolean = obj match {
    case that: Header => id sameElements that.id
    case _ => false
  }

  lazy val isGenesis: Boolean = interlinks.isEmpty
}

object Header {
  val ModifierTypeId: Byte = 101: Byte

}


object HeaderSerializer extends Serializer[Header] {
  override def toBytes(h: Header): Array[Version] = {
    val BytesWithoutInterlinksLength = 108

    def bytesWithoutInterlinks(h: Header): Array[Byte] = Bytes.concat(h.parentId, h.ADProofsRoot, h.transactionsRoot,
      h.stateRoot, Longs.toByteArray(h.timestamp), Ints.toByteArray(h.nonce))


    def interlinkBytes(links: Seq[Array[Byte]], acc: Array[Byte]): Array[Byte] = {
      if (links.isEmpty) {
        acc
      } else {
        val headLink: Array[Byte] = links.head
        val repeating: Byte = links.count(_ sameElements headLink).toByte
        interlinkBytes(links.drop(repeating), Bytes.concat(acc, Array(repeating), headLink))
      }
    }
    Bytes.concat(Array(h.version), bytesWithoutInterlinks(h), interlinkBytes(h.interlinks, Array[Byte]()))
  }

  override def parseBytes(bytes: Array[Version]): Try[Header] = Try {
    val version = bytes.head
    val parentId = bytes.slice(1, 33)
    val ADProofsRoot = bytes.slice(33, 65)
    val transactionsRoot = bytes.slice(65, 97)
    val stateRoot = bytes.slice(97, 129)
    val timestamp = Longs.fromByteArray(bytes.slice(129, 137))
    val nonce = Ints.fromByteArray(bytes.slice(137, 141))

    @tailrec
    def parseInnterlinks(index: Int, acc: Seq[Array[Byte]]): Seq[Array[Byte]] = if (bytes.length > index) {
      val repeatN: Int = bytes.slice(index, index + 1).head
      val link: Array[Byte] = bytes.slice(index + 1, index + 33)
      val links: Seq[Array[Byte]] = Array.fill(repeatN)(link)
      parseInnterlinks(index + 33, acc ++ links)
    } else {
      acc
    }

    val innerlinks = parseInnterlinks(141, Seq())

    Header(version, parentId, innerlinks, ADProofsRoot, stateRoot, transactionsRoot, timestamp, nonce)
  }
}
