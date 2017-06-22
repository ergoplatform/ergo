package org.ergoplatform.modifiers.block

import com.google.common.primitives.{Bytes, Ints, Longs}
import io.circe.Json
import io.circe.syntax._
import org.ergoplatform.modifiers.transaction.AnyoneCanSpendTransaction
import org.ergoplatform.settings.{Algos, Constants}
import scorex.core.NodeViewModifier.{ModifierId, ModifierTypeId}
import scorex.core.block.Block
import scorex.core.block.Block._
import scorex.core.serialization.Serializer
import scorex.crypto.encode.Base58

import scala.annotation.tailrec
import scala.util.Try

case class ErgoHeader(version: Version,
                      parentId: BlockId,
                      interlinks: Seq[Array[Byte]],
                      stateRoot: Array[Byte],
                      transactionsRoot: Array[Byte],
                      timestamp: Block.Timestamp,
                      nonce: Int) extends ErgoBlock {
  override def transactions: Option[Seq[AnyoneCanSpendTransaction]] = None

  override val modifierTypeId: ModifierTypeId = ErgoHeader.ModifierTypeId

  override lazy val id: ModifierId = Constants.hash(bytes)

  lazy val realDifficulty: BigInt = Algos.blockIdDifficulty(id)

  override def json: Json = Map(
    "id" -> Base58.encode(id).asJson,
    "innerchainLinks" -> interlinks.map(l => Base58.encode(l).asJson).asJson,
    "transactionsRoot" -> Base58.encode(transactionsRoot).asJson,
    "stateRoot" -> Base58.encode(stateRoot).asJson,
    "parentId" -> Base58.encode(parentId).asJson,
    "timestamp" -> timestamp.asJson,
    "nonce" -> nonce.asJson
  ).asJson


  override def toString: String = s"Header(${json.noSpaces})"

  override type M = ErgoHeader

  override def serializer: Serializer[ErgoHeader] = ErgoHeaderSerializer

  override def equals(obj: scala.Any): Boolean = obj match {
    case that: ErgoHeader => id sameElements that.id
    case _ => false
  }
}

object ErgoHeader {
  val ModifierTypeId = 10: Byte
}

object ErgoHeaderSerializer extends Serializer[ErgoHeader] {
  override def toBytes(h: ErgoHeader): Array[Version] = {
    val BytesWithoutInterlinksLength = 108

    def bytesWithoutInterlinks(h: ErgoHeader): Array[Byte] = {
      Bytes.concat(h.parentId, h.transactionsRoot, h.stateRoot, Longs.toByteArray(h.timestamp), Ints.toByteArray(h.nonce))
    }

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

  override def parseBytes(bytes: Array[Version]): Try[ErgoHeader] = Try {
    val version = bytes.head
    val parentId = bytes.slice(1, 33)
    val transactionsRoot = bytes.slice(33, 65)
    val stateRoot = bytes.slice(65, 97)
    val timestamp = Longs.fromByteArray(bytes.slice(97, 105))
    val nonce = Ints.fromByteArray(bytes.slice(105, 109))
    @tailrec
    def parseInnerchainLinks(index: Int, acc: Seq[Array[Byte]]): Seq[Array[Byte]] = if (bytes.length > index) {
      val repeatN: Int = bytes.slice(index, index + 1).head
      val link: Array[Byte] = bytes.slice(index + 1, index + 33)
      val links: Seq[Array[Byte]] = Array.fill(repeatN)(link)
      parseInnerchainLinks(index + 33, acc ++ links)
    } else {
      acc
    }
    val innerchainLinks = parseInnerchainLinks(109, Seq())

    ErgoHeader(version, parentId, innerchainLinks, stateRoot, transactionsRoot, timestamp, nonce)
  }
}

