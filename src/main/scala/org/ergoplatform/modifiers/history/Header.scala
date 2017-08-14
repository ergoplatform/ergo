package org.ergoplatform.modifiers.history

import com.google.common.primitives.{Bytes, Longs}
import io.circe.Json
import io.circe.syntax._
import org.ergoplatform.modifiers.{ErgoPersistentModifier, ModifierWithDigest}
import org.ergoplatform.nodeView.state.ErgoState.Digest
import org.ergoplatform.settings.Algos
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
                  nonce: Long,
                  extensionHash: Array[Byte],
                  votes: Array[Byte]) extends ErgoPersistentModifier {

  override val modifierTypeId: ModifierTypeId = Header.ModifierTypeId

  override lazy val id: ModifierId = Algos.hash(bytes)

  lazy val ADProofsId: ModifierId = ModifierWithDigest.computeId(ADProof.ModifierTypeId, id, ADProofsRoot)

  lazy val transactionsId: ModifierId =
    ModifierWithDigest.computeId(BlockTransactions.ModifierTypeId, id, transactionsRoot)

  lazy val headerHash: Digest = Algos.miningHash(id)

  lazy val realDifficulty: BigInt = Algos.blockIdDifficulty(id)

  override lazy val json: Json = Map(
    "id" -> Base58.encode(id).asJson,
    "transactionsRoot" -> Base58.encode(transactionsRoot).asJson,
    "interlinks" -> interlinks.map(i => Base58.encode(i).asJson).asJson,
    "ADProofsRoot" -> Base58.encode(ADProofsRoot).asJson,
    "stateRoot" -> Base58.encode(stateRoot).asJson,
    "parentId" -> Base58.encode(parentId).asJson,
    "timestamp" -> timestamp.asJson,
    "nonce" -> nonce.asJson,
    "extensionHash" -> Base58.encode(extensionHash).asJson,
    "votes" -> Base58.encode(votes).asJson
  ).asJson

  override lazy val toString: String = s"Header(${json.noSpaces})"

  override type M = Header

  override lazy val serializer: Serializer[Header] = HeaderSerializer

  lazy val isGenesis: Boolean = interlinks.isEmpty
}

object Header {
  val ModifierTypeId: Byte = 101: Byte
}


object HeaderSerializer extends Serializer[Header] {
  val BytesWithoutInterlinksLength = 108

  def bytesWithoutInterlinks(h: Header): Array[Byte] = Bytes.concat(Array(h.version), h.parentId, h.ADProofsRoot,
    h.transactionsRoot, h.stateRoot, Longs.toByteArray(h.timestamp), Longs.toByteArray(h.nonce), h.extensionHash,
    h.votes)

  override def toBytes(h: Header): Array[Version] = {

    def interlinkBytes(links: Seq[Array[Byte]], acc: Array[Byte]): Array[Byte] = {
      if (links.isEmpty) {
        acc
      } else {
        val headLink: Array[Byte] = links.head
        val repeating: Byte = links.count(_ sameElements headLink).toByte
        interlinkBytes(links.drop(repeating), Bytes.concat(acc, Array(repeating), headLink))
      }
    }
    Bytes.concat(bytesWithoutInterlinks(h), interlinkBytes(h.interlinks, Array[Byte]()))
  }

  override def parseBytes(bytes: Array[Version]): Try[Header] = Try {
    val version = bytes.head
    val parentId = bytes.slice(1, 33)
    val ADProofsRoot = bytes.slice(33, 65)
    val transactionsRoot = bytes.slice(65, 97)
    val stateRoot = bytes.slice(97, 129)
    val timestamp = Longs.fromByteArray(bytes.slice(129, 137))
    val nonce = Longs.fromByteArray(bytes.slice(137, 145))
    val extensionHash = bytes.slice(145, 177)
    val votes = bytes.slice(177, 182)

    @tailrec
    def parseInnterlinks(index: Int, acc: Seq[Array[Byte]]): Seq[Array[Byte]] = if (bytes.length > index) {
      val repeatN: Int = bytes.slice(index, index + 1).head
      val link: Array[Byte] = bytes.slice(index + 1, index + 33)
      val links: Seq[Array[Byte]] = Array.fill(repeatN)(link)
      parseInnterlinks(index + 33, acc ++ links)
    } else {
      acc
    }

    val innerlinks = parseInnterlinks(182, Seq())

    Header(version, parentId, innerlinks, ADProofsRoot, stateRoot, transactionsRoot, timestamp, nonce, extensionHash, votes)
  }
}
