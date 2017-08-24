package org.ergoplatform.modifiers.history

import com.google.common.primitives.{Bytes, Ints, Longs}
import io.circe.Json
import io.circe.syntax._
import org.ergoplatform.mining.difficulty.RequiredDifficulty
import org.ergoplatform.modifiers.{ErgoPersistentModifier, ModifierWithDigest}
import org.ergoplatform.nodeView.history.ErgoHistory
import org.ergoplatform.nodeView.history.ErgoHistory.Difficulty
import org.ergoplatform.nodeView.state.ErgoState.Digest
import org.ergoplatform.settings.{Algos, Constants}
import scorex.core.NodeViewModifier.{ModifierId, ModifierTypeId}
import scorex.core.block.Block._
import scorex.core.serialization.Serializer
import scorex.crypto.encode.Base58

import scala.annotation.tailrec
import scala.util.Try

case class Header(version: Version,
                  override val parentId: BlockId,
                  interlinks: Seq[Array[Byte]],
                  ADProofsRoot: Digest,
                  stateRoot: Digest,
                  transactionsRoot: Digest,
                  timestamp: Timestamp,
                  nonce: Long,
                  nBits: Long, //actually it is unsigned int
                  height: Int,
                  votes: Array[Byte]) extends ErgoPersistentModifier {

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
    "nBits" -> nBits.asJson,
    "height" -> height.asJson,
    "votes" -> Base58.encode(votes).asJson
  ).asJson

  override lazy val toString: String = s"Header(${json.noSpaces})"

  override type M = Header

  override lazy val serializer: Serializer[Header] = HeaderSerializer

  lazy val isGenesis: Boolean = height == ErgoHistory.GenesisHeight
}

object Header {
  val ModifierTypeId: Byte = 101: Byte

  lazy val GenesisParentId: Digest = Array.fill(Constants.hashLength)(0: Byte)
}


object HeaderSerializer extends Serializer[Header] {

  def bytesWithoutInterlinks(h: Header): Array[Byte] = Bytes.concat(Array(h.version), h.parentId, h.ADProofsRoot,
    h.transactionsRoot, h.stateRoot, Longs.toByteArray(h.timestamp), Longs.toByteArray(h.nonce), h.votes,
    RequiredDifficulty.toBytes(h.nBits), Ints.toByteArray(h.height))

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
    val votes = bytes.slice(145, 150)
    val nBits = RequiredDifficulty.parseBytes(bytes.slice(150, 154)).get
    val height = Ints.fromByteArray(bytes.slice(154, 158))

    @tailrec
    def parseInterlinks(index: Int, acc: Seq[Array[Byte]]): Seq[Array[Byte]] = if (bytes.length > index) {
      val repeatN: Int = bytes.slice(index, index + 1).head
      val link: Array[Byte] = bytes.slice(index + 1, index + 33)
      val links: Seq[Array[Byte]] = Array.fill(repeatN)(link)
      parseInterlinks(index + 33, acc ++ links)
    } else {
      acc
    }

    val interlinks = parseInterlinks(158, Seq())


    Header(version, parentId, interlinks, ADProofsRoot, stateRoot, transactionsRoot, timestamp, nonce,
      nBits, height, votes)
  }
}
