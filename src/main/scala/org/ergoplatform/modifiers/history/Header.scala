package org.ergoplatform.modifiers.history

import com.google.common.primitives._
import io.circe.Json
import io.circe.syntax._
import org.bouncycastle.crypto.digests.SHA256Digest
import org.ergoplatform.crypto.Equihash
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
import scala.math.BigInt
import scala.util.Try

case class Header(version: Version,
                  override val parentId: BlockId,
                  interlinks: Seq[Array[Byte]],
                  ADProofsRoot: Digest,
                  stateRoot: Array[Byte], //33 bytes! extra byte with tree height here!
                  transactionsRoot: Digest,
                  timestamp: Timestamp,
                  nBits: Long, //actually it is unsigned int
                  height: Int,
                  votes: Array[Byte],
                  nonce: Long,
                  equihashSolutions: Array[Byte]
                 ) extends ErgoPersistentModifier {

  override val modifierTypeId: ModifierTypeId = Header.ModifierTypeId

  override lazy val id: ModifierId = Algos.hash(bytes)

  lazy val powHash: Digest = {
    // H(I||V||x_1||x_2||...|x_2^k)
    val digest = new SHA256Digest()
    val bytes = HeaderSerializer.bytesWithoutNonceAndSolutions(this)
    digest.update(bytes, 0, bytes.length)
    Equihash.hashNonce(digest, nonce)
    EquihashSolutionsSerializer.parseBytes(equihashSolutions).get.foreach(s => Equihash.hashXi(digest, s))
    val h = new Array[Byte](32)
    digest.doFinal(h, 0)
    val secondDigest = new SHA256Digest()
    secondDigest.update(h, 0, h.length)
    val result = new Array[Byte](32)
    secondDigest.doFinal(result, 0)
    result
  }

  lazy val realDifficulty: Difficulty = Constants.MaxTarget / BigInt(1, powHash)

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

  lazy val isGenesis: Boolean = height == ErgoHistory.GenesisHeight
}

object Header {
  val ModifierTypeId: Byte = 101: Byte

  lazy val GenesisParentId: Digest = Array.fill(Constants.hashLength)(0: Byte)
}


object HeaderSerializer extends Serializer[Header] {

  def bytesWithoutInterlinksAndNonceAndSolutions(h: Header): Array[Byte] =
    Bytes.concat(
      Array(h.version),
      h.parentId,
      h.ADProofsRoot,
      h.transactionsRoot,
      h.stateRoot,
      Longs.toByteArray(h.timestamp),
      h.votes,
      RequiredDifficulty.toBytes(h.nBits),
      Ints.toByteArray(h.height))

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
    val interlinkBytesSize = Chars.toByteArray(interlinkBytes.length.toChar)

    Bytes.concat(bytesWithoutInterlinksAndNonceAndSolutions(h), interlinkBytesSize, interlinkBytes)
  }

  override def toBytes(h: Header): Array[Version] = {
    val equihashSolutionsSize = Chars.toByteArray(h.equihashSolutions.length.toChar)
    val equihashSolutionsBytes = h.equihashSolutions
    Bytes.concat(bytesWithoutNonceAndSolutions(h),
      Longs.toByteArray(h.nonce),
      equihashSolutionsSize, equihashSolutionsBytes
    )
  }

  override def parseBytes(bytes: Array[Version]): Try[Header] = Try {
    val version = bytes.head
    val parentId = bytes.slice(1, 33)
    val ADProofsRoot = bytes.slice(33, 65)
    val transactionsRoot = bytes.slice(65, 97)
    val stateRoot = bytes.slice(97, 130)
    val timestamp = Longs.fromByteArray(bytes.slice(130, 138))
    val votes = bytes.slice(138, 143)
    val nBits = RequiredDifficulty.parseBytes(bytes.slice(143, 147)).get
    val height = Ints.fromByteArray(bytes.slice(147, 151))

    @tailrec
    def parseInterlinks(index: Int, endIndex: Int, acc: Seq[Array[Byte]]): Seq[Array[Byte]] = if (endIndex > index) {
      val repeatN: Int = bytes.slice(index, index + 1).head
      val link: Array[Byte] = bytes.slice(index + 1, index + 33)
      val links: Seq[Array[Byte]] = Array.fill(repeatN)(link)
      parseInterlinks(index + 33, endIndex, acc ++ links)
    } else {
      acc
    }

    val interlinksSize = Chars.fromByteArray(bytes.slice(151, 153))
    val interlinks = parseInterlinks(153, 153 + interlinksSize, Seq())

    val nonce = Longs.fromByteArray(bytes.slice(153 + interlinksSize, 161 + interlinksSize))

    val equihashSolutionsBytesSize = Chars.fromByteArray(bytes.slice(161 + interlinksSize, 163 + interlinksSize))
    val equihashSolutions = bytes.slice(163 + interlinksSize, 163 + interlinksSize + equihashSolutionsBytesSize)

    Header(version, parentId, interlinks, ADProofsRoot, stateRoot, transactionsRoot, timestamp,
      nBits, height, votes, nonce, equihashSolutions)
  }
}
