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
import org.ergoplatform.settings.{Algos, Constants}
import scorex.core.{ModifierId, ModifierTypeId}
import scorex.core.block.Block._
import scorex.core.serialization.Serializer
import scorex.crypto.authds.ADDigest
import scorex.crypto.hash.Digest32

import scala.annotation.tailrec
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
                  votes: Array[Byte],
                  nonce: Long,
                  equihashSolutions: Array[Byte]
                 ) extends ErgoPersistentModifier {

  override val modifierTypeId: ModifierTypeId = Header.modifierTypeId

  override lazy val id: ModifierId = ModifierId @@ powHash

  //todo: why SHA256?
  //todo: tolsi: check this
  lazy val powHash: Digest32 = {
    // H(I||V||x_1||x_2||...|x_2^k)
    val digest = new SHA256Digest()
    val bytes = HeaderSerializer.bytesWithoutPow(this)
    digest.update(bytes, 0, bytes.length)
    Equihash.hashNonce(digest, nonce)
    EquihashSolutionsSerializer.parseBytes(equihashSolutions).get.foreach(s => Equihash.hashXi(digest, s))
    val h = new Array[Byte](32)
    digest.doFinal(h, 0)

    val secondDigest = new SHA256Digest()
    secondDigest.update(h, 0, h.length)
    val result = new Array[Byte](32)
    secondDigest.doFinal(result, 0)

    Digest32 @@ result
  }

  lazy val requiredDifficulty: Difficulty = RequiredDifficulty.decodeCompactBits(nBits)

  lazy val ADProofsId: ModifierId = ModifierWithDigest.computeId(ADProofs.modifierTypeId, id, ADProofsRoot)

  lazy val transactionsId: ModifierId =
    ModifierWithDigest.computeId(BlockTransactions.modifierTypeId, id, transactionsRoot)

  override lazy val json: Json = Map(
    "id" -> Algos.encode(id).asJson,
    "transactionsRoot" -> Algos.encode(transactionsRoot).asJson,
    "interlinks" -> interlinks.map(i => Algos.encode(i).asJson).asJson,
    "ADProofsRoot" -> Algos.encode(ADProofsRoot).asJson,
    "stateRoot" -> Algos.encode(stateRoot).asJson,
    "parentId" -> Algos.encode(parentId).asJson,
    "timestamp" -> timestamp.asJson,
    "nonce" -> nonce.asJson,
    "equihashSolutions" -> Algos.encode(equihashSolutions).asJson,
    "nBits" -> nBits.asJson,
    "height" -> height.asJson,
    "difficulty" -> requiredDifficulty.toString.asJson,
    "votes" -> Algos.encode(votes).asJson
  ).asJson

  override lazy val toString: String = s"Header(${json.noSpaces})"

  override type M = Header

  override lazy val serializer: Serializer[Header] = HeaderSerializer

  lazy val isGenesis: Boolean = height == ErgoHistory.GenesisHeight
}

object Header {
  val modifierTypeId: ModifierTypeId = ModifierTypeId @@ (101: Byte)

  lazy val GenesisParentId: ModifierId = ModifierId @@ Array.fill(Constants.hashLength)(0: Byte)
}


object HeaderSerializer extends Serializer[Header] {

  def bytesWithoutInterlinksAndPow(h: Header): Array[Byte] =
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

  def bytesWithoutPow(h: Header): Array[Byte] = {
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

    Bytes.concat(bytesWithoutInterlinksAndPow(h), interlinkBytesSize, interlinkBytes)
  }

  def nonceAndSolutionBytes(h:Header): Array[Byte] = {
    val equihashSolutionsSize = Chars.toByteArray(h.equihashSolutions.length.toChar)
    val equihashSolutionsBytes = h.equihashSolutions

    Bytes.concat(Longs.toByteArray(h.nonce), equihashSolutionsSize, equihashSolutionsBytes)
  }

  def bytesWithoutInterlinks(h: Header): Array[Byte] =
    Bytes.concat(
      bytesWithoutInterlinksAndPow(h),
      Chars.toByteArray(0),
      nonceAndSolutionBytes(h)
    )

  override def toBytes(h: Header): Array[Version] =
    Bytes.concat(bytesWithoutPow(h), nonceAndSolutionBytes(h))

  override def parseBytes(bytes: Array[Version]): Try[Header] = Try {
    val version = bytes.head
    val parentId = ModifierId @@ bytes.slice(1, 33)
    val ADProofsRoot = Digest32 @@ bytes.slice(33, 65)
    val transactionsRoot = Digest32 @@ bytes.slice(65, 97)
    val stateRoot = ADDigest @@ bytes.slice(97, 130)
    val timestamp = Longs.fromByteArray(bytes.slice(130, 138))
    val votes = bytes.slice(138, 143)
    val nBits = RequiredDifficulty.parseBytes(bytes.slice(143, 147)).get
    val height = Ints.fromByteArray(bytes.slice(147, 151))

    @tailrec
    def parseInterlinks(index: Int, endIndex: Int, acc: Seq[ModifierId]): Seq[ModifierId] = if (endIndex > index) {
      val repeatN: Int = bytes.slice(index, index + 1).head
      val link: ModifierId = ModifierId @@ bytes.slice(index + 1, index + 33)
      val links: Seq[ModifierId] = Array.fill(repeatN)(link)
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
