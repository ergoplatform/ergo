package org.ergoplatform.modifiers.history

import io.circe.{Decoder, Encoder, HCursor}
import io.circe.syntax._
import io.circe.{Decoder, Encoder, HCursor}
import org.ergoplatform.api.ApiCodecs
import org.ergoplatform.mining.difficulty.RequiredDifficulty
import org.ergoplatform.mining.{AutolykosSolution, AutolykosSolutionSerializer}
import org.ergoplatform.modifiers.{BlockSection, ErgoPersistentModifier}
import org.ergoplatform.nodeView.history.ErgoHistory
import org.ergoplatform.nodeView.history.ErgoHistory.Difficulty
import org.ergoplatform.settings.{Algos, Constants}
import scorex.core.ModifierTypeId
import scorex.core.block.Block._
import scorex.core.serialization.ScorexSerializer
import scorex.core.utils.NetworkTimeProvider
import scorex.crypto.authds.ADDigest
import scorex.crypto.hash.Digest32
import scorex.util._
import scorex.util.serialization.{Reader, VLQByteBufferWriter, Writer}

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
                  powSolution: AutolykosSolution,
                  override val sizeOpt: Option[Int] = None
                 ) extends ErgoPersistentModifier {

  override def serializedId: Array[Version] = Algos.hash(HeaderSerializer.toBytes(this))

  override val modifierTypeId: ModifierTypeId = Header.modifierTypeId

  lazy val requiredDifficulty: Difficulty = RequiredDifficulty.decodeCompactBits(nBits)

  lazy val ADProofsId: ModifierId = BlockSection.computeId(ADProofs.modifierTypeId, id, ADProofsRoot)

  lazy val transactionsId: ModifierId = BlockSection.computeId(BlockTransactions.modifierTypeId, id, transactionsRoot)

  lazy val extensionId: ModifierId = BlockSection.computeId(Extension.modifierTypeId, id, extensionRoot)

  lazy val sectionIds: Seq[(ModifierTypeId, ModifierId)] = Seq((ADProofs.modifierTypeId, ADProofsId),
    (BlockTransactions.modifierTypeId, transactionsId), (Extension.modifierTypeId, extensionId))

  override lazy val toString: String = s"Header(${this.asJson.noSpaces})"

  lazy val isGenesis: Boolean = height == ErgoHistory.GenesisHeight

  lazy val size = sizeOpt.getOrElse(HeaderSerializer.toBytes(this).length)

  /**
    * Checks, that modifier m corresponds t this header
    */
  def isCorrespondingModifier(m: ErgoPersistentModifier): Boolean = sectionIds.exists(_._2 == m.id)

  /**
    * Estimate that this Header is new enough to possibly be the best header
    */
  def isNew(timeProvider: NetworkTimeProvider, timeDiff: FiniteDuration): Boolean = {
    timeProvider.time() - timestamp < timeDiff.toMillis
  }

}

object Header extends ApiCodecs {

  val CurrentVersion: Byte = 1

  val modifierTypeId: ModifierTypeId = ModifierTypeId @@ (101: Byte)

  lazy val GenesisParentId: ModifierId = bytesToId(Array.fill(Constants.HashLength)(0: Byte))

  implicit val jsonEncoder: Encoder[Header] = { h: Header =>
    Map(
      "id" -> Algos.encode(h.id).asJson,
      "transactionsRoot" -> Algos.encode(h.transactionsRoot).asJson,
      "interlinks" -> h.interlinks.map(i => Algos.encode(i).asJson).asJson,
      "adProofsRoot" -> Algos.encode(h.ADProofsRoot).asJson,
      "stateRoot" -> Algos.encode(h.stateRoot).asJson,
      "parentId" -> Algos.encode(h.parentId).asJson,
      "timestamp" -> h.timestamp.asJson,
      "extensionHash" -> Algos.encode(h.extensionRoot).asJson,
      "powSolutions" -> h.powSolution.asJson,
      "nBits" -> h.nBits.asJson,
      "height" -> h.height.asJson,
      "difficulty" -> h.requiredDifficulty.toString.asJson,
      "version" -> h.version.asJson,
      "size" -> h.size.asJson
    ).asJson
  }

  implicit val jsonDecoder: Decoder[Header] = { c: HCursor =>
    for {
      transactionsRoot <- c.downField("transactionsRoot").as[Digest32]
      interlinks <- c.downField("interlinks").as[List[ModifierId]]
      adProofsRoot <- c.downField("adProofsRoot").as[Digest32]
      stateRoot <- c.downField("stateRoot").as[ADDigest]
      parentId <- c.downField("parentId").as[ModifierId]
      timestamp <- c.downField("timestamp").as[Long]
      extensionHash <- c.downField("extensionHash").as[Digest32]
      nBits <- c.downField("nBits").as[Long]
      height <- c.downField("height").as[Int]
      version <- c.downField("version").as[Byte]
      solutions <- c.downField("powSolutions").as[AutolykosSolution]
    } yield Header(version, parentId, interlinks, adProofsRoot, stateRoot,
      transactionsRoot, timestamp, nBits, height, extensionHash, solutions)
  }
}

object HeaderSerializer extends ScorexSerializer[Header] {

  override def serialize(h: Header, w: Writer): Unit = {
    serializeWithoutPow(h, w)
    serializeSolution(h, w)
  }

  def serializeWithoutInterlinksAndPow(h: Header, w: Writer): Unit = {
    w.put(h.version)
    w.putBytes(idToBytes(h.parentId))
    w.putBytes(h.ADProofsRoot)
    w.putBytes(h.transactionsRoot)
    w.putBytes(h.stateRoot)
    w.putLong(h.timestamp)
    w.putBytes(h.extensionRoot)
    RequiredDifficulty.serialize(h.nBits)
    w.putInt(h.height)
  }

  def serializeWithoutInterlinks(h: Header, w: Writer): Unit = {
    serializeWithoutInterlinksAndPow(h, w)
    w.putUShort(0)
    serializeSolution(h, w)
  }

  def serializeWithoutPow(h: Header, w: Writer): Unit = {

    @tailrec
    def serializeInterlink(links: Seq[ModifierId]): Unit= {
      links match {
        case Nil =>
        case headLink :: _ =>
          val repeating = links.count(_ == headLink)
          w.putUByte(repeating)
          w.putBytes(idToBytes(headLink))
          serializeInterlink(links.drop(repeating))
      }
    }

    serializeWithoutInterlinksAndPow(h, w)

    w.putUShort(h.interlinks.size)
    serializeInterlink(h.interlinks)
  }

  def bytesWithoutPow(header: Header): Array[Version] = {
    val w = new VLQByteBufferWriter(new ByteArrayBuilder())
    serializeWithoutPow(header, w)
    w.result().toBytes
  }

  def serializeSolution(h: Header, w: Writer): Unit = {
    AutolykosSolutionSerializer.serialize(h.powSolution, w)
  }

  override def parse(r: Reader): Header = {
    val version = r.getByte()
    val parentId = bytesToId(r.getBytes(32))
    val ADProofsRoot = Digest32 @@ r.getBytes(32)
    val transactionsRoot = Digest32 @@ r.getBytes(32)
    val stateRoot = ADDigest @@ r.getBytes(32)
    val timestamp = r.getLong()
    val extensionHash = Digest32 @@ r.getBytes(32)
    val nBits = RequiredDifficulty.parse(r)
    val height = r.getInt()

    val interlinksSize = r.getUShort()

    @tailrec
    def parseInterlinks(acc: Seq[ModifierId]): Seq[ModifierId] = {
      if (acc.length < interlinksSize) {
        val repeatN = r.getUByte()
        require(repeatN > 0)
        val link: ModifierId = bytesToId(r.getBytes(Constants.ModifierIdSize))
        val links: Seq[ModifierId] = Array.fill(repeatN)(link)
        parseInterlinks(acc ++ links)
      } else {
        acc
      }
    }

    val interlinks = parseInterlinks(Seq.empty)

    val powSolution = AutolykosSolutionSerializer.parse(r)

    Header(version, parentId, interlinks, ADProofsRoot, stateRoot, transactionsRoot, timestamp,
        nBits, height, extensionHash, powSolution, Some(r.consumed))
  }
}
