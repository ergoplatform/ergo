package org.ergoplatform.modifiers.history

import io.circe.syntax._
import io.circe.{Decoder, Encoder, HCursor}
import org.ergoplatform.http.api.ApiCodecs
import org.ergoplatform.mining.difficulty.RequiredDifficulty
import org.ergoplatform.mining.{AutolykosSolution, AutolykosSolutionSerializer}
import org.ergoplatform.modifiers.{BlockSection, ErgoPersistentModifier}
import org.ergoplatform.nodeView.history.ErgoHistory
import org.ergoplatform.nodeView.history.ErgoHistory.Difficulty
import org.ergoplatform.settings.{Algos, Constants}
import org.ergoplatform.wallet.interpreter.ErgoInterpreter
import scorex.core.{ModifierTypeId, idToBytes}
import scorex.core.block.Block._
import scorex.core.serialization.ScorexSerializer
import scorex.core.utils.NetworkTimeProvider
import scorex.crypto.authds.ADDigest
import scorex.crypto.hash.Digest32
import scorex.util._
import sigmastate.interpreter.CryptoConstants.EcPointType
import scorex.util.serialization.{Reader, VLQByteBufferWriter, Writer}

import scala.concurrent.duration.FiniteDuration
import scorex.util.Extensions._
import sigmastate.eval.{CAvlTree, CBigInt, CGroupElement, CHeader}
import sigmastate.eval.Extensions._

/**
  * Header without proof-of-work puzzle solution, see Header class description for details.
  */
class HeaderWithoutPow(val version: Version, // 1 byte
                       val parentId: ModifierId, // 32 bytes
                       val ADProofsRoot: Digest32, // 32 bytes
                       val stateRoot: ADDigest, //33 bytes! extra byte with tree height here!
                       val transactionsRoot: Digest32, // 32 bytes
                       val timestamp: Timestamp,
                       val nBits: Long, //actually it is unsigned int
                       val height: Int,
                       val extensionRoot: Digest32,
                       val votes: Array[Byte]) { //3 bytes
  def toHeader(powSolution: AutolykosSolution, headerSize: Option[Int] = None): Header =
    Header(version, parentId, ADProofsRoot, stateRoot, transactionsRoot, timestamp,
      nBits, height, extensionRoot, powSolution, votes, headerSize)
}

object HeaderWithoutPow {

  def apply(version: Version, parentId: ModifierId, ADProofsRoot: Digest32, stateRoot: ADDigest,
            transactionsRoot: Digest32, timestamp: Timestamp, nBits: Long, height: Int,
            extensionRoot: Digest32, votes: Array[Byte]): HeaderWithoutPow = {
    new HeaderWithoutPow(version, parentId, ADProofsRoot, stateRoot, transactionsRoot, timestamp,
      nBits, height, extensionRoot, votes)
  }

}

/**
  * Header of a block. It authenticates link to a previous block, other block sections
  * (transactions, UTXO set transformation proofs, extension), UTXO set, votes for parameters
  * to be changed and proof-of-work related data.
  *
  * @param version - protocol version
  * @param parentId - id of a parent block header
  * @param ADProofsRoot - digest of UTXO set transformation proofs
  * @param stateRoot - AVL+ tree digest of UTXO set (after the block)
  * @param transactionsRoot - Merkle tree digest of transactions in the block (BlockTransactions section)
  * @param timestamp - block generation time reported by a miner
  * @param nBits - difficulty encoded
  * @param height - height of the block (genesis block height == 1)
  * @param extensionRoot - Merkle tree digest of the extension section of the block
  * @param powSolution - solution for the proof-of-work puzzle
  * @param votes - votes for changing system parameters
  * @param sizeOpt - optionally, size of the header (to avoid serialization on calling .length)
  */
case class Header(override val version: Version,
                  override val parentId: ModifierId,
                  override val ADProofsRoot: Digest32,
                  override val stateRoot: ADDigest, //33 bytes! extra byte with tree height here!
                  override val transactionsRoot: Digest32,
                  override val timestamp: Timestamp,
                  override val nBits: Long, //actually it is unsigned int
                  override val height: Int,
                  override val extensionRoot: Digest32,
                  powSolution: AutolykosSolution,
                  override val votes: Array[Byte], //3 bytes
                  override val sizeOpt: Option[Int] = None) extends HeaderWithoutPow(version, parentId, ADProofsRoot, stateRoot, transactionsRoot, timestamp,
  nBits, height, extensionRoot, votes) with PreHeader with ErgoPersistentModifier {

  override def serializedId: Array[Version] = Algos.hash(bytes)

  override type M = Header

  override val modifierTypeId: ModifierTypeId = Header.modifierTypeId

  lazy val requiredDifficulty: Difficulty = RequiredDifficulty.decodeCompactBits(nBits)

  lazy val ADProofsId: ModifierId = BlockSection.computeId(ADProofs.modifierTypeId, id, ADProofsRoot)

  lazy val transactionsId: ModifierId = BlockSection.computeId(BlockTransactions.modifierTypeId, id, transactionsRoot)

  lazy val extensionId: ModifierId = BlockSection.computeId(Extension.modifierTypeId, id, extensionRoot)

  override def minerPk: EcPointType = powSolution.pk

  /**
    * Expected identifiers of the block sections
    */
  lazy val sectionIds: Seq[(ModifierTypeId, ModifierId)] = Seq(
    (ADProofs.modifierTypeId, ADProofsId),
    (BlockTransactions.modifierTypeId, transactionsId),
    (Extension.modifierTypeId, extensionId))

  override lazy val toString: String = s"Header(${this.asJson.noSpaces})"

  override lazy val serializer: ScorexSerializer[Header] = HeaderSerializer

  lazy val isGenesis: Boolean = height == ErgoHistory.GenesisHeight

  /**
    * Checks that modifier m corresponds to this header
    */
  def isCorrespondingModifier(m: ErgoPersistentModifier): Boolean = sectionIds.exists(_._2 == m.id)

  /**
    * Estimate that this header is recent enough to possibly be the best header
    */
  def isNew(timeProvider: NetworkTimeProvider, timeDiff: FiniteDuration): Boolean = {
    timeProvider.time() - timestamp < timeDiff.toMillis
  }

  /**
    * New voting epoch starts
    */
  def votingStarts(votingEpochLength: Int): Boolean = height % votingEpochLength == 0 && height > 0

}

/**
  * A fake header that is used to fill the chain that starts from the beginning
  */
object PreGenesisHeader extends Header(
  0.toByte,
  parentId = Header.GenesisParentId,
  ADProofsRoot = null,
  stateRoot = null,
  transactionsRoot = null,
  timestamp = 0L,
  nBits = 0L,
  height = ErgoHistory.EmptyHistoryHeight,
  extensionRoot = null,
  powSolution = null,
  votes = null,
  sizeOpt = None) {

  override def serializedId: Array[Byte] = idToBytes(Header.GenesisParentId)

}

object Header extends ApiCodecs {

  def toSigma(header: Header): special.sigma.Header =
    CHeader(
      id = header.id.toBytes.toColl,
      version = header.version,
      parentId = header.parentId.toBytes.toColl,
      ADProofsRoot = header.ADProofsRoot.asInstanceOf[Array[Byte]].toColl,
      stateRoot = CAvlTree(ErgoInterpreter.avlTreeFromDigest(header.stateRoot)),
      transactionsRoot = header.transactionsRoot.asInstanceOf[Array[Byte]].toColl,
      timestamp = header.timestamp,
      nBits = header.nBits,
      height = header.height,
      extensionRoot = header.extensionRoot.asInstanceOf[Array[Byte]].toColl,
      minerPk = CGroupElement(header.powSolution.pk),
      powOnetimePk = CGroupElement(header.powSolution.w),
      powNonce = header.powSolution.n.toColl,
      powDistance = CBigInt(header.powSolution.d.bigInteger),
      votes = header.votes.toColl
    )

  val InitialVersion: Byte = 1

  val modifierTypeId: ModifierTypeId = ModifierTypeId @@ (101: Byte)

  lazy val GenesisParentId: ModifierId = bytesToId(Array.fill(Constants.HashLength)(0: Byte))

  implicit val jsonEncoder: Encoder[Header] = { h: Header =>
    Map(
      "id" -> Algos.encode(h.id).asJson,
      "transactionsRoot" -> Algos.encode(h.transactionsRoot).asJson,
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
      "votes" -> Algos.encode(h.votes).asJson,
      "size" -> h.size.asJson,
      "extensionId" -> Algos.encode(h.extensionId).asJson,
      "transactionsId" -> Algos.encode(h.transactionsId).asJson,
      "adProofsId" -> Algos.encode(h.ADProofsId).asJson
    ).asJson
  }

  implicit val jsonDecoder: Decoder[Header] = { c: HCursor =>
    for {
      transactionsRoot <- c.downField("transactionsRoot").as[Digest32]
      adProofsRoot <- c.downField("adProofsRoot").as[Digest32]
      stateRoot <- c.downField("stateRoot").as[ADDigest]
      parentId <- c.downField("parentId").as[ModifierId]
      timestamp <- c.downField("timestamp").as[Long]
      extensionHash <- c.downField("extensionHash").as[Digest32]
      nBits <- c.downField("nBits").as[Long]
      height <- c.downField("height").as[Int]
      version <- c.downField("version").as[Byte]
      votes <- c.downField("votes").as[String]
      solutions <- c.downField("powSolutions").as[AutolykosSolution]
    } yield Header(version, parentId, adProofsRoot, stateRoot,
      transactionsRoot, timestamp, nBits, height, extensionHash, solutions, Algos.decode(votes).get)
  }

}

object HeaderSerializer extends ScorexSerializer[Header] {

  override def serialize(h: Header, w: Writer): Unit = {
    serializeWithoutPow(h, w)
    AutolykosSolutionSerializer.serialize(h.version, h.powSolution, w)
  }

  def serializeWithoutPow(h: HeaderWithoutPow, w: Writer): Unit = {
    w.put(h.version)
    w.putBytes(idToBytes(h.parentId))
    w.putBytes(h.ADProofsRoot)
    w.putBytes(h.transactionsRoot)
    w.putBytes(h.stateRoot)
    w.putULong(h.timestamp)
    w.putBytes(h.extensionRoot)
    RequiredDifficulty.serialize(h.nBits, w)
    w.putUInt(h.height)
    w.putBytes(h.votes)

    // For block version >= 2, this new byte encodes length of possible new fields.
    // Set to 0 for now, so no new fields.
    if (h.version > Header.InitialVersion) {
      w.putUByte(0: Byte)
    }
  }

  def bytesWithoutPow(header: HeaderWithoutPow): Array[Byte] = {
    val w = new VLQByteBufferWriter(new ByteArrayBuilder())
    serializeWithoutPow(header, w)
    w.result().toBytes
  }

  def parseWithoutPow(r: Reader): HeaderWithoutPow = {
    val version = r.getByte()
    val parentId = bytesToId(r.getBytes(32))
    val ADProofsRoot = Digest32 @@ r.getBytes(32)
    val transactionsRoot = Digest32 @@ r.getBytes(32)
    val stateRoot = ADDigest @@ r.getBytes(33)
    val timestamp = r.getULong()
    val extensionHash = Digest32 @@ r.getBytes(32)
    val nBits = RequiredDifficulty.parse(r)
    val height = r.getUInt().toIntExact
    val votes = r.getBytes(3)

    // For block version >= 2, a new byte encodes length of possible new fields.
    // If this byte > 0, we read new fields but do nothing, as semantics of the fields is not known.
    if (version > Header.InitialVersion) {
      val newFieldsSize = r.getUByte()
      if (newFieldsSize > 0) {
        r.getBytes(newFieldsSize)
      }
    }

    HeaderWithoutPow(version, parentId, ADProofsRoot, stateRoot, transactionsRoot, timestamp,
      nBits, height, extensionHash, votes)
  }

  override def parse(r: Reader): Header = {
    val headerWithoutPow = parseWithoutPow(r)
    val powSolution = AutolykosSolutionSerializer.parse(r, headerWithoutPow.version)
    headerWithoutPow.toHeader(powSolution, Some(r.consumed))
  }

}
