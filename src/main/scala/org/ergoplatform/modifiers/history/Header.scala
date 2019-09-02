package org.ergoplatform.modifiers.history

import io.circe.syntax._
import io.circe.{Decoder, Encoder, HCursor}
import org.ergoplatform.api.ApiCodecs
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
import sigmastate.eval.{CAvlTree, CBigInt, CGroupElement}
import special.collection.{Coll, CollOverArray}
import special.sigma
import special.sigma.{AvlTree, GroupElement}

case class Header(version: Version,
                  override val parentId: ModifierId,
                  ADProofsRoot: Digest32,
                  stateRoot: ADDigest, //33 bytes! extra byte with tree height here!
                  transactionsRoot: Digest32,
                  timestamp: Timestamp,
                  nBits: Long, //actually it is unsigned int
                  height: Int,
                  extensionRoot: Digest32,
                  powSolution: AutolykosSolution,
                  votes: Array[Byte], //3 bytes
                  override val sizeOpt: Option[Int] = None)
  extends PreHeader with ErgoPersistentModifier {

  override def serializedId: Array[Version] = Algos.hash(bytes)

  override type M = Header

  override val modifierTypeId: ModifierTypeId = Header.modifierTypeId

  lazy val requiredDifficulty: Difficulty = RequiredDifficulty.decodeCompactBits(nBits)

  lazy val ADProofsId: ModifierId = BlockSection.computeId(ADProofs.modifierTypeId, id, ADProofsRoot)

  lazy val transactionsId: ModifierId = BlockSection.computeId(BlockTransactions.modifierTypeId, id, transactionsRoot)

  lazy val extensionId: ModifierId = BlockSection.computeId(Extension.modifierTypeId, id, extensionRoot)

  override def minerPk: EcPointType = powSolution.pk

  lazy val sectionIds: Seq[(ModifierTypeId, ModifierId)] = Seq((ADProofs.modifierTypeId, ADProofsId),
    (BlockTransactions.modifierTypeId, transactionsId), (Extension.modifierTypeId, extensionId))

  override lazy val toString: String = s"Header(${this.asJson.noSpaces})"

  override lazy val serializer: ScorexSerializer[Header] = HeaderSerializer

  lazy val isGenesis: Boolean = height == ErgoHistory.GenesisHeight

  /**
    * Checks that modifier m corresponds to this header
    */
  def isCorrespondingModifier(m: ErgoPersistentModifier): Boolean = sectionIds.exists(_._2 == m.id)

  /**
    * Estimate that this Header is new enough to possibly be the best header
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

  def toSigma(header: Header): special.sigma.Header = new special.sigma.Header {
    override def id: Coll[Byte] = new CollOverArray(idToBytes(header.id))

    override def version: Version = header.version

    override def parentId: Coll[Byte] = new CollOverArray(idToBytes(header.parentId))

    override def ADProofsRoot: Coll[Byte] = new CollOverArray(header.ADProofsRoot)

    override def stateRoot: AvlTree = CAvlTree(ErgoInterpreter.avlTreeFromDigest(header.stateRoot))

    override def transactionsRoot: Coll[Byte] = new CollOverArray(header.transactionsRoot)

    override def timestamp: Timestamp = header.timestamp

    override def nBits: Timestamp = header.nBits

    override def height: Int = header.height

    override def extensionRoot: Coll[Version] = new CollOverArray(header.extensionRoot)

    override def minerPk: GroupElement = CGroupElement(header.powSolution.pk)

    override def powOnetimePk: GroupElement = CGroupElement(header.powSolution.w)

    override def powNonce: Coll[Byte] = new CollOverArray(header.powSolution.n)

    override def powDistance: sigma.BigInt = CBigInt(header.powSolution.d.bigInteger)

    override def votes: Coll[Byte] = new CollOverArray(header.votes)
  }

  val CurrentVersion: Byte = 1

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
    AutolykosSolutionSerializer.serialize(h.powSolution, w)
  }

  def serializeWithoutPow(h: Header, w: Writer): Unit = {
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
  }

  def bytesWithoutPow(header: Header): Array[Byte] = {
    val w = new VLQByteBufferWriter(new ByteArrayBuilder())
    serializeWithoutPow(header, w)
    w.result().toBytes
  }

  override def parse(r: Reader): Header = {
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

    val powSolution = AutolykosSolutionSerializer.parse(r)

    Header(version, parentId, ADProofsRoot, stateRoot, transactionsRoot, timestamp,
      nBits, height, extensionHash, powSolution, votes, Some(r.consumed))
  }
}
