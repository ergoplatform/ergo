package org.ergoplatform.modifiers.history.header

import cats.syntax.either._
import sigmastate.utils.Helpers._
import io.circe.syntax._
import io.circe.{Decoder, Encoder, HCursor}
import org.ergoplatform.http.api.ApiCodecs
import org.ergoplatform.mining.AutolykosSolution
import org.ergoplatform.mining.difficulty.DifficultySerializer
import org.ergoplatform.modifiers.history.extension.Extension
import org.ergoplatform.modifiers.history.{ADProofs, BlockTransactions, PreHeader}
import org.ergoplatform.modifiers.{BlockSection, HeaderTypeId, NetworkObjectTypeId, NonHeaderBlockSection}
import org.ergoplatform.nodeView.history.ErgoHistoryUtils._
import org.ergoplatform.settings.{Algos, Constants}
import org.ergoplatform.wallet.interpreter.ErgoInterpreter
import org.ergoplatform.serialization.ErgoSerializer
import scorex.crypto.authds.ADDigest
import scorex.crypto.hash.Digest32
import scorex.util._
import sigmastate.crypto.CryptoConstants.EcPointType
import sigmastate.eval.Extensions._
import sigmastate.eval.{CAvlTree, CBigInt, CGroupElement, CHeader}

import scala.annotation.nowarn
import scala.concurrent.duration.FiniteDuration

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
case class Header(override val version: Header.Version,
                  override val parentId: ModifierId,
                  override val ADProofsRoot: Digest32,
                  override val stateRoot: ADDigest, //33 bytes! extra byte with tree height here!
                  override val transactionsRoot: Digest32,
                  override val timestamp: Header.Timestamp,
                  override val nBits: Long, //actually it is unsigned int
                  override val height: Int,
                  override val extensionRoot: Digest32,
                  powSolution: AutolykosSolution,
                  override val votes: Array[Byte], //3 bytes
                  override val sizeOpt: Option[Int] = None) extends HeaderWithoutPow(version, parentId, ADProofsRoot, stateRoot, transactionsRoot, timestamp,
  nBits, height, extensionRoot, votes) with PreHeader with BlockSection {

  override def serializedId: Array[Header.Version] = Algos.hash(bytes)

  override type M = Header

  override val modifierTypeId: NetworkObjectTypeId.Value = Header.modifierTypeId

  lazy val requiredDifficulty: Difficulty = DifficultySerializer.decodeCompactBits(nBits)

  lazy val ADProofsId: ModifierId = NonHeaderBlockSection.computeId(ADProofs.modifierTypeId, id, ADProofsRoot)

  lazy val transactionsId: ModifierId = NonHeaderBlockSection.computeId(BlockTransactions.modifierTypeId, id, transactionsRoot)

  lazy val extensionId: ModifierId = NonHeaderBlockSection.computeId(Extension.modifierTypeId, id, extensionRoot)

  override def minerPk: EcPointType = powSolution.pk

  /**
    * Expected identifiers of the block sections corresponding to this header
    */
  @nowarn
  lazy val sectionIds: Seq[(NetworkObjectTypeId.Value, ModifierId)] =
    Array(
      (ADProofs.modifierTypeId, ADProofsId),
      (BlockTransactions.modifierTypeId, transactionsId),
      (Extension.modifierTypeId, extensionId)
    )

  /**
    * Expected identifiers of the block sections corresponding to this header,
    * except of state transformations proof section id
    */
  lazy val sectionIdsWithNoProof: Seq[(NetworkObjectTypeId.Value, ModifierId)] = sectionIds.tail

  override lazy val toString: String = s"Header(${this.asJson.noSpaces})"

  override lazy val serializer: ErgoSerializer[Header] = HeaderSerializer

  lazy val isGenesis: Boolean = height == GenesisHeight

  /**
    * Checks that modifier m corresponds to this header
    */
  def isCorrespondingModifier(m: BlockSection): Boolean = sectionIds.exists(_._2 == m.id)

  /**
    * Estimate that this header is recent enough to possibly be the best header
    */
  def isNew(timeDiff: FiniteDuration): Boolean = {
    System.currentTimeMillis() - timestamp < timeDiff.toMillis
  }

  /**
    * New voting epoch starts
    */
  def votingStarts(votingEpochLength: Int): Boolean = height % votingEpochLength == 0 && height > 0

}



object Header extends ApiCodecs {

  type Timestamp = Long
  type Version = Byte

  /**
    * Block version during mainnet launch
    */
  val InitialVersion: Byte = 1

  /**
    * Block version after the Hardening hard-fork
    * Autolykos v2 PoW, witnesses in transactions Merkle tree
    */
  val HardeningVersion: Byte = 2

  /**
    * Block version after the 5.0 soft-fork
    * 5.0 interpreter with JITC, monotonic height rule (EIP-39)
    */
  val Interpreter50Version: Byte = 3

  /**
    * Block version after the 6.0 soft-fork
    * 6.0 interpreter (EIP-50)
    */
  val Interpreter60Version: Byte = 4

  def toSigma(header: Header): sigma.Header =
    CHeader(
      id = header.id.toBytes.toColl,
      version = header.version,
      parentId = header.parentId.toBytes.toColl,
      ADProofsRoot = header.ADProofsRoot.asInstanceOf[Array[Byte]].toColl,
      stateRoot = CAvlTree(ErgoInterpreter.avlTreeFromDigest(header.stateRoot.toColl)),
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

  val modifierTypeId: NetworkObjectTypeId.Value = HeaderTypeId.value

  lazy val GenesisParentId: ModifierId = bytesToId(Array.fill(Constants.HashLength)(0: Byte))

  implicit val jsonEncoder: Encoder[Header] = Encoder.instance { h: Header =>
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

  implicit val jsonDecoder: Decoder[Header] = Decoder.instance { c: HCursor =>
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
