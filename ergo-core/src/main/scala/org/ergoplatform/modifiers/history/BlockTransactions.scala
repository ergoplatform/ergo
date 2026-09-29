package org.ergoplatform.modifiers.history

import cats.syntax.either._
import sigmastate.utils.Helpers._
import io.circe.syntax._
import io.circe.{Decoder, Encoder, HCursor}
import org.ergoplatform.http.api.ApiCodecs
import org.ergoplatform.modifiers.{BlockTransactionsTypeId, NetworkObjectTypeId, NonHeaderBlockSection, TransactionsCarryingBlockSection}
import org.ergoplatform.modifiers.history.header.Header
import org.ergoplatform.modifiers.mempool.{ErgoTransaction, ErgoTransactionSerializer}
import org.ergoplatform.nodeView.mempool.TransactionMembershipProof
import org.ergoplatform.settings.{Algos, Constants}
import org.ergoplatform.modifiers.history.header.Header.Version
import org.ergoplatform.serialization.ErgoSerializer
import scorex.crypto.authds.{LeafData, Side}
import scorex.crypto.authds.merkle.{InternalNode, Leaf, MerkleProof, MerkleTree, Node}
import scorex.crypto.hash.Digest32
import scorex.util.serialization.{Reader, Writer}
import scorex.util.{ModifierId, bytesToId, idToBytes}
import scorex.util.Extensions._
import sigma.VersionContext

import scala.annotation.nowarn
import scala.collection.mutable


/**
  * Section of a block which contains transactions.
  *
  * @param headerId     - identifier of a header of a corresponding block
  * @param blockVersion - protocol version for the block
  * @param txs          - transactions of the block
  * @param sizeOpt      - (optional) size of the section (cached to not be calculated again)
  */
@nowarn
case class BlockTransactions(headerId: ModifierId,
                             blockVersion: Version,
                             txs: Seq[ErgoTransaction],
                             override val sizeOpt: Option[Int] = None)
  extends NonHeaderBlockSection with TransactionsCarryingBlockSection {

  assert(txs.nonEmpty, "Block should always contain at least 1 coinbase-like transaction")

  override val modifierTypeId: NetworkObjectTypeId.Value = BlockTransactions.modifierTypeId

  /**
    * Ids of block transactions
    */
  lazy val txIds: Seq[Array[Byte]] = txs.map(_.serializedId)

  /**
    * Ids of transaction witnesses (signatures aka spending proofs).
    */
  lazy val witnessIds: Seq[Array[Byte]] = txs.map(tx => tx.witnessSerializedId)

  /**
    * Non-empty (because there's at least 1 transaction) Merkle tree of the block transactions
    */
  lazy val merkleTree: MerkleTree[Digest32] = if (blockVersion == Header.InitialVersion) {
    Algos.merkleTree(LeafData @@ txIds)
  } else {
    Algos.merkleTree(LeafData @@ (txIds ++ witnessIds))
  }

  /**
    * Root hash of the Merkle tree of block transactions
    */
  override lazy val digest: Digest32 = merkleTree.rootHash

  /**
    * Calculates Merkle-tree based membership proof for a given transaction identifier
    *
    * @param txId - transaction identifier
    * @return Some(proof) or None (if transaction with given id is not in the block)
    */
  def proofFor(txId: Array[Byte]): Option[MerkleProof[Digest32]] =
    txIds.indexWhere(_.sameElements(txId)) match {
      case -1 => None
      case index =>
        val leafCount = if (blockVersion == Header.InitialVersion) {
          txIds.size.toLong
        } else {
          txIds.size.toLong + witnessIds.size.toLong
        }
        BlockTransactions
          .proofByIndex(merkleTree, index, leafCount)
          .filter(_.leafData.sameElements(txId))
    }

  def proofFor(txId: ModifierId): Option[MerkleProof[Digest32]] = proofFor(scorex.util.idToBytes(txId))

  override type M = BlockTransactions

  override lazy val serializer: ErgoSerializer[BlockTransactions] = BlockTransactionsSerializer

  override def toString: String = {
    val idStr = Algos.encode(id)
    val headerIdStr = Algos.encode(headerId)
    val displayMaxObjects = 5
    // Artificial limit to show only first `displayMaxObjects` txs.
    val txsStr = txs.take(displayMaxObjects).map(_.toString).mkString(",")
    val txsSuffix = if (txs.lengthCompare(displayMaxObjects) > 0) ", ..." else ""

    s"BlockTransactions(id: $idStr, headerId: $headerIdStr, txs: $txsStr$txsSuffix)"
  }

  override lazy val transactions: Seq[ErgoTransaction] = txs
}

object BlockTransactions extends ApiCodecs {

  val modifierTypeId: NetworkObjectTypeId.Value = BlockTransactionsTypeId.value

  private def proofByIndex(tree: MerkleTree[Digest32],
                           index: Int,
                           leafCount: Long): Option[MerkleProof[Digest32]] = {
    def paddedLength(length: Long, target: Long): Long =
      if (length >= target) length else paddedLength(length * 2, target)

    def loop(node: Node[Digest32],
             leafIndex: Long,
             subtreeLength: Long,
             levels: List[(Digest32, Side)]): Option[MerkleProof[Digest32]] = node match {
      case internal: InternalNode[Digest32] =>
        val halfLength = subtreeLength / 2
        if (leafIndex < halfLength) {
          loop(
            internal.left,
            leafIndex,
            halfLength,
            (internal.right.hash, MerkleProof.LeftSide) :: levels
          )
        } else {
          loop(
            internal.right,
            leafIndex - halfLength,
            halfLength,
            (internal.left.hash, MerkleProof.RightSide) :: levels
          )
        }
      case leaf: Leaf[Digest32] if leafIndex == 0 && subtreeLength == 1 =>
        Some(MerkleProof[Digest32](leaf.data, levels)(leaf.hf))
      case _ =>
        None
    }

    if (index < 0 || index.toLong >= leafCount) None
    else loop(tree.topNode, index.toLong, paddedLength(2L, leafCount), Nil)
  }

  // Used in the miner when a BlockTransaction instance is not generated yet (because a header is not known)
  def transactionsRoot(txs: Seq[ErgoTransaction], blockVersion: Version): Digest32 = {
    if (blockVersion == Header.InitialVersion) {
      Algos.merkleTreeRoot(LeafData @@ txs.map(_.serializedId))
    } else {
      Algos.merkleTreeRoot(LeafData @@ (txs.map(_.serializedId) ++ txs.map(_.witnessSerializedId)))
    }
  }

  // Could be useful when only digest of transactions is available, not a BlockTransaction instance
  def proofValid(transactionsDigest: Digest32, proof: MerkleProof[Digest32]): Boolean = proof.valid(transactionsDigest)

  def proofValid(transactionsDigest: Digest32, proof: TransactionMembershipProof): Boolean =
    proofValid(transactionsDigest, proof.proof)

  implicit val jsonEncoder: Encoder[BlockTransactions] = Encoder.instance { bt: BlockTransactions =>
    Map(
      "headerId" -> Algos.encode(bt.headerId).asJson,
      "transactions" -> bt.txs.map(_.asJson).asJson,
      "blockVersion" -> bt.blockVersion.asJson,
      "size" -> bt.size.asJson
    ).asJson
  }

  @nowarn
  implicit val jsonDecoder: Decoder[BlockTransactions] = Decoder.instance { c: HCursor =>
    for {
      headerId <- c.downField("headerId").as[ModifierId]
      transactions <- c.downField("transactions").as[mutable.WrappedArray[ErgoTransaction]]
      blockVersion <- c.downField("blockVersion").as[Version]
      size <- c.downField("size").as[Int]
    } yield BlockTransactions(headerId, blockVersion, transactions.toSeq, Some(size))
  }
}

object BlockTransactionsSerializer extends ErgoSerializer[BlockTransactions] {
  // See a comment in the parse() function
  val MaxTransactionsInBlock = 10000000

  override def serialize(bt: BlockTransactions, w: Writer): Unit = {
    w.putBytes(idToBytes(bt.headerId))
    val blockVersion = bt.blockVersion
    if (blockVersion > 1) {
      // see comments in parse()
      w.putUInt(MaxTransactionsInBlock.toLong + bt.blockVersion)
    }
    w.putUInt(bt.txs.size.toLong)
    bt.txs.foreach { tx =>
      if (blockVersion >= VersionContext.V6SoftForkVersion) {
        // since 6.0 we use versioned serializers
        VersionContext.withVersions(blockVersion, blockVersion) {
          ErgoTransactionSerializer.serialize(tx, w)
        }
      } else {
        // before 6.0 activation, VersionContext is not used
        ErgoTransactionSerializer.serialize(tx, w)
      }
    }
  }

  override def parse(r: Reader): BlockTransactions = {
    val startPos = r.position
    val headerId: ModifierId = bytesToId(r.getBytes(Constants.ModifierIdSize))
    val verOrCount = r.getUInt().toIntExact

    /*
     * A hack to avoid need for a database rescan if older version of the serializer was used to put.
     * block transactions into.
     *
     * We consider that in a block there could be no more than 10,000,000 transactions.
     *
     * Then the new serializer puts 10,000,000 + block version (while the old one just puts tx count with no version),
     * and the reader knows that a new serializer was used if the first unsigned integer read is more than 10,000,000.
     */
    var blockVersion = 1: Byte
    var txCount = verOrCount
    if (verOrCount > MaxTransactionsInBlock) {
      blockVersion = (verOrCount - MaxTransactionsInBlock).toByte
      txCount = r.getUInt().toIntExact
    }

    val txs: IndexedSeq[ErgoTransaction] = {
      lazy val version = Header.scriptAndTreeFromBlockVersions(blockVersion)

      (1 to txCount).map { _ =>
        if (blockVersion >= Header.Interpreter60Version) {
          if (headerId == "3f5a4acbdfd76a97f2fdf387559c2a67b4ea5f9e9bcf66ef079cde766c6e9398") {
            // todo: public testnet bug with v7 tree included in v4 block, remove after testnet relaunch
            VersionContext.withVersions(1, 1) {
              ErgoTransactionSerializer.parse(r)
            }
          } else {
            VersionContext.withVersions(version.activatedVersion, version.ergoTreeVersion) {
              ErgoTransactionSerializer.parse(r)
            }
          }
        } else {
          ErgoTransactionSerializer.parse(r)
        }
      }
    }
    BlockTransactions(headerId, blockVersion, txs, Some(r.position - startPos))
  }
}
