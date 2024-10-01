package org.ergoplatform.subblocks

import org.ergoplatform.modifiers.history.header.{Header, HeaderSerializer}
import org.ergoplatform.serialization.ErgoSerializer
import org.ergoplatform.settings.Constants
import scorex.crypto.authds.merkle.BatchMerkleProof
import scorex.crypto.authds.merkle.serialization.BatchMerkleProofSerializer
import scorex.crypto.hash.{Blake2b, Blake2b256, CryptographicHash, Digest32}
import scorex.util.Extensions.IntOps
import scorex.util.serialization.{Reader, Writer}

/**
  * Sub-block message, sent by the node to peers when a sub-block is generated
  *
  * @param version - message version E(to allow injecting new fields)
  * @param subBlock - subblock
  * @param prevSubBlockId - previous sub block id `subBlock` is following, if missed, sub-block is linked
  *                         to a previous block
  * @param subblockTransactionsDigest - digest of new transactions appeared in subblock
  * @param merkleProof - batch Merkle proof for `prevSubBlockId`` and `subblockTransactionsDigest`
  *                      (as they are coming from extension section, and committed in `subBlock` header via extension
  *                      digest)
  */
case class SubBlockInfo(version: Byte,
                        subBlock: Header,
                        prevSubBlockId: Option[Array[Byte]],
                        subblockTransactionsDigest: Digest32,
                        merkleProof: BatchMerkleProof[Digest32] // Merkle proof for both prevSubBlockId & subblockTransactionsDigest
                       ) {

  def valid(): Boolean = {
    // todo: implement data validity checks
    false
  }

  def transactionsConfirmedDigest: Digest32 = subBlock.transactionsRoot
}

object SubBlockInfo {

  val initialMessageVersion = 1

  private val bmp = new BatchMerkleProofSerializer[Digest32, CryptographicHash[Digest32]]()(Blake2b256)

  def serializer: ErgoSerializer[SubBlockInfo] = new ErgoSerializer[SubBlockInfo] {
    override def serialize(sbi: SubBlockInfo, w: Writer): Unit = {
      w.put(sbi.version)
      HeaderSerializer.serialize(sbi.subBlock, w)
      w.putOption(sbi.prevSubBlockId){case (w, id) => w.putBytes(id)}
      w.putBytes(sbi.subblockTransactionsDigest)
      val proof = bmp.serialize(sbi.merkleProof)
      w.putUShort(proof.length.toShort)
      w.putBytes(proof)
    }

    override def parse(r: Reader): SubBlockInfo = {
      val version = r.getByte()
      if (version == initialMessageVersion) {
        val subBlock = HeaderSerializer.parse(r)
        val prevSubBlockId = r.getOption(r.getBytes(Constants.ModifierIdSize))
        val subblockTransactionsDigest = Digest32 @@ r.getBytes(Constants.ModifierIdSize)
        val merkleProofSize = r.getUShort().toShortExact
        val merkleProofBytes = r.getBytes(merkleProofSize)
        val merkleProof = bmp.deserialize(merkleProofBytes).get // parse Merkle proof
        new SubBlockInfo(version, subBlock, prevSubBlockId, subblockTransactionsDigest, merkleProof)
      } else {
        throw new Exception("Unsupported sub-block message version")
      }
    }
  }
}
