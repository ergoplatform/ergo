package org.ergoplatform.subblocks

import org.ergoplatform.modifiers.history.header.{Header, HeaderSerializer}
import org.ergoplatform.serialization.ErgoSerializer
import org.ergoplatform.settings.Constants
import scorex.crypto.authds.merkle.MerkleProof
import scorex.crypto.hash.Digest32
import scorex.util.serialization.{Reader, Writer}

/**
  * Sub-block message, sent by the node to peers when a sub-block is generated
  *
  * @param version - message version E(to allow injecting new fields)
  * @param subBlock - subblock
  * @param prevSubBlockId - previous sub block id `subBlock` is following, if missed, sub-block is linked
  *                         to a previous block
  */
case class SubBlockInfo(version: Byte,
                        subBlock: Header,
                        prevSubBlockId: Option[Array[Byte]],
                        subblockTransactionsDigest: Digest32,
                        merkleProof: MerkleProof[Digest32] // Merkle proof for both prevSubBlockId & subblockTransactionsDigest
                       ) {
  // todo: implement Merkle proof serialization
  // todo: implement data validity checks

  def transactionsConfirmedDigest: Digest32 = subBlock.transactionsRoot
}

object SubBlockInfo {

  val initialMessageVersion = 1

  def serializer: ErgoSerializer[SubBlockInfo] = new ErgoSerializer[SubBlockInfo] {
    override def serialize(sbi: SubBlockInfo, w: Writer): Unit = {
      w.put(sbi.version)
      HeaderSerializer.serialize(sbi.subBlock, w)
      w.putOption(sbi.prevSubBlockId){case (w, id) => w.putBytes(id)}
      w.putBytes(sbi.subblockTransactionsDigest)
      // todo: add Merkle proof serialization
    }

    override def parse(r: Reader): SubBlockInfo = {
      val version = r.getByte()
      if (version == initialMessageVersion) {
        val subBlock = HeaderSerializer.parse(r)
        val prevSubBlockId = r.getOption(r.getBytes(Constants.ModifierIdSize))
        val subblockTransactionsDigest = Digest32 @@ r.getBytes(Constants.ModifierIdSize)
        val merkleProof = null // parse Merkle proof
        new SubBlockInfo(version, subBlock, prevSubBlockId, subblockTransactionsDigest, merkleProof)
      } else {
        throw new Exception("Unsupported sub-block message version")
      }
    }
  }
}
