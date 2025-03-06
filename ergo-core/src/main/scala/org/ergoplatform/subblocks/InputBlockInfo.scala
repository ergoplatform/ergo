package org.ergoplatform.subblocks

import org.ergoplatform.core.bytesToId
import org.ergoplatform.modifiers.history.header.{Header, HeaderSerializer}
import org.ergoplatform.serialization.ErgoSerializer
import org.ergoplatform.settings.{Algos, Constants}
import org.ergoplatform.subblocks.InputBlockInfo.FakePrevInputBlockId
import scorex.crypto.authds.merkle.BatchMerkleProof
import scorex.crypto.authds.merkle.serialization.BatchMerkleProofSerializer
import scorex.crypto.hash.{Blake2b, Blake2b256, CryptographicHash, Digest32}
import scorex.util.Extensions.IntOps
import scorex.util.ModifierId
import scorex.util.serialization.{Reader, Writer}

/**
  * Sub-block message, sent by the node to peers when a sub-block is generated
  *
  * @param version - message version E(to allow injecting new fields)
  * @param header - subblock
  * @param prevInputBlockId - previous sub block id `subBlock` is following, if missed, sub-block is linked
  *                         to a previous block
  * @param transactionsDigest - digest of new transactions appeared in subblock
  * @param merkleProof - batch Merkle proof for `prevSubBlockId`` and `subblockTransactionsDigest`
  *                      (as they are coming from extension section, and committed in `subBlock` header via extension
  *                      digest)
  */
// todo: include prev txs digest and Merkle proof
case class InputBlockInfo(version: Byte,
                          header: Header,
                          prevInputBlockId: Option[Array[Byte]],
                          transactionsDigest: Digest32,
                          merkleProof: BatchMerkleProof[Digest32] // Merkle proof for both prevSubBlockId & subblockTransactionsDigest
                       ) {

  lazy val id: ModifierId = header.id

  def valid(): Boolean = {
    // todo: implement data validity checks
    false
  }

  def transactionsConfirmedDigest: Digest32 = header.transactionsRoot
}

object InputBlockInfo {

  private val FakePrevInputBlockId: Array[Byte] = Array.fill(32)(0.toByte)

  val initialMessageVersion = 1.toByte

  private val bmp = new BatchMerkleProofSerializer[Digest32, CryptographicHash[Digest32]]()(Blake2b256)

  def serializer: ErgoSerializer[InputBlockInfo] = new ErgoSerializer[InputBlockInfo] {
    override def serialize(sbi: InputBlockInfo, w: Writer): Unit = {
      w.put(sbi.version)
      HeaderSerializer.serialize(sbi.header, w)
      w.putOption(sbi.prevInputBlockId){case (w, id) => w.putBytes(id)}
      w.putBytes(sbi.transactionsDigest)
      val proof = bmp.serialize(sbi.merkleProof)
      w.putUShort(proof.length.toShort)
      w.putBytes(proof)
    }

    override def parse(r: Reader): InputBlockInfo = {
      val version = r.getByte()
      if (version == initialMessageVersion) {
        val subBlock = HeaderSerializer.parse(r)
        val prevSubBlockId = r.getOption(r.getBytes(Constants.ModifierIdSize))
        val transactionsDigest = Digest32 @@ r.getBytes(Constants.ModifierIdSize)
        val merkleProofSize = r.getUShort().toShortExact
        val merkleProofBytes = r.getBytes(merkleProofSize)
        val merkleProof = bmp.deserialize(merkleProofBytes).get // parse Merkle proof
        new InputBlockInfo(version, subBlock, prevSubBlockId, transactionsDigest, merkleProof)
      } else {
        throw new Exception("Unsupported sub-block message version")
      }
    }
  }
}
