package org.ergoplatform.subblocks

import org.ergoplatform.mining.InputBlockFields
import org.ergoplatform.modifiers.history.header.{Header, HeaderSerializer}
import org.ergoplatform.serialization.ErgoSerializer
import org.ergoplatform.settings.Constants
import scorex.crypto.authds.merkle.BatchMerkleProof
import scorex.crypto.authds.merkle.serialization.BatchMerkleProofSerializer
import scorex.crypto.hash.{Blake2b256, CryptographicHash, Digest32}
import scorex.util.Extensions.IntOps
import scorex.util.ModifierId
import scorex.util.serialization.{Reader, Writer}

/**
  * Sub-block message, sent by the node to peers when a sub-block is generated
  *
  * @param version - message version (to allow injection of new fields)
  * @param header - subblock header
  * @param inputBlockFields - input block related fields in extension section along with Merkle proof of their inclusion
  */
case class InputBlockInfo(version: Byte,
                          header: Header,
                          inputBlockFields: InputBlockFields) {

  lazy val id: ModifierId = header.id

  // todo: only Merkle proof validated for now, check if it is enough
  def valid(): Boolean = {
    inputBlockFields.inputBlockFieldsProof.valid(header.extensionRoot)
  }

  def prevInputBlockId: Option[Array[Byte]] = inputBlockFields.prevInputBlockId

  def transactionsDigest: Digest32 = inputBlockFields.transactionsDigest

  def merkleProof: BatchMerkleProof[Digest32] = inputBlockFields.inputBlockFieldsProof

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
      w.putBytes(sbi.inputBlockFields.prevTransactionsDigest)
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
        val prevTransactionsDigest = Digest32 @@ r.getBytes(Constants.ModifierIdSize)
        val merkleProofSize = r.getUShort().toShortExact
        val merkleProofBytes = r.getBytes(merkleProofSize)
        val merkleProof = bmp.deserialize(merkleProofBytes).get // parse Merkle proof
        new InputBlockInfo(version, subBlock, new InputBlockFields(prevSubBlockId, transactionsDigest, prevTransactionsDigest, merkleProof))
      } else {
        throw new Exception("Unsupported sub-block message version")
      }
    }
  }

}
