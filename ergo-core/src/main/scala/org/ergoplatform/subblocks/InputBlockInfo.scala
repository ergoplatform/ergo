package org.ergoplatform.subblocks

import org.ergoplatform.mining.{AutolykosPowScheme, InputBlockFields}
import org.ergoplatform.modifiers.history.header.{Header, HeaderSerializer}
import org.ergoplatform.modifiers.mempool.ErgoTransaction
import org.ergoplatform.serialization.ErgoSerializer
import org.ergoplatform.settings.{Constants, Parameters}
import scorex.crypto.authds.merkle.BatchMerkleProof
import scorex.crypto.authds.merkle.serialization.BatchMerkleProofSerializer
import scorex.crypto.hash.{Blake2b256, CryptographicHash, Digest32}
import scorex.util.Extensions.IntOps
import scorex.util.{ModifierId, ScorexLogging, bytesToId, idToBytes}
import scorex.util.serialization.{Reader, Writer}
import sigma.util.Extensions.LongOps

/**
  * Sub-block message, sent by the node to peers when a sub-block is generated
  *
  * @param version - message version (to allow injection of new fields)
  * @param header - subblock header
  * @param inputBlockFields - input block related fields in extension section along with Merkle proof of their inclusion
  * @param weakTxIds - optionally, weak transaction ids if they are known during instance construction
  */
case class InputBlockInfo(version: Byte,
                          header: Header,
                          inputBlockFields: InputBlockFields,
                          weakTxIds: Option[Seq[ErgoTransaction.WeakId]]) extends ScorexLogging {

  lazy val id: ModifierId = header.id

  def valid(powScheme: AutolykosPowScheme, parameters: Parameters): Boolean = {
    // todo: check difficulty

    val powValid = powScheme.checkInputBlockPoW(header, parameters)
    val extValid = inputBlockFields.inputBlockFieldsProof.valid(header.extensionRoot)

    if (!powValid) {
      log.warn(s"PoW check fails for sub-block ${header.id}")
    }
    if (!extValid) {
      log.warn(s"Extension section check fails for sub-block ${header.id}")
    }
    powValid && extValid
  }

  lazy val prevInputBlockId: Option[ModifierId] = inputBlockFields.prevInputBlockId.map(bytesToId)

  def transactionsDigest: Digest32 = inputBlockFields.transactionsDigest

  def merkleProof: BatchMerkleProof[Digest32] = inputBlockFields.inputBlockFieldsProof

}

object InputBlockInfo {

  val initialMessageVersion: Byte = 1.toByte

  private val bmp = new BatchMerkleProofSerializer[Digest32, CryptographicHash[Digest32]]()(Blake2b256)

  def serializer: ErgoSerializer[InputBlockInfo] = new ErgoSerializer[InputBlockInfo] {
    override def serialize(sbi: InputBlockInfo, w: Writer): Unit = {
      w.put(sbi.version)
      HeaderSerializer.serialize(sbi.header, w)
      w.putOption(sbi.prevInputBlockId){case (w, id) => w.putBytes(idToBytes(id))}
      w.putBytes(sbi.transactionsDigest)
      w.putBytes(sbi.inputBlockFields.prevTransactionsDigest)
      val proof = bmp.serialize(sbi.merkleProof)
      w.putUShort(proof.length.toShort)
      w.putBytes(proof)
      w.putOption(sbi.weakTxIds){case (w,ids) =>
        w.putUInt(ids.length)
        ids.foreach(w.putBytes)
      }
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
        val weakTxIds = r.getOption({
          val cnt = r.getUInt().toIntExact
          (1 to cnt).map(_ => r.getBytes(ErgoTransaction.WeakIdLength))
        })
        val fields = new InputBlockFields(prevSubBlockId, transactionsDigest, prevTransactionsDigest, merkleProof)
        new InputBlockInfo(version, subBlock, fields, weakTxIds)
      } else {
        // todo: consider proper versioning, eg by adding unparsed bytes like done in Header
        throw new Exception("Unsupported sub-block message version")
      }
    }
  }

}
