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
case class InputBlockAnnouncement(version: Byte,
                          header: Header,
                          inputBlockFields: InputBlockFields,
                          weakTxIds: Option[Seq[ErgoTransaction.WeakId]],
                          unparsedBytes: Array[Byte] = Array.emptyByteArray) extends ScorexLogging {

  lazy val id: ModifierId = header.id

  def valid(powScheme: AutolykosPowScheme,
             parameters: Parameters,
             expectedNBits: Option[Long] = None): Boolean = {
    val powValid = powScheme.checkInputBlockPoW(header, parameters)
    val extValid = inputBlockFields.inputBlockFieldsProof.valid(header.extensionRoot)
    val nBitsValid = expectedNBits.forall(header.nBits == _)

    if (!powValid) {
      log.warn(s"PoW check fails for sub-block ${header.id}")
    }
    if (!extValid) {
      log.warn(s"Extension section check fails for sub-block ${header.id}")
    }
    if (!nBitsValid) {
      log.warn(s"Difficulty (nBits) mismatch for sub-block ${header.id}: " +
        s"header.nBits=${header.nBits}, expected=${expectedNBits.getOrElse("unknown")}")
    }
    powValid && extValid && nBitsValid
  }

  lazy val prevInputBlockId: Option[ModifierId] = inputBlockFields.prevInputBlockId.map(bytesToId)

  def transactionsDigest: Digest32 = inputBlockFields.transactionsDigest

  def merkleProof: BatchMerkleProof[Digest32] = inputBlockFields.inputBlockFieldsProof

}

object InputBlockAnnouncement {

  val initialMessageVersion: Byte = 1.toByte

  private val bmp = new BatchMerkleProofSerializer[Digest32, CryptographicHash[Digest32]]()(Blake2b256)

  def serializer: ErgoSerializer[InputBlockAnnouncement] = new ErgoSerializer[InputBlockAnnouncement] {
    override def serialize(sbi: InputBlockAnnouncement, w: Writer): Unit = {
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
      if (sbi.version > initialMessageVersion) {
        w.putUByte(sbi.unparsedBytes.length)
        w.putBytes(sbi.unparsedBytes)
      }
    }

    override def parse(r: Reader): InputBlockAnnouncement = {
      val version = r.getByte()
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
      val unparsedBytes = if (version > initialMessageVersion) {
        val newFieldsSize = r.getUByte()
        if (newFieldsSize > 0) {
          r.getBytes(newFieldsSize)
        } else {
          Array.emptyByteArray
        }
      } else {
        Array.emptyByteArray
      }
      new InputBlockAnnouncement(version, subBlock, fields, weakTxIds, unparsedBytes)
    }
  }

}
