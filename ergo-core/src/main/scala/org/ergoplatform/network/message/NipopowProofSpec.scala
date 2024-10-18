package org.ergoplatform.network.message

import scorex.util.Extensions._
import scorex.util.serialization.{Reader, Writer}

/**
 * The `NipopowProof` message is a reply to a `GetNipopowProof` message.
 */
object NipopowProofSpec extends MessageSpecInitial[Array[Byte]] {

  val SizeLimit = 2000000
  override val messageCode: Byte = 91
  override val messageName: String = "NipopowProof"

  override def serialize(proof: Array[Byte], w: Writer): Unit = {
    w.putUInt(proof.length)
    w.putBytes(proof)
    w.putUShort(0) // to allow adding new data in future, we are adding possible pad length
  }

  override def parse(r: Reader): Array[Byte] = {
    require(r.remaining <= SizeLimit, s"Too big NipopowProofSpec message(size: ${r.remaining})")
    val proofSize = r.getUInt().toIntExact
    require(proofSize > 0  && proofSize < SizeLimit)
    val proof = r.getBytes(proofSize)
    val remainingBytes = r.getUShort()
    if (remainingBytes > 0 && remainingBytes < SizeLimit) {
      r.getBytes(remainingBytes) // current version of reader just skips possible additional bytes
    }
    proof
  }
}
