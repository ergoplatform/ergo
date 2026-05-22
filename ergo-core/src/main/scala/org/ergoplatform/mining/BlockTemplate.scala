package org.ergoplatform.mining

import io.circe.Encoder
import io.circe.syntax._
import org.ergoplatform.http.api.ApiCodecs
import org.ergoplatform.modifiers.history.extension.ExtensionCandidate
import org.ergoplatform.modifiers.history.header.HeaderWithoutPow
import org.ergoplatform.modifiers.mempool.ErgoTransaction
import org.ergoplatform.settings.Algos
import scorex.crypto.authds.SerializedAdProof
import sigma.data.ProveDlog

/**
  * Block template for external miners and block-building extensions.
  *
  * Exposes the pre-PoW header (with all roots already computed by the node) together with the
  * block's transactions, extension and AD proofs, so callers can reassemble the header bytes
  * themselves and either mine it as-is or modify the components before mining. Solutions are
  * submitted back through `POST /mining/solution`.
  *
  * `b` is the PoW target derived from `header.nBits`; `msg` is `Blake2b256(bytesWithoutPow)`
  * over the header — the same value an external GPU miner works on.
  */
case class BlockTemplate(header: HeaderWithoutPow,
                         transactions: Seq[ErgoTransaction],
                         extension: ExtensionCandidate,
                         adProofBytes: SerializedAdProof,
                         pk: ProveDlog,
                         b: BigInt,
                         msg: Array[Byte])

object BlockTemplate extends ApiCodecs {

  def fromCandidateBlock(cb: CandidateBlock,
                         pk: ProveDlog,
                         powScheme: AutolykosPowScheme): BlockTemplate = {
    val header = CandidateUtils.deriveUnprovenHeader(cb)
    BlockTemplate(
      header       = header,
      transactions = cb.transactions,
      extension    = cb.extension,
      adProofBytes = cb.adProofBytes,
      pk           = pk,
      b            = powScheme.getB(cb.nBits),
      msg          = powScheme.msgByHeader(header)
    )
  }

  implicit val encoder: Encoder[BlockTemplate] = Encoder.instance { t: BlockTemplate =>
    Map(
      "header"       -> t.header.asJson,
      "transactions" -> t.transactions.asJson,
      "extension"    -> t.extension.asJson,
      "adProofBytes" -> Algos.encode(t.adProofBytes).asJson,
      "pk"           -> t.pk.asJson,
      "b"            -> t.b.asJson(bigIntEncoder),
      "msg"          -> Algos.encode(t.msg).asJson
    ).asJson
  }

}
