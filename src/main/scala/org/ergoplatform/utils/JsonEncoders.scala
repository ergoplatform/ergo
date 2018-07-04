package org.ergoplatform.utils

import org.ergoplatform.api.ApiCodecs
import org.ergoplatform.local.{MiningStatusEncoder, NodeInfoEncoder}
import org.ergoplatform.mining.CandidateBlockEncoder
import org.ergoplatform.modifiers.ErgoFullBlockEncoder
import org.ergoplatform.modifiers.history.{ADProofsEncoder, BlockTransactionsEncoder, HeaderEncoder}
import org.ergoplatform.modifiers.mempool.{ErgoTransactionDecoder, ErgoTransactionEncoder, TransactionIdsForHeader}
import org.ergoplatform.settings.ApiSettings
import scorex.core.serialization.SerializerRegistry
import scorex.core.serialization.SerializerRegistry.SerializerRecord

class JsonEncoders(implicit val settings: ApiSettings) extends ApiCodecs {

  implicit val headerEncoder = new HeaderEncoder
  implicit val proofsEncoder = new ADProofsEncoder
  implicit val transactionEncoder = new ErgoTransactionEncoder
  implicit val transactionDecoder = new ErgoTransactionDecoder
  implicit val blockTransactionsEncoder = new BlockTransactionsEncoder
  implicit val candidateBlockEncoder = new CandidateBlockEncoder
  implicit val ergoFullBlockEncoder = new ErgoFullBlockEncoder
  implicit val nodeInfoEncoder = new NodeInfoEncoder
  implicit val miningStatusEncoder = new MiningStatusEncoder

  implicit val serializerReg: SerializerRegistry = SerializerRegistry(Seq(
    SerializerRecord(TransactionIdsForHeader.jsonEncoder),
    SerializerRecord(headerEncoder),
    SerializerRecord(nodeInfoEncoder)
  ))

}

object JsonEncoders {
  val default: JsonEncoders = new JsonEncoders()(ApiSettings.empty)
}
