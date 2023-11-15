package org.ergoplatform.http.api

import io.circe._
import io.circe.syntax._
import org.ergoplatform.ErgoAddressEncoder
import org.ergoplatform.sdk.JsonCodecs
import org.ergoplatform.nodeView.history.extra.ExtraIndexer.getAddress
import org.ergoplatform.nodeView.history.extra.{BalanceInfo, IndexedErgoBox, IndexedErgoTransaction, IndexedToken}

trait ApiExtraCodecs extends JsonCodecs {
  implicit val ergoAddressEncoder: ErgoAddressEncoder = null

  implicit val indexedBoxEncoder: Encoder[IndexedErgoBox] = { iEb =>
    iEb.box.asJson.deepMerge(Json.obj(
      "globalIndex" -> iEb.globalIndex.asJson,
      "inclusionHeight" -> iEb.inclusionHeight.asJson,
      "address" -> ergoAddressEncoder.toString(getAddress(iEb.box.ergoTree)).asJson,
      "spentTransactionId" -> iEb.spendingTxIdOpt.asJson
    ))
  }

  implicit val indexedBoxSeqEncoder: Encoder[(Seq[IndexedErgoBox], Long)] = { iEbSeq =>
    Json.obj(
      "items" -> iEbSeq._1.asJson,
      "total" -> iEbSeq._2.asJson
    )
  }

  implicit val indexedTxEncoder: Encoder[IndexedErgoTransaction] = { iEt =>
    Json.obj(
      "id" -> iEt.txid.asJson,
      "blockId" -> iEt.blockId.asJson,
      "inclusionHeight" -> iEt.inclusionHeight.asJson,
      "timestamp" -> iEt.timestamp.asJson,
      "index" -> iEt.index.asJson,
      "globalIndex" -> iEt.globalIndex.asJson,
      "numConfirmations" -> iEt.numConfirmations.asJson,
      "inputs" -> iEt.inputs.asJson,
      "dataInputs" -> iEt.dataInputs.asJson,
      "outputs" -> iEt.outputs.asJson,
      "size" -> iEt.txSize.asJson
    )
  }

  implicit val indexedTxSeqEncoder: Encoder[(Seq[IndexedErgoTransaction], Long)] = { iEtSeq =>
    Json.obj(
      "items" -> iEtSeq._1.asJson,
      "total" -> iEtSeq._2.asJson
    )
  }

  implicit val IndexedTokenEncoder: Encoder[IndexedToken] = { token =>
    Json.obj(
      "id" -> token.tokenId.asJson,
      "boxId" -> token.boxId.asJson,
      "emissionAmount" -> token.amount.asJson,
      "name" -> token.name.asJson,
      "description" -> token.description.asJson,
      "decimals" -> token.decimals.asJson
    )
  }

  implicit val BalanceInfoEncoder: Encoder[BalanceInfo] = { bal =>
    Json.obj(
      "nanoErgs" -> bal.nanoErgs.asJson,
      "tokens" -> bal.tokens.map(token => {
        Json.obj(
          "tokenId" -> token._1.asJson,
          "amount" -> token._2.asJson,
          "decimals" -> bal.additionalTokenInfo(token._1)._2.asJson,
          "name" -> bal.additionalTokenInfo(token._1)._1.asJson
        )
      }).asJson
    )
  }

  implicit val TotalBalanceInfoEncoder: Encoder[(BalanceInfo, BalanceInfo)] = { tBal =>
    Json.obj(
      "confirmed" -> tBal._1.asJson,
      "unconfirmed" -> tBal._2.asJson
    )
  }
}
