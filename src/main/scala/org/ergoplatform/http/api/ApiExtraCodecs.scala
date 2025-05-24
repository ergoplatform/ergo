package org.ergoplatform.http.api

import io.circe._
import io.circe.syntax._
import org.ergoplatform.ErgoAddressEncoder
import org.ergoplatform.sdk.JsonCodecs
import org.ergoplatform.nodeView.history.extra.ExtraIndexer.getAddress
import org.ergoplatform.nodeView.history.extra.{BalanceInfo, IndexedErgoBox, IndexedErgoTransaction, IndexedToken}
import org.ergoplatform.nodeView.state.SnapshotsInfo
import org.ergoplatform.nodeView.state.UtxoState.ManifestId
import org.ergoplatform.nodeView.wallet.persistence.WalletDigest

/**
  * JSON codecs related to extra indices (available via /blockchain API methods), and also few codecs for wallet
  * digest (wallet balances) and UTXO snapshot manifests
  */
trait ApiExtraCodecs extends JsonCodecs {

  def ergoAddressEncoder: ErgoAddressEncoder

  implicit val indexedBoxEncoder: Encoder[IndexedErgoBox] = { iEb =>
    iEb.box.asJson.deepMerge(Json.obj(
      "globalIndex" -> iEb.globalIndex.asJson,
      "inclusionHeight" -> iEb.inclusionHeight.asJson,
      "address" -> ergoAddressEncoder.toString(getAddress(iEb.box.ergoTree)(ergoAddressEncoder)).asJson,
      "spentTransactionId" -> iEb.spendingTxIdOpt.asJson,
      "spendingProof" -> iEb.spendingProofOpt.asJson
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
      "size" -> iEt.size.asJson
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


  implicit val balancesSnapshotEncoder: Encoder[WalletDigest] = { v =>
    import v._
    Json.obj(
      "height" -> height.asJson,
      "balance" -> walletBalance.asJson,
      "assets" -> walletAssetBalances.toMap.map(x => (x._1: String, x._2)).asJson //toMap to have assets as JSON map
    )
  }

  implicit val SnapshotInfoEncoder: Encoder[SnapshotsInfo] = { si =>
    Json.obj(
      "availableManifests" -> si.availableManifests.map { case (height, manifest) =>
        height -> manifest
      }.asJson
    )
  }

  implicit val SnapshotInfoDecoder: Decoder[SnapshotsInfo] = { cursor =>
    for {
      availableManifests <- Decoder.decodeMap[Int, ManifestId].tryDecode(cursor.downField("availableManifests"))
    } yield new SnapshotsInfo(availableManifests)
  }

}
