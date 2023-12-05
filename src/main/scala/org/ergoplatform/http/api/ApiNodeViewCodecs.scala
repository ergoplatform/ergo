package org.ergoplatform.http.api

import io.circe._
import io.circe.syntax._
import org.ergoplatform.modifiers.mempool.UnsignedErgoTransaction
import org.ergoplatform.nodeView.state.SnapshotsInfo
import org.ergoplatform.nodeView.state.UtxoState.ManifestId
import org.ergoplatform.nodeView.wallet.persistence.WalletDigest
import org.ergoplatform.nodeView.wallet.requests.{ExternalSecret, GenerateCommitmentsRequest, TransactionSigningRequest}
import org.ergoplatform.sdk.wallet.secrets.{DhtSecretKey, DlogSecretKey}
import org.ergoplatform.wallet.interpreter.TransactionHintsBag

trait ApiNodeViewCodecs extends ApiCodecs {
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

  implicit val transactionSigningRequestEncoder: Encoder[TransactionSigningRequest] = { tsr =>
    Json.obj(
      "tx" -> tsr.unsignedTx.asJson,
      "secrets" -> Json.obj(
        "dlog" -> tsr.dlogs.asJson,
        "dht" -> tsr.dhts.asJson
      ),
      "hints" -> tsr.hints.asJson,
      "inputsRaw" -> tsr.inputs.asJson,
      "dataInputsRaw" -> tsr.dataInputs.asJson
    )
  }

  implicit val transactionSigningRequestDecoder: Decoder[TransactionSigningRequest] = { cursor =>
    for {
      tx <- cursor.downField("tx").as[UnsignedErgoTransaction]
      dlogs <- cursor.downField("secrets").downField("dlog").as[Option[Seq[DlogSecretKey]]]
      dhts <- cursor.downField("secrets").downField("dht").as[Option[Seq[DhtSecretKey]]]
      hints <- cursor.downField("hints").as[Option[TransactionHintsBag]]
      inputs <- cursor.downField("inputsRaw").as[Option[Seq[String]]]
      dataInputs <- cursor.downField("dataInputsRaw").as[Option[Seq[String]]]
      secrets = (dlogs.getOrElse(Seq.empty) ++ dhts.getOrElse(Seq.empty)).map(ExternalSecret.apply)
    } yield TransactionSigningRequest(tx, hints.getOrElse(TransactionHintsBag.empty), secrets, inputs, dataInputs)
  }

  implicit val generateCommitmentsRequestEncoder: Encoder[GenerateCommitmentsRequest] = { gcr =>
    Json.obj(
      "tx" -> gcr.unsignedTx.asJson,
      "secrets" -> Json.obj(
        "dlog" -> gcr.dlogs.asJson,
        "dht" -> gcr.dhts.asJson,
        "inputsRaw" -> gcr.inputs.asJson,
        "dataInputsRaw" -> gcr.dataInputs.asJson
      )
    )
  }

  implicit val generateCommitmentsRequestDecoder: Decoder[GenerateCommitmentsRequest] = { cursor =>
    for {
      tx <- cursor.downField("tx").as[UnsignedErgoTransaction]
      dlogs <- cursor.downField("secrets").downField("dlog").as[Option[Seq[DlogSecretKey]]]
      dhts <- cursor.downField("secrets").downField("dht").as[Option[Seq[DhtSecretKey]]]
      secrets = (dlogs.getOrElse(Seq.empty) ++ dhts.getOrElse(Seq.empty)).map(ExternalSecret.apply)
      secretsOpt = if (secrets.isEmpty) None else Some(secrets)
      inputs <- cursor.downField("inputsRaw").as[Option[Seq[String]]]
      dataInputs <- cursor.downField("dataInputsRaw").as[Option[Seq[String]]]
    } yield GenerateCommitmentsRequest(tx, secretsOpt, inputs, dataInputs)
  }
}
