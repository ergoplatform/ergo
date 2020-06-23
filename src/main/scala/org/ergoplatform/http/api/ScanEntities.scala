package org.ergoplatform.http.api

import io.circe.syntax._
import io.circe.{Decoder, Encoder, HCursor, Json}
import org.ergoplatform.ErgoBox.BoxId
import org.ergoplatform.JsonCodecs
import org.ergoplatform.settings.Algos
import org.ergoplatform.wallet.Constants.ScanId
import scorex.crypto.authds.ADKey

/**
  * Application-related classes and objects needed for API
  */
object ScanEntities {

  case class ScanIdWrapper(scanId: ScanId)

  object ScanIdWrapper extends ApiCodecs {

    implicit val scanIdWrapperEncoder: Encoder[ScanIdWrapper] = { scanStatus =>
      Json.obj("scanId" -> scanStatus.scanId.asJson)
    }

    implicit val scanIdWrapperDecoder: Decoder[ScanIdWrapper] = { c: HCursor =>
      for {
        scanId <- ScanId @@ c.downField("scanId").as[Short]
      } yield ScanIdWrapper(scanId)
    }

  }

  case class ScanIdBoxId(scanId: ScanId, boxId: BoxId)

  object ScanIdBoxId extends ApiCodecs {

    implicit val scanIdBoxIdEncoder: Encoder[ScanIdBoxId] = { scanStatus =>
      Json.obj("scanId" -> scanStatus.scanId.asJson, "boxId" -> Algos.encode(scanStatus.boxId).asJson)
    }

    implicit val scanIdBoxIdDecoder: Decoder[ScanIdBoxId] = { c: HCursor =>
      for {
        scanId <- ScanId @@ c.downField("scanId").as[Short]
        boxId <- c.downField("boxId").as[ADKey]
      } yield ScanIdBoxId(scanId, boxId)
    }

  }

  case class ScanIdBox(scanId: ScanId, boxBytes: Array[Byte])

  object ScanIdBox extends JsonCodecs {

    implicit val scanIdBoxDecoder: Decoder[ScanIdBox] = { c: HCursor =>
      for {
        scanId <- ScanId @@ c.downField("scanId").as[Short]
        boxBytes <- c.downField("boxBytes").as[Array[Byte]]
      } yield ScanIdBox(scanId, boxBytes)
    }

  }

}
