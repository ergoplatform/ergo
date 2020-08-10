package org.ergoplatform.http.api

import io.circe.{Decoder, HCursor}
import org.ergoplatform.ErgoBox.BoxId
import org.ergoplatform.JsonCodecs
import org.ergoplatform.wallet.Constants.ScanId

/**
  * Scan-related classes and objects needed for API
  */
object ScanEntities {

  case class ScanIdWrapper(scanId: ScanId)

  object ScanIdWrapper extends ApiCodecs {

    import io.circe._, io.circe.generic.semiauto._

    implicit val scanIdWrapperEncoder: Decoder[ScanIdWrapper] = deriveDecoder[ScanIdWrapper]
    implicit val scanIdWrapperDecoder: Encoder[ScanIdWrapper] = deriveEncoder[ScanIdWrapper]

  }

  case class ScanIdBoxId(scanId: ScanId, boxId: BoxId)

  object ScanIdBoxId extends ApiCodecs {

    import io.circe._, io.circe.generic.semiauto._

    implicit val scanIdBoxIdEncoder: Decoder[ScanIdBoxId] = deriveDecoder[ScanIdBoxId]
    implicit val scanIdBoxIdDecoder: Encoder[ScanIdBoxId] = deriveEncoder[ScanIdBoxId]

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
