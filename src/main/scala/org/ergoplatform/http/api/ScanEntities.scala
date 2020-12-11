package org.ergoplatform.http.api

import io.circe.{Decoder, HCursor}
import org.ergoplatform.ErgoBox.BoxId
import org.ergoplatform.{ErgoBox, JsonCodecs}
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


  case class BoxWithScanIds(box: ErgoBox, scanIds: Set[ScanId])

  object BoxWithScanIds extends JsonCodecs {
    implicit val scanIdsBoxDecoder: Decoder[BoxWithScanIds] = { c: HCursor =>
      for {
        box <- c.downField("box").as[ErgoBox]
        scanIds <- ScanId @@ c.downField("scanIds").as[Set[Short]]
      } yield BoxWithScanIds(box, scanIds)
    }
  }

}
