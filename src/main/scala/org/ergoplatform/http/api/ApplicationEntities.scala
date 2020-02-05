package org.ergoplatform.http.api

import io.circe.syntax._
import io.circe.{Decoder, Encoder, HCursor, Json}
import org.ergoplatform.ErgoBox.BoxId
import org.ergoplatform.JsonCodecs
import org.ergoplatform.nodeView.wallet.scanning.ExternalApplication.AppId
import org.ergoplatform.settings.Algos
import scorex.crypto.authds.ADKey

//Application-related classes and objects
object ApplicationEntities {

  case class ApplicationId(appId: AppId)

  object ApplicationId {

    implicit val applicationIdEncoder: Encoder[ApplicationId] = { appStatus =>
      Json.obj("appId" -> appStatus.appId.asJson)
    }

    implicit val applicationIdDecoder: Decoder[ApplicationId] = { c: HCursor =>
      for {
        appId <- c.downField("appId").as[Short]
      } yield ApplicationId(appId)
    }

  }

  case class ApplicationIdBoxId(appId: AppId, boxId: BoxId)

  object ApplicationIdBoxId extends JsonCodecs {

    implicit val applicationIdBoxIdEncoder: Encoder[ApplicationIdBoxId] = { appStatus =>
      Json.obj("appId" -> appStatus.appId.asJson, "boxId" -> Algos.encode(appStatus.boxId).asJson)
    }

    implicit val applicationIdDecoder: Decoder[ApplicationIdBoxId] = { c: HCursor =>
      for {
        appId <- c.downField("appId").as[Short]
        boxId <- c.downField("boxId").as[ADKey]
      } yield ApplicationIdBoxId(appId, boxId)
    }

  }

  case class ApplicationIdBox(appId: AppId, boxBytes: Array[Byte])

  object ApplicationIdBox extends JsonCodecs {

    implicit val applicationIdDecoder: Decoder[ApplicationIdBox] = { c: HCursor =>
      for {
        appId <- c.downField("appId").as[Short]
        boxBytes <- c.downField("boxBytes").as[Array[Byte]]
      } yield ApplicationIdBox(appId, boxBytes)
    }

  }

}
