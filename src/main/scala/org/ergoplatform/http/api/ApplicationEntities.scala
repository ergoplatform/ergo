package org.ergoplatform.http.api

import io.circe.syntax._
import io.circe.{Decoder, Encoder, HCursor, Json}
import org.ergoplatform.ErgoBox.BoxId
import org.ergoplatform.JsonCodecs
import org.ergoplatform.settings.Algos
import org.ergoplatform.wallet.Constants.ApplicationId
import scorex.crypto.authds.ADKey

/**
  * Application-related classes and objects needed for API
  */
object ApplicationEntities {

  case class ApplicationIdWrapper(appId: ApplicationId)

  object ApplicationIdWrapper extends ApiCodecs {

    implicit val applicationIdWrapperEncoder: Encoder[ApplicationIdWrapper] = { appStatus =>
      Json.obj("appId" -> appStatus.appId.asJson)
    }

    implicit val applicationIdWrapperDecoder: Decoder[ApplicationIdWrapper] = { c: HCursor =>
      for {
        appId <- ApplicationId @@ c.downField("appId").as[Short]
      } yield ApplicationIdWrapper(appId)
    }

  }

  case class ApplicationIdBoxId(appId: ApplicationId, boxId: BoxId)

  object ApplicationIdBoxId extends ApiCodecs {

    implicit val applicationIdBoxIdEncoder: Encoder[ApplicationIdBoxId] = { appStatus =>
      Json.obj("appId" -> appStatus.appId.asJson, "boxId" -> Algos.encode(appStatus.boxId).asJson)
    }

    implicit val applicationIdDecoder: Decoder[ApplicationIdBoxId] = { c: HCursor =>
      for {
        appId <- ApplicationId @@ c.downField("appId").as[Short]
        boxId <- c.downField("boxId").as[ADKey]
      } yield ApplicationIdBoxId(appId, boxId)
    }

  }

  case class ApplicationIdBox(appId: ApplicationId, boxBytes: Array[Byte])

  object ApplicationIdBox extends JsonCodecs {

    implicit val applicationIdDecoder: Decoder[ApplicationIdBox] = { c: HCursor =>
      for {
        appId <- ApplicationId @@ c.downField("appId").as[Short]
        boxBytes <- c.downField("boxBytes").as[Array[Byte]]
      } yield ApplicationIdBox(appId, boxBytes)
    }

  }

}
