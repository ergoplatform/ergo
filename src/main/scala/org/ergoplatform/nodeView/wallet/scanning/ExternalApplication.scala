package org.ergoplatform.nodeView.wallet.scanning

import org.ergoplatform.api.ApiCodecs
import scorex.core.serialization.ScorexSerializer
import scorex.util.serialization.{Reader, Writer}

import scala.util.{Failure, Success, Try}

case class ExternalApplication(appId: Long, appName: String, trackingRule: ScanningPredicate)

object ExternalApplication {
  val MaxAppNameLength = 255
}

case class ExternalAppRequest(appName: String, trackingRule: ScanningPredicate) {
  def toApp(appId: Long): Try[ExternalApplication] = {
    if(appName.getBytes("UTF-8").length > ExternalApplication.MaxAppNameLength) {
      Failure(new Exception(s"Too application name: $appName"))
    } else {
      Success(ExternalApplication(appId, appName, trackingRule))
    }
  }
}

object ExternalApplicationSerializer extends ScorexSerializer[ExternalApplication] {
  override def serialize(obj: ExternalApplication, w: Writer): Unit = {
    w.putLong(obj.appId)
    w.putShortString(obj.appName)
    ScanningPredicateSerializer.serialize(obj.trackingRule, w)
  }

  override def parse(r: Reader): ExternalApplication = {
    val appId = r.getLong()
    val appName = r.getShortString()
    val sp = ScanningPredicateSerializer.parse(r)
    ExternalApplication(appId, appName, sp)
  }
}

object ExternalApplicationJsonCodecs extends ApiCodecs {
  import ScanningPredicateJsonCodecs._
  import io.circe._, io.circe.generic.semiauto._

  implicit val appReqDecoder: Decoder[ExternalAppRequest] = deriveDecoder[ExternalAppRequest]
  implicit val appReqEncoder: Encoder[ExternalAppRequest] = deriveEncoder[ExternalAppRequest]

  implicit val appDecoder: Decoder[ExternalApplication] = deriveDecoder[ExternalApplication]
  implicit val appEncoder: Encoder[ExternalApplication] = deriveEncoder[ExternalApplication]
}