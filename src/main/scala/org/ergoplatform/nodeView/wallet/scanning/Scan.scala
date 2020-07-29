package org.ergoplatform.nodeView.wallet.scanning

import org.ergoplatform.http.api.ApiCodecs
import org.ergoplatform.nodeView.history.ErgoHistory.Height
import org.ergoplatform.wallet.Constants.ScanId
import scorex.core.serialization.ScorexSerializer
import scorex.util.serialization.{Reader, Writer}
import scorex.util.Extensions._

import scala.util.{Failure, Success, Try}

/**
  * Wraps information about user scan.
  *
  * A scan is providing scanning rules for specific boxes, and then work with boxes found by the node.
  *
  * @param scanId         - unique identifier of an scan in the local system
  * @param scanName       - scan description (255 bytes in UTF-8 encoding max)
  * @param trackingRule  - a predicate to scan the blockchain for specific scan-related boxes
  */
case class Scan(scanId: ScanId, scanName: String, trackingRule: ScanningPredicate, startingHeight: Height)

object Scan {

  val MaxScanNameLength = 255

}

/**
  * A class that encodes an API request to create a scan.
  *
  * @param scanName       - scan description (255 bytes in UTF-8 encoding max)
  * @param trackingRule  - a predicate to scan the blockchain for specific scan-related boxes
  *
  */

case class ScanRequest(scanName: String,
                       trackingRule: ScanningPredicate) {
  def toScan(scanId: ScanId, currentHeight: Height): Try[Scan] = {
    if (scanName.getBytes("UTF-8").length > Scan.MaxScanNameLength) {
      Failure(new Exception(s"Too long scan name: $scanName"))
    } else {
      Success(Scan(scanId, scanName, trackingRule, currentHeight))
    }
  }
}

object ScanSerializer extends ScorexSerializer[Scan] {
  override def serialize(app: Scan, w: Writer): Unit = {
    w.putShort(app.scanId)
    w.putShortString(app.scanName)
    w.putUInt(app.startingHeight)
    ScanningPredicateSerializer.serialize(app.trackingRule, w)
  }

  override def parse(r: Reader): Scan = {
    val scanId = ScanId @@ r.getShort()
    val appName = r.getShortString()
    val startingHeight = r.getUInt().toIntExact
    val sp = ScanningPredicateSerializer.parse(r)
    Scan(scanId, appName, sp, startingHeight)
  }
}

object ScanJsonCodecs extends ApiCodecs {

  import ScanningPredicateJsonCodecs._
  import io.circe._, io.circe.generic.semiauto._

  implicit val scanReqDecoder: Decoder[ScanRequest] = deriveDecoder[ScanRequest]
  implicit val scanReqEncoder: Encoder[ScanRequest] = deriveEncoder[ScanRequest]

  implicit val scanDecoder: Decoder[Scan] = deriveDecoder[Scan]
  implicit val scanEncoder: Encoder[Scan] = deriveEncoder[Scan]
}
