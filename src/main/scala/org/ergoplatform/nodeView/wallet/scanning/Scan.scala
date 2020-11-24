package org.ergoplatform.nodeView.wallet.scanning

import org.ergoplatform.http.api.ApiCodecs
import org.ergoplatform.nodeView.wallet.scanning.ScanWalletInteraction.ScanWalletInteraction
import org.ergoplatform.wallet.Constants.ScanId
import scorex.core.serialization.ScorexSerializer
import scorex.util.serialization.{Reader, Writer}

import scala.util.{Failure, Success, Try}



/**
  * Wraps information about user scan.
  *
  * A scan is providing scanning rules for specific boxes, and then work with boxes found by the node.
  *
  * @param scanId         - unique identifier of an scan in the local system
  * @param scanName       - scan description (255 bytes in UTF-8 encoding max)
  * @param trackingRule  - a predicate to scan the blockchain for specific scan-related boxes
  * @param walletInteraction - a flag which is prescribing how the scan is interacting with the p2pk-wallet
  */
case class Scan(scanId: ScanId,
                scanName: String,
                trackingRule: ScanningPredicate,
                walletInteraction: ScanWalletInteraction.Value)

object Scan {

  val MaxScanNameLength = 255

}

object ScanSerializer extends ScorexSerializer[Scan] {
  override def serialize(app: Scan, w: Writer): Unit = {
    w.putShort(app.scanId)
    w.putShortString(app.scanName)
    w.put(ScanWalletInteraction.toByte(app.walletInteraction))
    ScanningPredicateSerializer.serialize(app.trackingRule, w)
  }

  override def parse(r: Reader): Scan = {
    val scanId = ScanId @@ r.getShort()
    val appName = r.getShortString()

    // hack to read scans serialized with previous versions (they will have positive first byte)
    // for scans written with previous versions, walletInteraction flag is set to "off"
    val interactionFlag = r.peekByte() match {
      case x: Byte if x < 0 => r.getByte(); ScanWalletInteraction.fromByte(x)
      case _ => ScanWalletInteraction.Off
    }
    val sp = ScanningPredicateSerializer.parse(r)
    Scan(scanId, appName, sp, interactionFlag)
  }
}

/**
  * A class that encodes an API request to create a scan.
  *
  * @param scanName       - scan description (255 bytes in UTF-8 encoding max)
  * @param trackingRule  - a predicate to scan the blockchain for specific scan-related boxes
  * @param walletInteraction - how scan should interact with (p2pk) wallet, see @ScanWalletInteraction for details
  *
  */

case class ScanRequest(scanName: String,
                       trackingRule: ScanningPredicate,
                       walletInteraction: Option[ScanWalletInteraction]) {
  def toScan(scanId: ScanId): Try[Scan] = {
    if (scanName.getBytes("UTF-8").length > Scan.MaxScanNameLength) {
      Failure(new Exception(s"Too long scan name: $scanName"))
    } else {
      Success(Scan(scanId, scanName, trackingRule, walletInteraction.getOrElse(ScanWalletInteraction.Shared)))
    }
  }
}

object ScanJsonCodecs extends ApiCodecs {

  import ScanningPredicateJsonCodecs._
  import io.circe._, io.circe.generic.semiauto._

  implicit val intflagDecoder: Decoder[ScanWalletInteraction] = Decoder.enumDecoder(ScanWalletInteraction)
  implicit val intflagEncoder: Encoder[ScanWalletInteraction.Value] = Encoder.enumEncoder(ScanWalletInteraction)

  implicit val scanReqDecoder: Decoder[ScanRequest] = deriveDecoder[ScanRequest]
  implicit val scanReqEncoder: Encoder[ScanRequest] = deriveEncoder[ScanRequest]

  implicit val scanDecoder: Decoder[Scan] = deriveDecoder[Scan]
  implicit val scanEncoder: Encoder[Scan] = deriveEncoder[Scan]
}
