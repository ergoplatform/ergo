package org.ergoplatform.nodeView.wallet

import io.circe.syntax._
import io.circe.{Decoder, DecodingFailure, Encoder}
import org.ergoplatform.settings.ErgoSettings
import org.ergoplatform.{ErgoAddress, ErgoAddressEncoder}

import scala.util.{Failure, Success}

case class ErgoAddressJsonEncoder(settings: ErgoSettings) {

  implicit val encoder: Encoder[ErgoAddress] = { address =>
    ErgoAddressEncoder(settings.chainSettings.addressPrefix).toString(address).asJson
  }

  implicit val decoder: Decoder[ErgoAddress] = { cursor =>
    def decodeString(addrStr: String) = {
      ErgoAddressEncoder(settings.chainSettings.addressPrefix).fromString(addrStr) match {
        case Success(address) => Right(address)
        case Failure(exception) => Left(DecodingFailure(exception.toString, cursor.history))
      }
    }

    for {
      addressStr <- cursor.as[String]
      address <- decodeString(addressStr)
    } yield address
  }
}

