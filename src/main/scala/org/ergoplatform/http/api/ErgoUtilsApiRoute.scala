package org.ergoplatform.http.api

import akka.actor.ActorRefFactory
import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.server.Route
import io.circe.Json
import io.circe.syntax._
import org.ergoplatform.http.api.ApiError.{BadRequest, InternalError}
import org.ergoplatform.settings.{ErgoSettings, RESTApiSettings}
import org.ergoplatform.{ErgoAddressEncoder, P2PKAddress}
import scorex.core.api.http.{ApiResponse, ApiRoute}
import org.ergoplatform.utils.ScorexEncoding
import scorex.crypto.hash.Blake2b256
import scorex.util.encode.Base16
import sigma.data.ProveDlog

import java.security.SecureRandom
import scala.util.Failure
import sigma.serialization.{ErgoTreeSerializer, GroupElementSerializer, SigmaSerializer}
import akka.actor.ActorRef
import akka.pattern.ask
import org.ergoplatform.nodeView.ErgoReadersHolder.{GetReaders, Readers}

import scala.concurrent.duration._
import scala.concurrent.Await

class ErgoUtilsApiRoute(val readersHolder: ActorRef, val ergoSettings: ErgoSettings)(
  implicit val context: ActorRefFactory
) extends ApiRoute
  with ScorexEncoding {

  private val SeedSize = 32
  private val treeSerializer: ErgoTreeSerializer = new ErgoTreeSerializer

  override val settings: RESTApiSettings = ergoSettings.scorexSettings.restApi

  implicit val ergoAddressEncoder: ErgoAddressEncoder =
    new ErgoAddressEncoder(ergoSettings.chainSettings.addressPrefix)

  override val route: Route = pathPrefix("utils") {
    seedRoute ~
    length ~
    hashBlake2b ~
    rawToAddressR ~
    addressToRawR ~
    validateAddressPostR ~
    validateAddressGetR ~
    ergoTreeToAddressPostR ~
    ergoTreeToAddressGetR ~
    schnorrSignR
  }

  private def seed(length: Int): String = {
    val seed = new Array[Byte](length)
    new SecureRandom().nextBytes(seed) //seed mutated here!
    encoder.encode(seed)
  }

  def seedRoute: Route = (get & path("seed")) {
    ApiResponse(seed(SeedSize))
  }

  def length: Route = (get & path("seed" / IntNumber)) { length =>
    ApiResponse(seed(length))
  }

  def hashBlake2b: Route = {
    (post & path("hash" / "blake2b") & entity(as[Json])) { json =>
      json.as[String] match {
        case Right(message) => ApiResponse(encoder.encode(Blake2b256(message)))
        case Left(ex)       => ApiError(StatusCodes.BadRequest, ex.getMessage())
      }
    }
  }

  def rawToAddressR: Route = (get & path("rawToAddress" / Segment)) { pubKeyHex =>
    Base16
      .decode(pubKeyHex)
      .flatMap(pkBytes =>
        GroupElementSerializer.parseTry(SigmaSerializer.startReader(pkBytes))
      )
      .map(pkPoint => P2PKAddress(ProveDlog(pkPoint)))
      .fold(
        e => BadRequest(e.getMessage),
        address => ApiResponse(Map("address" -> address.toString().asJson).asJson)
      )
  }

  def addressToRawR: Route = (get & path("addressToRaw" / Segment)) { addressStr =>
    ergoAddressEncoder
      .fromString(addressStr)
      .map(address => address.contentBytes)
      .map(Base16.encode)
      .fold(
        e => BadRequest(e.getMessage),
        raw => ApiResponse(Map("raw" -> raw).asJson)
      )
  }

  private def ergoTreeToAddressResponse(ergoTreeHex: String) = {
    Base16
      .decode(ergoTreeHex)
      .flatMap { etBytes =>
        ergoAddressEncoder.fromProposition(treeSerializer.deserializeErgoTree(etBytes))
      }
      .fold(
        e => BadRequest(e.getMessage),
        address => ApiResponse(Map("address" -> address.toString.asJson).asJson)
      )
  }

  def ergoTreeToAddressGetR: Route = (get & path("ergoTreeToAddress" / Segment))(ergoTreeToAddressResponse)
  def ergoTreeToAddressPostR: Route = (post & path("ergoTreeToAddress") & entity(as[Json])) { json =>
    json.as[String] match {
      case Right(ergoTreeHex) => ergoTreeToAddressResponse(ergoTreeHex)
      case Left(ex)           => ApiError(StatusCodes.BadRequest, ex.getMessage())
    }
  }

  private def validateAddressResponse(addressStr: String) = {
    val address = ergoAddressEncoder.fromString(addressStr)
    val error = address match {
      case Failure(exception) => Map("error" -> exception.getMessage.asJson)
      case _ => Map()
    }

    val resp: Map[String, Json] = error ++ Map(
      "address" -> addressStr.asJson,
      "isValid" -> address.isSuccess.asJson
    )
    ApiResponse(resp.asJson)
  }

  def validateAddressGetR: Route = (get & path("address" / Segment))(validateAddressResponse)

  def validateAddressPostR: Route = (post & path("address") & entity(as[Json])) { json =>
    json.as[String] match {
      case Right(addressStr) => validateAddressResponse(addressStr)
      case Left(ex)          => ApiError(StatusCodes.BadRequest, ex.getMessage())
    }
  }

  def schnorrSignR: Route = (post & path("schnorrSign") & entity(as[Json])) { json =>

    import io.circe.generic.auto._

    // Define case class for the request (without derivation path)
    case class SchnorrSignRequest(address: String, message: String)

    json.as[SchnorrSignRequest] match {
      case Right(req) =>
        // Validate hex encoding of message
        scorex.util.encode.Base16.decode(req.message) match {
          case scala.util.Success(messageBytes) =>
            // Validate address format
            ergoAddressEncoder.fromString(req.address) match {
              case scala.util.Success(p2pkAddress: P2PKAddress) =>
                try {
                  // Access wallet to get the private key
                  val readersFuture = (readersHolder ? GetReaders).mapTo[Readers]
                  val readers = Await.result(readersFuture, 5.seconds)
                  val walletReader = readers.w

                  // Find the private key for the given address by looking up the public key
                  val extKeysFuture = walletReader.allExtendedPublicKeys()
                  val extKeys = Await.result(extKeysFuture, 5.seconds)

                  extKeys.find(_.key.value.equals(p2pkAddress.pubkey.value)) match {
                    case Some(extKey) =>
                      val path = extKey.path
                      // Get the private key for the derivation path
                      val privateKeyFuture = walletReader.getPrivateKeyFromPath(path)
                      val privateKeyTry = Await.result(privateKeyFuture, 5.seconds)

                      privateKeyTry match {
                        case scala.util.Success(privateKeyInput) =>
                          // Extract public key from private key
                          val publicKeyPoint = privateKeyInput.publicImage.value
                          val publicKeyBytes = GroupElementSerializer.toBytes(publicKeyPoint)

                          // Generate the Schnorr signature following the specification
                          import sigma.crypto.CryptoConstants
                          import scorex.crypto.hash.Blake2b256
                          import java.security.SecureRandom
                          import org.bouncycastle.util.BigIntegers

                          // Generate a random nonce
                          val secureRandom = new SecureRandom()
                          val kBytes = new Array[Byte](32)
                          secureRandom.nextBytes(kBytes)
                          val kBI = BigInt(BigIntegers.fromUnsignedByteArray(kBytes))

                          // Calculate a = g^k (random point)
                          val aPoint = CryptoConstants.dlogGroup.exponentiate(CryptoConstants.dlogGroup.generator, kBI.bigInteger)

                          // Calculate challenge e = H(a || message || public_key)
                          val aBytes = GroupElementSerializer.toBytes(aPoint)
                          val challengeInput = aBytes ++ messageBytes ++ publicKeyBytes
                          val eFull = Blake2b256(challengeInput)
                          val eBI = BigInt(BigIntegers.fromUnsignedByteArray(eFull)).mod(CryptoConstants.groupOrder)

                          // todo: remove before release
                          log.info(s"e: $eBI , challenge input: ${Base16.encode(challengeInput)}")

                          // Calculate response z = k + e * s (mod n) where s is the private key
                          val privateKeyBI = privateKeyInput.w
                          val zBI = kBI.bigInteger.add(eBI.bigInteger.multiply(privateKeyBI)).mod(CryptoConstants.groupOrder)

                          // Get the compressed form of R for the signature format
                          val aComponent = GroupElementSerializer.toBytes(aPoint)

                          val zComponent = BigIntegers.asUnsignedByteArray(32, zBI)

                          // todo: make .debug before release
                          log.info(s"For message ${req.message} a: ${Base16.encode(aComponent)} , z: ${Base16.encode(zComponent)} , e: $eBI")

                          val formattedSignature = aComponent ++ zComponent

                          val response = Json.obj(
                            "signedMessage" -> scorex.util.encode.Base16.encode(messageBytes).asJson,
                            "signature" -> scorex.util.encode.Base16.encode(formattedSignature).asJson,
                            "publicKey" -> scorex.util.encode.Base16.encode(publicKeyBytes).asJson
                          )
                          ApiResponse(response)

                        case scala.util.Failure(exception) =>
                          BadRequest(s"Node does not have the secret key for the specified address - ${exception.getMessage}")
                      }
                    case None =>
                      BadRequest("Node does not have the secret key for the specified address")
                  }
                } catch {
                  case _: Throwable =>
                    InternalError("WalletError")
                }

              case scala.util.Success(_) =>
                BadRequest("InvalidAddressType")

              case scala.util.Failure(_) =>
                BadRequest("InvalidAddress")
            }
          case scala.util.Failure(_) =>
            BadRequest("InvalidMessage")
        }
      case Left(ex) =>
        InternalError(ex.getMessage())
    }
  }

}

object ErgoUtilsApiRoute {

  def apply(
    readersHolder: ActorRef,
    ergoSettings: ErgoSettings
  )(implicit context: ActorRefFactory): ErgoUtilsApiRoute = {
    new ErgoUtilsApiRoute(readersHolder, ergoSettings)
  }

}
