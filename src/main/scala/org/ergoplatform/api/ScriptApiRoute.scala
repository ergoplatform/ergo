package org.ergoplatform.api

import akka.actor.{ActorRef, ActorRefFactory}
import akka.http.scaladsl.server.{Directive1, Route}
import akka.pattern.ask
import io.circe.Decoder.Result
import io.circe.generic.auto._
import io.circe.syntax._
import io.circe.{Decoder, Encoder, HCursor, Json}
import org.ergoplatform.nodeView.ErgoReadersHolder.{GetReaders, Readers}
import org.ergoplatform.nodeView.wallet.ErgoWalletReader
import org.ergoplatform.nodeView.wallet.requests.PaymentRequestDecoder
import org.ergoplatform.settings.ErgoSettings
import org.ergoplatform._
import scorex.core.api.http.ApiError.BadRequest
import scorex.core.api.http.ApiResponse
import scorex.core.settings.RESTApiSettings
import scorex.util.encode.Base16
import sigmastate.Values.{ErgoTree, SigmaBoolean}
import sigmastate.basics.DLogProtocol.ProveDlog
import sigmastate.basics.ProveDHTuple
import sigmastate.eval.{CompiletimeIRContext, IRContext, RuntimeIRContext}
import sigmastate.lang.SigmaCompiler
import sigmastate.serialization.DataJsonEncoder
import sigmastate._
import special.sigma.AnyValue

import scala.concurrent.Future
import scala.concurrent.duration._
import scala.util.{Failure, Success, Try}

case class CryptoResult(value: SigmaBoolean, cost: Long)

case class ScriptApiRoute(readersHolder: ActorRef, ergoSettings: ErgoSettings)
                         (implicit val context: ActorRefFactory) extends ErgoBaseApiRoute with ApiCodecs {

  implicit val paymentRequestDecoder: PaymentRequestDecoder = new PaymentRequestDecoder(ergoSettings)
  implicit val addressEncoder: ErgoAddressEncoder = ErgoAddressEncoder(ergoSettings.chainSettings.addressPrefix)
  implicit val addressJsonEncoder: Encoder[ErgoAddress] = paymentRequestDecoder.addressEncoders.encoder

  val settings: RESTApiSettings = ergoSettings.scorexSettings.restApi

  override val route: Route = pathPrefix("script") {
    toStrictEntity(10.seconds) {
      corsHandler {
        p2shAddressR ~
        p2sAddressR ~
        addressToTreeR ~
        executeWithContextR
      }
    }
  }

  private val loadMaxKeys: Int = 100

  private val source: Directive1[String] = entity(as[Json]).flatMap { p =>
    p.hcursor.downField("source").as[String]
      .fold(_ => reject, s => provide(s))
  }

  private def addressResponse(address: ErgoAddress): Json = Json.obj("address" -> addressJsonEncoder(address))

  private def keysToEnv(keys: Seq[ProveDlog]): Map[String, Any] = {
    keys.zipWithIndex.map { case (pk, i) => s"myPubKey_$i" -> pk }.toMap
  }

  private def compileSource(source: String, env: Map[String, Any]): Try[ErgoTree] = {
    import sigmastate.Values._
    val compiler = SigmaCompiler(ergoSettings.chainSettings.addressPrefix)
    Try(compiler.compile(env, source)(new CompiletimeIRContext)).flatMap {
      case script: Value[SSigmaProp.type@unchecked] if script.tpe == SSigmaProp =>
        Success(script)
      case script: Value[SBoolean.type@unchecked] if script.tpe == SBoolean =>
        Success(script.toSigmaProp)
      case other =>
        Failure(new Exception(s"Source compilation result is of type ${other.tpe}, but `SBoolean` expected"))
    }
  }

  private def withWalletOp[T](op: ErgoWalletReader => Future[T])(toRoute: T => Route): Route = {
    onSuccess((readersHolder ? GetReaders).mapTo[Readers].flatMap(r => op(r.w)))(toRoute)
  }

  def p2sAddressR: Route = (path("p2sAddress") & post & source) { source =>
    withWalletOp(_.publicKeys(0, loadMaxKeys)) { addrs =>
      compileSource(source, keysToEnv(addrs.map(_.pubkey))).map(Pay2SAddress.apply).fold(
        e => BadRequest(e.getMessage),
        address => ApiResponse(addressResponse(address))
      )
    }
  }

  def p2shAddressR: Route = (path("p2shAddress") & post & source) { source =>
    withWalletOp(_.publicKeys(0, loadMaxKeys)) { addrs =>
      compileSource(source, keysToEnv(addrs.map(_.pubkey))).map(Pay2SHAddress.apply).fold(
        e => BadRequest(e.getMessage),
        address => ApiResponse(addressResponse(address))
      )
    }
  }


  case class ExecuteRequest(script: String,
                            env: Map[String,Any],
                            ctx: ErgoLikeContext)


  implicit final val decodeMap: Decoder[Any] = new Decoder[Any] {
    final def apply(c: HCursor): Result[Any] = {
      val x = DataJsonEncoder.decode(c.value)
      Right(x)
    }
  }

  class ExecuteRequestDecoder(settings: ErgoSettings) extends Decoder[ExecuteRequest] with JsonCodecs {
    def apply(cursor: HCursor): Decoder.Result[ExecuteRequest] = {
      for {
        script <- cursor.downField("script").as[String]
        env <- cursor.downField("env").as[Map[String,Any]]
        ctx <- cursor.downField("ctx").as[ErgoLikeContext]
      } yield ExecuteRequest(script, env.asInstanceOf[Map[String,AnyValue]], ctx)
    }
  }

  implicit val executeRequestDecoder: ExecuteRequestDecoder = new ExecuteRequestDecoder(ergoSettings)

  implicit def sigmaBooleanEncoder: Encoder[SigmaBoolean] = {
    sigma =>
      val op = sigma.opCode.toByte.asJson
      sigma match {
        case dlog: ProveDlog   => Map("op" -> op, "h"->dlog.h.asJson).asJson
        case dht: ProveDHTuple => Map("op" -> op, "g"->dht.g.asJson, "h"->dht.h.asJson, "u"->dht.u.asJson, "v"->dht.v.asJson).asJson
        case tp: TrivialProp => Map("op" -> op, "condition"->tp.condition.asJson).asJson
        case and: CAND =>
          Map("op" -> op, "and" -> and.sigmaBooleans.map(_.asJson).asJson).asJson
        case or: COR =>
          Map("op" -> op, "or" -> or.sigmaBooleans.map(_.asJson).asJson).asJson
        case th: CTHRESHOLD =>
          Map("op" -> op, "threshold" -> th.sigmaBooleans.map(_.asJson).asJson).asJson
      }
  }

  implicit def cryptResultEncoder: Encoder[CryptoResult] = {
    res =>
      val fields = Map(
        "value" -> res.value.asJson,
        "cost" -> res.cost.asJson
      )
      fields.asJson
  }

  def executeWithContextR: Route =
    (path("executeWithContext") & post & entity(as[ExecuteRequest])) { req =>
      compileSource(req.script, req.env).fold(
        e => BadRequest(e.getMessage),
        tree => {
          implicit val irc : IRContext = new RuntimeIRContext()
          val interpreter : ErgoLikeInterpreter = new ErgoLikeInterpreter()
          val prop = interpreter.propositionFromErgoTree(tree, req.ctx.asInstanceOf[interpreter.CTX])
          val res = interpreter.reduceToCrypto(req.ctx.asInstanceOf[interpreter.CTX], prop)
          res.fold(
            e => BadRequest(e.getMessage),
            s => ApiResponse(CryptoResult(s._1, s._2).asJson)
          )
        }
      )
    }

  def addressToTreeR: Route = (get & path("addressToTree" / Segment)) { addressStr =>
    addressEncoder.fromString(addressStr)
      .map(address => address.script.bytes)
      .map(Base16.encode)
      .fold(
        e => BadRequest(e.getMessage),
        tree => ApiResponse(Map("tree" -> tree).asJson)
      )
  }

}
