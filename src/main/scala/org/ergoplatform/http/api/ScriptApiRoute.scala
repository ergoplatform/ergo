package org.ergoplatform.http.api

import java.math.BigInteger

import akka.actor.{ActorRef, ActorRefFactory}
import akka.http.scaladsl.server.{Directive1, Route}
import akka.pattern.ask
import io.circe.generic.auto._
import io.circe.syntax._
import io.circe.{Decoder, DecodingFailure, Encoder, HCursor, Json}
import org.bouncycastle.util.BigIntegers
import org.ergoplatform.nodeView.ErgoReadersHolder.{GetReaders, Readers}
import org.ergoplatform.nodeView.wallet.ErgoWalletReader
import org.ergoplatform.nodeView.wallet.requests.PaymentRequestDecoder
import org.ergoplatform.settings.ErgoSettings
import org.ergoplatform._
import org.ergoplatform.wallet.interpreter.ErgoProvingInterpreter
import scorex.core.api.http.ApiError.BadRequest
import scorex.core.api.http.ApiResponse
import scorex.core.settings.RESTApiSettings
import scorex.util.encode.Base16
import sigmastate.Values.{ErgoTree, SigmaBoolean}
import sigmastate._
import sigmastate.basics.DLogProtocol.{FirstDLogProverMessage, ProveDlog}
import sigmastate.basics.ProveDHTuple
import sigmastate.eval.{CompiletimeIRContext, IRContext, RuntimeIRContext}
import sigmastate.interpreter.CryptoConstants
import sigmastate.interpreter.CryptoConstants.{EcPointType, dlogGroup, secureRandom}
import sigmastate.lang.SigmaCompiler
import sigmastate.serialization.OpCodes
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
      // p2shAddressR ~
      p2sAddressR ~
      addressToTreeR ~
      executeWithContextR ~
      generateCommitmentR
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

  //todo: temporarily switched off due to https://github.com/ergoplatform/ergo/issues/936
  def p2shAddressR: Route = (path("p2shAddress") & post & source) { source =>
    withWalletOp(_.publicKeys(0, loadMaxKeys)) { addrs =>
      compileSource(source, keysToEnv(addrs.map(_.pubkey))).map(Pay2SHAddress.apply).fold(
        e => BadRequest(e.getMessage),
        address => ApiResponse(addressResponse(address))
      )
    }
  }


 /**
   * Represent a request for execution of a script in a given context.
   * @param script  ErgoScript source code of the contract to execute
   * @param env      environment map of named constants used to compile the script
   * @param ctx      script execution context
   */
  case class ExecuteRequest(script: String,
                            env: Map[String,Any],
                            ctx: ErgoLikeContext)


  class ExecuteRequestDecoder(settings: ErgoSettings) extends Decoder[ExecuteRequest] with JsonCodecs {
    def apply(cursor: HCursor): Decoder.Result[ExecuteRequest] = {
      for {
        script <- cursor.downField("script").as[String]
        env <- cursor.downField("namedConstants").as[Map[String,AnyValue]]
        ctx <- cursor.downField("context").as[ErgoLikeContext]
      } yield ExecuteRequest(script, env.map({ case (k,v) => k -> v.value }), ctx)
    }
  }

  implicit val executeRequestDecoder: ExecuteRequestDecoder = new ExecuteRequestDecoder(ergoSettings)

  implicit val sigmaBooleanEncoder: Encoder[SigmaBoolean] = {
    sigma =>
      val op = sigma.opCode.toByte.asJson
      sigma match {
        case dlog: ProveDlog   => Map("op" -> op, "h" -> dlog.h.asJson).asJson
        case dht: ProveDHTuple => Map("op" -> op, "g" -> dht.g.asJson, "h" -> dht.h.asJson, "u" -> dht.u.asJson, "v" -> dht.v.asJson).asJson
        case tp: TrivialProp   => Map("op" -> op, "condition" -> tp.condition.asJson).asJson
        case and: CAND =>
          Map("op" -> op, "args" -> and.sigmaBooleans.map(_.asJson).asJson).asJson
        case or: COR =>
          Map("op" -> op, "args" -> or.sigmaBooleans.map(_.asJson).asJson).asJson
        case th: CTHRESHOLD =>
          Map("op" -> op, "args" -> th.sigmaBooleans.map(_.asJson).asJson).asJson
      }
  }

  implicit val sigmaBooleanDecoder: Decoder[SigmaBoolean] = Decoder.instance { c =>
    c.downField("op").as[Byte].flatMap {
      case b: Byte if b == OpCodes.ProveDlogCode =>
        c.downField("h").as[EcPointType].map(h => ProveDlog(h))
      case _ =>
        //only dlog is supported for now
        Left(DecodingFailure("Unsupported value", List()))
    }
  }

  implicit val cryptResultEncoder: Encoder[CryptoResult] = {
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

  def generateCommitmentR: Route = (path("generateCommitment") & post & entity(as[SigmaBoolean])) { sigma =>
    val (r, a) = ErgoProvingInterpreter.generateCommitmentFor(sigma)
    ApiResponse(Map("r" -> r.asJson, "a" -> a.asInstanceOf[FirstDLogProverMessage].ecData.asJson).asJson)
  }

}


object Generate extends App with ApiCodecs {
  import CryptoConstants.dlogGroup

  implicit val sigmaBooleanEncoder: Encoder[SigmaBoolean] = {
    sigma =>
      val op = Base16.encode(Array(sigma.opCode.toByte)).asJson
      sigma match {
        case dlog: ProveDlog   => Map("op" -> op, "h" -> dlog.h.asJson).asJson
        case dht: ProveDHTuple => Map("op" -> op, "g" -> dht.g.asJson, "h" -> dht.h.asJson, "u" -> dht.u.asJson, "v" -> dht.v.asJson).asJson
        case tp: TrivialProp   => Map("op" -> op, "condition" -> tp.condition.asJson).asJson
        case and: CAND =>
          Map("op" -> op, "args" -> and.sigmaBooleans.map(_.asJson).asJson).asJson
        case or: COR =>
          Map("op" -> op, "args" -> or.sigmaBooleans.map(_.asJson).asJson).asJson
        case th: CTHRESHOLD =>
          Map("op" -> op, "args" -> th.sigmaBooleans.map(_.asJson).asJson).asJson
      }
  }

  val qMinusOne = dlogGroup.order.subtract(BigInteger.ONE)
  val r = BigIntegers.createRandomInRange(BigInteger.ZERO, qMinusOne, secureRandom)
  val a = dlogGroup.exponentiate(dlogGroup.generator, r)
  r -> FirstDLogProverMessage(a)

  println(sigmaBooleanEncoder.apply(ProveDlog(a)))
  println(sigmaBooleanEncoder.apply(CAND(Seq(ProveDlog(a), ProveDlog(a)))))
  println(sigmaBooleanEncoder.apply(CAND(Seq(ProveDlog(a), ProveDlog(a), ProveDlog(a)))))
}
