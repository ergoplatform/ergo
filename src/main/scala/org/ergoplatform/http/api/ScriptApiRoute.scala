package org.ergoplatform.http.api

import akka.actor.{ActorRef, ActorRefFactory}
import akka.http.scaladsl.server.{Directive1, Route}
import akka.pattern.ask
import io.circe.syntax._
import io.circe.{Encoder, Json}
import org.ergoplatform._
import org.ergoplatform.http.api.ApiError.BadRequest
import org.ergoplatform.http.api.requests.{CryptoResult, ExecuteRequest}
import org.ergoplatform.nodeView.ErgoReadersHolder.{GetReaders, Readers}
import org.ergoplatform.nodeView.wallet.requests.PaymentRequestDecoder
import org.ergoplatform.settings.{ErgoSettings, RESTApiSettings}
import scorex.core.api.http.ApiResponse
import scorex.util.encode.Base16
import sigma.VersionContext
import sigma.ast.{ByteArrayConstant, ErgoTree, SBoolean, SSigmaProp, Value}
import sigma.compiler.{CompilerResult, SigmaCompiler}
import sigma.compiler.ir.CompiletimeIRContext
import sigma.data.ProveDlog
import sigma.serialization.ValueSerializer
import sigmastate.interpreter.Interpreter

import scala.concurrent.duration._
import scala.util.{Failure, Success, Try}


case class ScriptApiRoute(readersHolder: ActorRef, ergoSettings: ErgoSettings)
                         (implicit val context: ActorRefFactory) extends ErgoBaseApiRoute with ApiCodecs with ApiRequestsCodecs {

  implicit val paymentRequestDecoder: PaymentRequestDecoder = new PaymentRequestDecoder(ergoSettings)
  implicit val addressEncoder: ErgoAddressEncoder = ErgoAddressEncoder(ergoSettings.chainSettings.addressPrefix)
  implicit val addressJsonEncoder: Encoder[ErgoAddress] = paymentRequestDecoder.addressEncoders.encoder

  val settings: RESTApiSettings = ergoSettings.scorexSettings.restApi

  override val route: Route = pathPrefix("script") {
    toStrictEntity(10.seconds) {
      p2shAddressR ~
        p2sAddressR ~
        addressToTreeR ~
        addressToBytesR ~
        executeWithContextR
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

  private def compileSource(source: String, env: Map[String, Any], treeVersion: Int = 0): Try[ErgoTree] = {
    val compiler = new SigmaCompiler(ergoSettings.chainSettings.addressPrefix)
    val ergoTreeHeader = ErgoTree.defaultHeaderWithVersion(treeVersion.toByte)
    Try(compiler.compile(env, source)(new CompiletimeIRContext)).flatMap {
      case CompilerResult(_, _, _, script: Value[SSigmaProp.type@unchecked]) if script.tpe == SSigmaProp =>
        Success(ErgoTree.fromProposition(ergoTreeHeader, script))
      case CompilerResult(_, _, _, script: Value[SBoolean.type@unchecked]) if script.tpe == SBoolean =>
        Success(ErgoTree.fromProposition(ergoTreeHeader, script.toSigmaProp))
      case other =>
        Failure(new Exception(s"Source compilation result is of type ${other.buildTree.tpe}, but `SBoolean` expected"))
    }
  }

  private def withWalletAndStateOp[T](op: (Readers) => T)(toRoute: T => Route): Route = {
    onSuccess((readersHolder ? GetReaders).mapTo[Readers].map(r => op(r)))(toRoute)
  }

  // todo: unite p2sAddress and p2shAddress
  def p2sAddressR: Route = (path("p2sAddress") & post & source) { source =>
    withWalletAndStateOp(r => (r.w.publicKeys(0, loadMaxKeys), r.s.stateContext)) { case (addrsF, sc) =>
      onSuccess(addrsF) { addrs =>
        VersionContext.withScriptVersion(VersionContext.fromBlockVersion(sc.blockVersion).activatedVersion) {
          // todo: treeVersion == 1 is used here, revisit, likely 0 should be default for now
          compileSource(source, keysToEnv(addrs.map(_.pubkey)), 1).map(Pay2SAddress.apply).fold(
            e => BadRequest(e.getMessage),
            address => ApiResponse(addressResponse(address))
          )
        }
      }
    }
  }

  def p2shAddressR: Route = (path("p2shAddress") & post & source) { source =>
    withWalletAndStateOp(r => (r.w.publicKeys(0, loadMaxKeys), r.s.stateContext.blockVersion)) { case (addrsF, bv) =>
      onSuccess(addrsF) { addrs =>
        VersionContext.withScriptVersion(VersionContext.fromBlockVersion(bv).activatedVersion) {
          compileSource(source, keysToEnv(addrs.map(_.pubkey))).map(Pay2SHAddress.apply).fold(
            e => BadRequest(e.getMessage),
            address => ApiResponse(addressResponse(address))
          )
        }
      }
    }
  }

  def executeWithContextR: Route =
    (path("executeWithContext") & post & entity(as[ExecuteRequest])) { req =>
      compileSource(req.script, req.env).fold(
        e => BadRequest(e.getMessage),
        tree => {
          val interpreter: ErgoLikeInterpreter = new ErgoLikeInterpreter()
          val res = Try(interpreter.fullReduction(tree, req.ctx.asInstanceOf[interpreter.CTX], Interpreter.emptyEnv))
          res.fold(
            e => BadRequest(e.getMessage),
            s => ApiResponse(CryptoResult(s.value, s.cost).asJson)
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

  def addressToBytesR: Route = (get & path("addressToBytes" / Segment)) { addressStr =>
    addressEncoder.fromString(addressStr)
      .map(address => address.script.bytes)
      .map(ByteArrayConstant.apply)
      .map(ValueSerializer.serialize)
      .map(Base16.encode)
      .fold(
        e => BadRequest(e.getMessage),
        bs => ApiResponse(Map("bytes" -> bs).asJson)
      )
  }

}
