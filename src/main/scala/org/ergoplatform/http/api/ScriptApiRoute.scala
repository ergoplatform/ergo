package org.ergoplatform.http.api


import akka.actor.{ActorRef, ActorRefFactory}
import akka.http.scaladsl.server.{Directive1, Route}
import akka.pattern.ask
import io.circe.generic.auto._
import io.circe.syntax._
import io.circe.{Decoder, Encoder, HCursor, Json}
import org.ergoplatform.nodeView.ErgoReadersHolder.{GetReaders, Readers}
import org.ergoplatform.nodeView.wallet.ErgoWalletReader
import org.ergoplatform.nodeView.wallet.requests.{PaymentRequestDecoder, TransactionSigningRequest}
import org.ergoplatform.settings.{Constants, ErgoSettings}
import org.ergoplatform._
import org.ergoplatform.modifiers.mempool.{ErgoTransaction, UnsignedErgoTransaction}
import org.ergoplatform.nodeView.state.UtxoStateReader
import org.ergoplatform.wallet.interpreter.ErgoProvingInterpreter
import scorex.core.api.http.ApiError.BadRequest
import scorex.core.api.http.ApiResponse
import scorex.core.settings.RESTApiSettings
import scorex.crypto.authds.ADKey
import scorex.util.encode.Base16
import sigmastate.Values.{ErgoTree, SigmaBoolean}
import sigmastate._
import sigmastate.basics.DLogProtocol.{FirstDLogProverMessage, ProveDlog}
import sigmastate.eval.{CGroupElement, CompiletimeIRContext, IRContext, RuntimeIRContext}
import sigmastate.interpreter.RealCommitment
import sigmastate.lang.SigmaCompiler
import sigmastate.serialization.{GroupElementSerializer, ValueSerializer}
import special.sigma.AnyValue

import scala.concurrent.Future
import scala.concurrent.duration._
import scala.util.{Failure, Success, Try}

case class CryptoResult(value: SigmaBoolean, cost: Long)

case class ScriptApiRoute(readersHolder: ActorRef, ergoSettings: ErgoSettings)
                         (implicit val context: ActorRefFactory) extends ErgoBaseApiRoute with ApiCodecs {

  import org.ergoplatform.nodeView.wallet.requests.SigmaBooleanCodecs._
  import HintExtractionRequest._

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
      generateCommitmentR ~
      extractHintsR
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

  def extractHintsR: Route = (path("extractHints") & post & entity(as[HintExtractionRequest])) { her =>
    val tx = her.tx

    onSuccess((readersHolder ? GetReaders).mapTo[Readers].flatMap{readers =>
      val (boxesToSpend, dataBoxes) = readers.s match {
        case utxo: UtxoStateReader =>
          val bts = tx.inputs.map(_.boxId).flatMap(utxo.boxById)
          val db = tx.dataInputs.map(_.boxId).flatMap(utxo.boxById)
          (bts, db)
        case _ => ???
      }
      readers.w.extractHints(tx, boxesToSpend, dataBoxes, her.real, her.simulated)
    })(_ => ApiResponse(Map().asJson))
  }

}

case class HintExtractionRequest(tx: ErgoTransaction, real: Seq[SigmaBoolean], simulated: Seq[SigmaBoolean])

object HintExtractionRequest extends ApiCodecs {

  import org.ergoplatform.nodeView.wallet.requests.SigmaBooleanCodecs.{sigmaBooleanEncoder, sigmaBooleanDecoder}

  //cd0354efc32652cad6cf1231be987afa29a686af30b5735995e3ce51339c4d0ca380 , cd is op, 03... is ec point

  implicit val hintExtractionRequestEncoder: Encoder[HintExtractionRequest] = {hr =>
    Map(
      "transaction" -> hr.tx.asJson,
      "real" -> hr.real.asJson,
      "simulated" -> hr.simulated.asJson
    ).asJson
  }

  implicit val hintExtractionRequestDecoder: Decoder[HintExtractionRequest] = {cursor =>
    for {
      tx <- cursor.downField("transaction").as[ErgoTransaction]
      real <- cursor.downField("real").as[Seq[SigmaBoolean]]
      simulated <- cursor.downField("simulated").as[Seq[SigmaBoolean]]
    } yield HintExtractionRequest(tx, real, simulated)
  }

}


object ApiReqPlayground extends App with ApiCodecs {

  import io.circe.syntax._

  val pbs = Base16.decode("02b353df14cd94849c36194bba03000dafaeb91b3a425a863f5660565189ddfe8f").get
  val ge = GroupElementSerializer.parse(pbs)

  val vbs = ValueSerializer.serialize(Values.GroupElementConstant(CGroupElement(ge)))
  println(Base16.encode(vbs))

  println(Base16.decode("cd").get.head)

  val i0 = new UnsignedInput(ADKey @@ Base16.decode("3ad975c5d1fef7f5d28d4e891dd2aa469e02d3049617abc115aa58c1f91299c2").get)
  val out0 = new ErgoBoxCandidate(1000000000L, Constants.TrueLeaf, 200000)
  val ut = UnsignedErgoTransaction(IndexedSeq(i0), IndexedSeq.empty, IndexedSeq(out0))

  println(ut.asJson)

  val a = GroupElementSerializer.parse(Base16.decode("037146ed72259a19b262271c5a49ccc97e5d32ab83d1de0216b36c557d8f6b2d95").get)
  val cmt = FirstDLogProverMessage(a)
  val pdlog = ProveDlog(GroupElementSerializer.parse(Base16.decode("0354efc32652cad6cf1231be987afa29a686af30b5735995e3ce51339c4d0ca380").get))
  val tsr = TransactionSigningRequest(ut, Seq(RealCommitment(pdlog, cmt)), Seq.empty, None, None)

  val out = tsr.asJson.toString()
  println(out)
}
