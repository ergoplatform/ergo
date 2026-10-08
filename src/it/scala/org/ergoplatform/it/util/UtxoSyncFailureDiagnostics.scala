package org.ergoplatform.it.util

import io.circe.Json

import java.util.concurrent.TimeoutException
import scala.concurrent.{Await, ExecutionContext, Future}
import scala.concurrent.duration._

/** Additional observations after failure, with their own bound and no effect on the assertion. */
object UtxoSyncFailureDiagnostics {
  private val requestBudget = 2.seconds
  private val collectionBudget = 2500.millis
  private val numericFields = Seq("headersHeight", "fullHeight", "headersScore", "fullBlocksScore", "peersCount")
  private val headerFields = Seq("bestHeaderId", "bestFullHeaderId", "genesisBlockId")

  def project(node: Int, sampledAt: Long, response: Either[String, Json]): Json = {
    val identity = Seq("node" -> Json.fromInt(node), "sampledAt" -> Json.fromLong(sampledAt))
    def failed(errorClass: String): Json = {
      val safeClass = if (errorClass.matches("[A-Za-z_$][A-Za-z0-9_$]{0,127}")) errorClass else "ObservationFailure"
      Json.obj((identity :+ ("errorClass" -> Json.fromString(safeClass))): _*)
    }
    response match {
      case Left(errorClass) => failed(errorClass)
      case Right(body) if !body.isObject => failed("InvalidInfoResponse")
      case Right(body) =>
        def field(name: String)(decode: Json => Option[Json]): (String, Json) = {
          val value = body.hcursor.downField(name).focus match {
            case None => Json.Null
            case Some(value) if value.isNull => Json.Null
            case Some(value) => decode(value).getOrElse(Json.fromString("invalid"))
          }
          name -> value
        }
        val numbers = numericFields.map(name => field(name) { value =>
          value.asNumber.flatMap(_.toBigInt).filter(_ >= 0).map(Json.fromBigInt)
        })
        val headers = headerFields.map(name => field(name) { value =>
          value.asString.map(id => Json.fromString(ConvergenceObservations.headerId(id)))
        })
        val mining = field("isMining")(_.asBoolean.map(Json.fromBoolean))
        Json.obj((identity ++ numbers ++ headers :+ mining): _*)
    }
  }

  def rethrowAfterCapture(original: TimeoutException, requests: Seq[() => Future[Json]])
                         (emit: String => Unit)(implicit ec: ExecutionContext): Unit = {
    try {
      val observations = new ConvergenceObservations
      try {
        val snapshots = requests.zipWithIndex.map { case (request, index) =>
          observations.probe(Future(request()).flatMap(response => response))
            .sample(requestBudget).map(response => project(index, System.currentTimeMillis(), response))
        }
        // This bound starts only after the synchronization assertion has already failed.
        val result = Await.result(Future.sequence(snapshots), collectionBudget)
        emit(Json.arr(result: _*).noSpaces)
      } finally observations.close()
    } finally throw original
  }
}
