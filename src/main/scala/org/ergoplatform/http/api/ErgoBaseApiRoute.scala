package org.ergoplatform.http.api

import akka.http.scaladsl.server.{Directive, Directive1, ValidationRejection}
import org.ergoplatform.settings.Algos
import scorex.core.api.http.ApiRoute
import scorex.util.{ModifierId, bytesToId}

import scala.concurrent.ExecutionContextExecutor
import scala.util.Success

trait ErgoBaseApiRoute extends ApiRoute {

  implicit val ec: ExecutionContextExecutor = context.dispatcher

  val modifierId: Directive1[ModifierId] = pathPrefix(Segment).flatMap(handleModifierId)

  val modifierIdGet: Directive1[ModifierId] = parameters("id".as[String])
    .flatMap(handleModifierId)

  private def handleModifierId(value: String): Directive1[ModifierId] =
    Algos.decode(value) match {
      case Success(bytes) => provide(bytesToId(bytes))
      case _ => reject(ValidationRejection("Wrong modifierId format"))
    }

}
