package org.ergoplatform.api

import akka.http.scaladsl.server.{Directive, Directive1, ValidationRejection}
import org.ergoplatform.settings.Algos
import scorex.core.api.http.ApiRoute
import scorex.util.{ModifierId, bytesToId}

import scala.concurrent.ExecutionContextExecutor
import scala.util.Success

trait ErgoBaseApiRoute extends ApiRoute {

  implicit val ec: ExecutionContextExecutor = context.dispatcher

  val paging: Directive[(Int, Int)] = parameters("offset".as[Int] ? 0, "limit".as[Int] ? 50)

  val modifierId: Directive1[ModifierId] = pathPrefix(Segment).flatMap { h =>
    Algos.decode(h) match {
      case Success(header) => provide(bytesToId(header))
      case _ => reject(ValidationRejection("Wrong modifierId format"))
    }
  }

}
