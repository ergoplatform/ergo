package org.ergoplatform.api

import akka.http.scaladsl.server.{Directive, Directive1}
import scorex.core.ModifierId
import scorex.core.api.http.ApiRoute

import scala.util.Success

trait ErgoBaseApiRoute extends ApiRoute {

  implicit val ec = context.dispatcher

  val paging: Directive[(Int, Int)] = parameters("offset".as[Int] ? 0, "limit".as[Int] ? 50)

  val headerId: Directive1[ModifierId] = pathPrefix(Segment).flatMap { h =>
    Algos.decode(h) match {
      case Success(header) => provide(ModifierId @@ header)
      case _ => reject
    }
  }
}
