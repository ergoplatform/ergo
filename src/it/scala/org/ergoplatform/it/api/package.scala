package org.ergoplatform.it

import org.asynchttpclient.Response

import scala.concurrent.{ExecutionContext, Future}

package object api {
  implicit class ResponseFutureExt(val f: Future[Response]) extends AnyVal {
    def as[A: Format](implicit ec: ExecutionContext): Future[A] = f.map(r => parse(r.getResponseBody).as[A])(ec)
  }
}
