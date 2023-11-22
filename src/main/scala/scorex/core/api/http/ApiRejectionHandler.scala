package scorex.core.api.http

import akka.http.scaladsl.server._
import org.ergoplatform.http.api.ApiError

object ApiRejectionHandler {

  implicit val rejectionHandler: RejectionHandler = RejectionHandler.newBuilder()
    .handleAll[SchemeRejection] { rejections =>
      val schemes = rejections.map(_.supported).mkString(", ")
      ApiError.BadRequest(s"Uri scheme not allowed, supported schemes: $schemes")
    }
    .handle {
      case AuthorizationFailedRejection =>
        ApiError.Forbidden("The supplied authentication is not authorized to access this resource")
    }
    .handle {
      case MalformedRequestContentRejection(msg, _) =>
        ApiError.BadRequest("The request content was malformed:\n" + msg)
    }
    .handle {
      case InvalidOriginRejection(allowedOrigins) =>
        ApiError.Forbidden(s"Allowed `Origin` header values: ${allowedOrigins.mkString(", ")}")
    }
    .handle {
      case MissingQueryParamRejection(paramName) =>
        ApiError.NotExists(s"Request is missing required query parameter '$paramName'")
    }
    .handle {
      case RequestEntityExpectedRejection =>
        ApiError.BadRequest("Request entity expected but not supplied")
    }
    .handle { case ValidationRejection(msg, _) => ApiError.BadRequest(msg) }
    .handle { case x => ApiError.InternalError(s"Unhandled rejection: $x") }
    .handleNotFound { ApiError.BadRequest("The requested resource/endpoint could not be found.") }
    .result()
}
