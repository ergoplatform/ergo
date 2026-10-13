package scorex.core.api.http

import akka.http.scaladsl.server._
import org.ergoplatform.http.api.ApiError

object ApiRejectionHandler {

  /**
   * Sanitizes error messages by removing non-printable characters and truncating long strings.
   * This prevents log pollution from malformed HTTP requests.
   */
  private def sanitizeErrorMessage(msg: String, maxLength: Int = 200): String = {
    val printable = msg.filter(c => c >= 32 && c < 127 || c == '\n' || c == '\r' || c == '\t')
    if (printable.length > maxLength) {
      printable.take(maxLength) + "... (truncated)"
    } else {
      printable
    }
  }

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
        val sanitized = sanitizeErrorMessage(msg)
        ApiError.BadRequest("The request content was malformed:\n" + sanitized)
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
    .handle { case ValidationRejection(msg, _) => 
      val sanitized = sanitizeErrorMessage(msg)
      ApiError.BadRequest(sanitized) 
    }
    .handle { case x => 
      val sanitized = sanitizeErrorMessage(x.toString)
      ApiError.InternalError(s"Unhandled rejection: $sanitized") 
    }
    .handleNotFound { ApiError.BadRequest("The requested resource/endpoint could not be found.") }
    .result()
}
