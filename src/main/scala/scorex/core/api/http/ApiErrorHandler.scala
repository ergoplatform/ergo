package scorex.core.api.http

import akka.http.scaladsl.model.IllegalRequestException
import akka.http.scaladsl.server.ExceptionHandler
import org.ergoplatform.http.api.ApiError
import scorex.util.ScorexLogging

import scala.util.control.NonFatal

object ApiErrorHandler extends ScorexLogging {

  /**
   * Sanitizes error messages by removing non-printable characters and truncating long strings.
   * This prevents log pollution from malformed HTTP requests with non-printable characters.
   */
  private def sanitizeErrorMessage(msg: String, maxLength: Int = 200): String = {
    val printable = msg.filter(c => c >= 32 && c < 127 || c == '\n' || c == '\r' || c == '\t')
    if (printable.length > maxLength) {
      printable.take(maxLength) + "... (truncated)"
    } else {
      printable
    }
  }

  implicit val exceptionHandler: ExceptionHandler = ExceptionHandler {
    case e: IllegalRequestException =>
      // Handle malformed HTTP requests (e.g., "HTTP method too long") with sanitized logging
      val sanitizedMsg = sanitizeErrorMessage(e.getMessage)
      log.debug(s"Illegal request rejected: $sanitizedMsg")
      ApiError.BadRequest(s"Malformed HTTP request: $sanitizedMsg")
    case NonFatal(e) => 
      ApiError(e)
  }
}
