package org.ergoplatform

/**
  * Base exception for Ergo-specific errors.
  *
  * @param code    numeric error code identifying the error category
  * @param message human-readable description
  * @param cause   optional underlying throwable
  */
@SuppressWarnings(Array("org.wartremover.warts.Null"))
class ErgoException(val code: Int, message: String, cause: Option[Throwable] = None)
  extends Exception(message, cause.orNull)

object ErgoException {
  val ConfigError      = 1001
  val StateError       = 1002
  val NetworkError     = 1003
  val ValidationError  = 1004
  val WalletError      = 1005
  val UnknownTypeError = 1006
}
