package org.ergoplatform.modifiers.history


trait HeaderError { this: Exception =>
  def message: String
  def isFatal: Boolean
}

case class MalformedHeaderError(message: String) extends Exception(message) with HeaderError {
  def isFatal: Boolean = true
}

case class RecoverableHeaderError(message: String) extends Exception(message) with HeaderError {
  def isFatal: Boolean = false
}
