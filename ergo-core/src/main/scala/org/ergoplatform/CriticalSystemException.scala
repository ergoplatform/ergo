package org.ergoplatform

/** Exception that triggers system shutdown */
case class CriticalSystemException(message: String) extends Exception(message)
