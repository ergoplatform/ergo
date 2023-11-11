package org.ergoplatform

/** Exception that triggers proper system shutdown */
case class CriticalSystemException(message: String) extends Exception(message)