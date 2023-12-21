package org.ergoplatform.modifiers.transaction


/**
  * A transaction is an atomic state modifier
  */
trait Signable {

  val messageToSign: Array[Byte]
  
}
