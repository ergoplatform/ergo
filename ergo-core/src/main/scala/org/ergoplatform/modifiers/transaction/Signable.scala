package org.ergoplatform.modifiers.transaction


/**
  * A basic trait for entities which can be signed
  */
trait Signable {

  /**
    * Bytes to be signed
    */
  val messageToSign: Array[Byte]

}
