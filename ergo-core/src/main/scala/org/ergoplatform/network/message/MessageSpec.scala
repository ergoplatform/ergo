package org.ergoplatform.network.message

import org.ergoplatform.network.Version
import org.ergoplatform.network.message.MessageConstants._
import org.ergoplatform.serialization.ErgoSerializer

/**
  * Base trait for app p2p messages in the network
  */
trait MessageSpec[Content] extends ErgoSerializer[Content] {

  /**
    * The p2p protocol version in which this message type first appeared
    */
  val protocolVersion: Version

  /**
    * Code which identifies what message type is contained in the payload
    */

  val messageCode: MessageCode

  /**
    * Name of this message type. For debug purposes only.
    */
  val messageName: String

  override def toString: String = s"MessageSpec($messageCode: $messageName)"
}

/**
  * P2p messages, that where implemented before sub-blocks
  */
trait MessageSpecInitial[Content] extends MessageSpec[Content] {

  override val protocolVersion: Version = Version.initial

}


/**
  * Sub-blocks related messages, V2 of the protocol
  */
trait MessageSpecInputBlocks[Content] extends MessageSpec[Content] {

  override val protocolVersion: Version = Version.SubblocksVersion

}
