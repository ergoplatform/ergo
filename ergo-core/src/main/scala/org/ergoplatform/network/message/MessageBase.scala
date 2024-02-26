package org.ergoplatform.network.message

import org.ergoplatform.network.message.MessageConstants._

import scala.util.{Success, Try}

/**
  * Trait for a ergo network message
  *
  * @param spec   - message specification
  * @param input  - message being wrapped, whether in byte-array form (if from outside),
  *               or structured data (if formed locally)
  * @tparam Content - message data type
  */
trait MessageBase[Content] {
  val spec: MessageSpec[Content]
  val input: Either[Array[Byte], Content]

  /**
    * Message data bytes
    */
  lazy val dataBytes: Array[Byte] = input match {
    case Left(db) => db
    case Right(d) => spec.toBytes(d)
  }

  /**
    * Structured message content
    */
  lazy val data: Try[Content] = input match {
    case Left(db) => spec.parseBytesTry(db)
    case Right(d) => Success(d)
  }

  lazy val dataLength: Int = dataBytes.length

  /**
    * @return serialized message length in bytes
    */
  def messageLength: Int = {
    if (dataLength > 0) {
      HeaderLength + ChecksumLength + dataLength
    } else {
      HeaderLength
    }
  }

}
