package org.ergoplatform.wallet.serialization

import java.nio.ByteBuffer

import scorex.util.ByteArrayBuilder
import scorex.util.serialization._

import scala.util.Try

trait ErgoWalletSerializer[T] extends Serializer[T, T, Reader, Writer] {

  def toBytes(obj: T): Array[Byte] = {
    val writer = new VLQByteBufferWriter(new ByteArrayBuilder())
    serialize(obj, writer)
    writer.result().toBytes
  }

  def parseBytes(bytes: Array[Byte]): T = {
    val reader = new VLQByteBufferReader(ByteBuffer.wrap(bytes))
    parse(reader)
  }

  def parseBytesTry(bytes: Array[Byte]): Try[T] = Try(parseBytes(bytes))

}
