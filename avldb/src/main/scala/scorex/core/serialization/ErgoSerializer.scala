package scorex.core.serialization

import java.nio.ByteBuffer
import scorex.util.ByteArrayBuilder
import scorex.util.serialization._

import scala.util.Try

/**
  * Basic interface for serializer with additional methods to work with bytes, not only Reader/Writer instances
  */
trait ErgoSerializer[T] extends Serializer[T, T, Reader, Writer] {

  /**
    * Serialize object `obj` to byte array
    */
  def toBytes(obj: T): Array[Byte] = {
    val writer = new VLQByteBufferWriter(new ByteArrayBuilder())
    serialize(obj, writer)
    writer.result().toBytes
  }


  /**
    * Deserialize byte array into object of type `T` (or throw exception)
    */
  def parseBytes(bytes: Array[Byte]): T = {
    val reader = new VLQByteBufferReader(ByteBuffer.wrap(bytes))
    parse(reader)
  }

  /**
    * Deserialize byte array into object of type `T` (or return Failure)
    */
  def parseBytesTry(bytes: Array[Byte]): Try[T] = {
    Try(parseBytes(bytes))
  }

}
