package scorex.core.serialization

/**
  * Basic interface for objects which can be represented as bytes
  */
trait BytesSerializable extends Serializable {

  type M >: this.type <: BytesSerializable

  def bytes: Array[Byte] = serializer.toBytes(this)

  def serializer: ErgoSerializer[M]
}
