package org.ergoplatform.network

import org.ergoplatform.core.BytesSerializable
import org.ergoplatform.serialization.ErgoSerializer
import scorex.util.serialization._

/**
  * Version of p2p protocol. Node can only process messages of it's version or lower.
  */
case class Version(firstDigit: Byte, secondDigit: Byte, thirdDigit: Byte) extends BytesSerializable with Ordered[Version] {
  override type M = Version

  override def serializer: ErgoSerializer[Version] = ApplicationVersionSerializer

  override def compare(that: Version): Int = {
    if (this.firstDigit != that.firstDigit) {
      this.firstDigit - that.firstDigit
    } else if (this.secondDigit != that.secondDigit) {
      this.secondDigit - that.secondDigit
    } else {
      this.thirdDigit - that.thirdDigit
    }
  }

  override def toString: String = s"$firstDigit.$secondDigit.$thirdDigit"
}

object Version {

  def apply(v: String): Version = {
    val splitted = v.split("\\.")
    Version(splitted(0).toByte, splitted(1).toByte, splitted(2).toByte)
  }

  val initial: Version = Version(0, 0, 1)

  val Eip37ForkVersion: Version = Version(4, 0, 100)

  val SubblocksVersion: Version = Version(6, 0, 0)  // todo: set to proper value before activation, to send input block related messages only to peers able to parse them

  val UtxoSnapsnotActivationVersion: Version = Version(5, 0, 12)

  val NipopowActivationVersion: Version = Version(5, 0, 13)
}

object ApplicationVersionSerializer extends ErgoSerializer[Version] {
  val SerializedVersionLength: Int = 3


  override def serialize(obj: Version, w: Writer): Unit = {
    w.put(obj.firstDigit)
    w.put(obj.secondDigit)
    w.put(obj.thirdDigit)
  }

  override def parse(r: Reader): Version = {
    Version(
      r.getByte(),
      r.getByte(),
      r.getByte()
    )
  }
}
