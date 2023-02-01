package scorex.core.app

import scorex.core.serialization.{BytesSerializable, ScorexSerializer}
import scorex.util.serialization._

/**
  * Version of p2p protocol. Node can only process messages of it's version or lower.
  */
case class Version(firstDigit: Byte, secondDigit: Byte, thirdDigit: Byte) extends BytesSerializable with Ordered[Version] {
  override type M = Version

  override def serializer: ScorexSerializer[Version] = ApplicationVersionSerializer

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

  val v4017: Version = Version(4, 0, 17)

  val v4018: Version = Version(4, 0, 18)

  val v4022: Version = Version(4, 0, 22)

  val v4043: Version = Version(4, 0, 43)

  val Eip37ForkVersion: Version = Version(4, 0, 100)
  val JitSoftForkVersion: Version = Version(5, 0, 0)

  val UtxoSnapsnotActivationVersion: Version = Version(5, 0, 6) // todo: set proper version around release

  val NipopowActivationVersion: Version = Version(5, 0, 6) // todo: set proper version around release
}

object ApplicationVersionSerializer extends ScorexSerializer[Version] {
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
