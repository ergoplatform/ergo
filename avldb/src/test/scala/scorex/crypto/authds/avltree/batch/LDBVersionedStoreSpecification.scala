package scorex.crypto.authds.avltree.batch

import com.google.common.primitives.Longs
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalatest.{Matchers, PropSpec}
import scorex.crypto.authds.avltree.batch.helpers.TestHelper
import scorex.crypto.hash.Blake2b256
import scorex.db.LDBVersionedStore

import scala.collection.mutable.ArrayBuffer

class LDBVersionedStoreSpecification extends PropSpec
  with GeneratorDrivenPropertyChecks
  with Matchers
  with TestHelper {

  override protected val KL = 32
  override protected val VL = 8
  override protected val LL = 32

  val storeTest: LDBVersionedStore => Unit = { store =>
    var version = store.lastVersionID
    val keys: ArrayBuffer[(Array[Byte], Array[Byte])] = ArrayBuffer()
    forAll { b: Array[Byte] =>
      val pair = (Blake2b256(0.toByte +: version.map(_.head).getOrElse(0: Byte) +: b),
        Blake2b256(version.map(_.head).getOrElse(0: Byte) +: b))
      keys += pair
      val nextVersion = Longs.toByteArray(version.map(Longs.fromByteArray).getOrElse(0L) + 1)
      store.update(nextVersion, Seq(), Seq(pair))

      if (version.isDefined) {
        store.rollbackTo(version.get)
        store.update(nextVersion, Seq(), Seq(pair))
      }
      version = Some(nextVersion)
      keys.foreach(k => store(k._1).sameElements(k._2) shouldBe true)
    }
  }

  property("LDBVersionedStore") { storeTest(createVersionedStore()) }

}
