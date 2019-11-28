package scorex.crypto.authds.avltree.batch

import com.google.common.primitives.Longs
import io.iohk.iodb.{ByteArrayWrapper, Store}
import org.scalatest.prop.{GeneratorDrivenPropertyChecks, PropertyChecks}
import org.scalatest.{Matchers, PropSpec}
import scorex.crypto.authds.avltree.batch.helpers.TestHelper
import scorex.crypto.hash.Blake2b256

import scala.collection.mutable.ArrayBuffer

class IODBStorageSpecification extends PropSpec
  with GeneratorDrivenPropertyChecks
  with Matchers
  with TestHelper {

  override protected val KL = 32
  override protected val VL = 8
  override protected val LL = 32

  val storeTest: Store => Unit = { store =>
    var version = store.lastVersionID.map(v => Longs.fromByteArray(v.data))
    val keys: ArrayBuffer[(ByteArrayWrapper, ByteArrayWrapper)] = ArrayBuffer()
    forAll { b: Array[Byte] =>
      val pair = (ByteArrayWrapper(Blake2b256(0.toByte +: version.getOrElse(0L).toByte +: b)),
        ByteArrayWrapper(Blake2b256(version.getOrElse(0L).toByte +: b)))
      keys += pair
      val nextVersion = version.getOrElse(0L) + 1
      store.update(nextVersion, Seq(), Seq(pair))

      if (version.isDefined) {
        store.rollback(ByteArrayWrapper.fromLong(version.get))
        store.update(nextVersion, Seq(), Seq(pair))
      }
      version = Some(nextVersion)
      keys.foreach(k => store(k._1).data shouldEqual k._2.data)
    }
  }

  property("IODB with LSM") { storeTest(createLSMStore()) }

  property("IODB with QuickStore") { quickTest(storeTest(createQuickStore())) }
}
