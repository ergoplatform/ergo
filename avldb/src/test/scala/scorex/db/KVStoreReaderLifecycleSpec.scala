package scorex.db

import java.lang.reflect.{InvocationHandler, Method, Proxy}
import java.util.AbstractMap.SimpleImmutableEntry

import org.iq80.leveldb.{DB, DBIterator, ReadOptions, Snapshot}
import org.scalatest.matchers.should.Matchers
import org.scalatest.propspec.AnyPropSpec

class KVStoreReaderLifecycleSpec extends AnyPropSpec with Matchers {

  private class Fixture {
    val key: Array[Byte] = Array(1.toByte)
    val value: Array[Byte] = Array(2.toByte)
    var iteratorFailure: Option[IllegalStateException] = None
    var iteratorCloseFailure: Option[IllegalStateException] = None
    var snapshotCloses = 0
    var iteratorCloses = 0
    var dbCloses = 0

    private var position = 0
    private val entry = new SimpleImmutableEntry[Array[Byte], Array[Byte]](key, value)

    private val snapshot: Snapshot = new Snapshot {
      override def close(): Unit = snapshotCloses += 1
    }

    private val iterator: DBIterator =
      Proxy.newProxyInstance(
        classOf[DBIterator].getClassLoader,
        Array(classOf[DBIterator]),
        new InvocationHandler {
          override def invoke(proxy: Any, method: Method, args: Array[AnyRef]): AnyRef =
            method.getName match {
              case "seek" | "seekToFirst" =>
                position = 0
                null
              case "hasNext" =>
                java.lang.Boolean.valueOf(position == 0)
              case "next" =>
                if (position != 0) throw new NoSuchElementException
                position = 1
                entry
              case "close" =>
                iteratorCloses += 1
                iteratorCloseFailure.foreach(throw _)
                null
              case other =>
                throw new UnsupportedOperationException(other)
            }
        }
      ).asInstanceOf[DBIterator]

    private val fakeDb: DB =
      Proxy.newProxyInstance(
        classOf[DB].getClassLoader,
        Array(classOf[DB]),
        new InvocationHandler {
          override def invoke(proxy: Any, method: Method, args: Array[AnyRef]): AnyRef =
            method.getName match {
              case "getSnapshot" =>
                snapshot
              case "iterator" if args != null && args.length == 1 =>
                val selectedSnapshot = args(0).asInstanceOf[ReadOptions].snapshot()
                selectedSnapshot should be theSameInstanceAs snapshot
                iteratorFailure.foreach(throw _)
                iterator
              case "close" =>
                dbCloses += 1
                null
              case other =>
                throw new UnsupportedOperationException(other)
            }
        }
      ).asInstanceOf[DB]

    val reader: KVStoreReader = new KVStoreReader {
      override protected val db: DB = fakeDb
    }
  }

  private val reads: Seq[(String, Fixture => Seq[(Array[Byte], Array[Byte])])] = Seq(
    "getWithFilter" -> ((f: Fixture) => f.reader.getWithFilter((_, _) => true).toSeq),
    "getRange" -> ((f: Fixture) => f.reader.getRange(f.key, f.key).toSeq)
  )

  reads.foreach { case (name, read) =>
    property(s"$name closes its iterator and snapshot after a healthy read") {
      val f = new Fixture
      val rows = read(f)

      rows.length shouldBe 1
      rows.head._1.toSeq shouldBe f.key.toSeq
      rows.head._2.toSeq shouldBe f.value.toSeq
      f.iteratorCloses shouldBe 1
      f.snapshotCloses shouldBe 1
      f.dbCloses shouldBe 0
    }

    property(s"$name closes its snapshot when iterator construction throws") {
      val f = new Fixture
      val failure = new IllegalStateException("iterator construction")
      f.iteratorFailure = Some(failure)

      val caught = intercept[IllegalStateException] { read(f) }
      (caught eq failure) shouldBe true
      f.iteratorCloses shouldBe 0
      f.snapshotCloses shouldBe 1
      f.dbCloses shouldBe 0
    }

    property(s"$name closes its snapshot when iterator close throws") {
      val f = new Fixture
      val failure = new IllegalStateException("iterator close")
      f.iteratorCloseFailure = Some(failure)

      val caught = intercept[IllegalStateException] { read(f) }
      (caught eq failure) shouldBe true
      f.iteratorCloses shouldBe 1
      f.snapshotCloses shouldBe 1
      f.dbCloses shouldBe 0
    }
  }
}
