package scorex.db

import java.lang.reflect.{InvocationHandler, InvocationTargetException, Method, Proxy}
import java.nio.file.Files

import org.iq80.leveldb.{DB, DBException, ReadOptions, Snapshot}
import org.scalatest.matchers.should.Matchers
import org.scalatest.propspec.AnyPropSpec

import scala.util.control.ControlThrowable
import scala.util.{Failure, Try}

class LDBVersionedStoreSnapshotSpec extends AnyPropSpec with Matchers {

  private class SnapshotCalls {
    var acquisitionFailure: Option[Throwable] = None
    var closeFailure: Option[Throwable] = None
    var acquisitions = 0
    var closes = 0
  }

  private def withStore(test: (LDBVersionedStore, SnapshotCalls) => Unit): Unit = {
    val dir = Files.createTempDirectory("snapshot-lifecycle").toFile
    val store = new LDBVersionedStore(dir, 2)
    val original = store.db
    val calls = new SnapshotCalls
    var acquired: Snapshot = null
    var exposedSnapshot: Snapshot = null
    val proxy = Proxy.newProxyInstance(classOf[DB].getClassLoader, Array(classOf[DB]), new InvocationHandler {
      override def invoke(proxy: Any, method: Method, args: Array[AnyRef]): AnyRef = {
        if (method.getName == "getSnapshot") {
          store.lock.isWriteLockedByCurrentThread shouldBe true
          calls.acquisitions += 1
          calls.acquisitionFailure.foreach(throw _)
          acquired = original.getSnapshot
          exposedSnapshot = new Snapshot {
            override def close(): Unit = {
              calls.closes += 1
              acquired.close()
              calls.closeFailure.foreach(throw _)
            }
          }
          exposedSnapshot
        } else if (method.getName == "get" && args.length == 2) {
          val options = args(1).asInstanceOf[ReadOptions]
          options.snapshot() should be theSameInstanceAs exposedSnapshot
          original.get(args(0).asInstanceOf[Array[Byte]], new ReadOptions().snapshot(acquired))
        } else {
          try {
            method.invoke(original, Option(args).getOrElse(Array.empty[AnyRef]): _*)
          } catch {
            case e: InvocationTargetException => throw e.getCause
          }
        }
      }
    }).asInstanceOf[DB]
    // Replace only the dependency after the real store has finished initialization.
    val dbField = classOf[LDBVersionedStore].getDeclaredField("db")
    dbField.setAccessible(true)
    dbField.set(store, proxy)
    try {
      test(store, calls)
    } finally {
      // Permit cleanup even when checking an implementation that retains the lock.
      while (store.lock.isWriteLockedByCurrentThread) store.lock.writeLock().unlock()
      store.close()
      LDBFactory.factory.destroy(new java.io.File(dir, "ldb_main"), new org.iq80.leveldb.Options())
      LDBFactory.factory.destroy(new java.io.File(dir, "ldb_undo"), new org.iq80.leveldb.Options())
      dir.delete()
    }
  }

  property("snapshot acquisition failure releases the write lock") {
    withStore { (store, calls) =>
      calls.acquisitionFailure = Some(new DBException("snapshot acquisition failed"))
      Try(store.processSnapshot(_ => ()))
      store.lock.isWriteLocked shouldBe false
    }
  }

  property("snapshot acquisition failure is returned without processing or closing an absent snapshot") {
    withStore { (store, calls) =>
      val failure = new DBException("snapshot acquisition failed")
      calls.acquisitionFailure = Some(failure)
      var processed = false
      Try(store.processSnapshot(_ => processed = true)).flatten shouldBe Failure(failure)
      processed shouldBe false
      calls.acquisitions shouldBe 1
      calls.closes shouldBe 0
    }
  }

  property("snapshot processing failure closes the snapshot once") {
    withStore { (store, calls) =>
      val failure = new IllegalStateException("processing failed")
      store.processSnapshot(_ => throw failure) shouldBe Failure(failure)
      calls.closes shouldBe 1
      store.lock.isWriteLocked shouldBe false
    }
  }

  property("snapshot close failure is returned as a Failure") {
    withStore { (store, calls) =>
      val failure = new java.io.IOException("snapshot close failed")
      calls.closeFailure = Some(failure)
      val result = store.processSnapshot(_ => 42)
      result shouldBe Failure(failure)
      calls.closes shouldBe 1
      store.lock.isWriteLocked shouldBe false
    }
  }

  property("snapshot processing failure remains primary when closing also fails") {
    withStore { (store, calls) =>
      val primary = new IllegalStateException("processing failed")
      val secondary = new java.io.IOException("snapshot close failed")
      calls.closeFailure = Some(secondary)
      Try(store.processSnapshot(_ => throw primary)).flatten shouldBe Failure(primary)
      primary.getSuppressed.toSeq shouldBe Seq(secondary)
      calls.closes shouldBe 1
    }
  }

  property("the same processing and close failure does not cause self-suppression") {
    withStore { (store, calls) =>
      val failure = new IllegalStateException("shared failure")
      calls.closeFailure = Some(failure)
      val result = store.processSnapshot(_ => throw failure)
      result shouldBe Failure(failure)
      failure.getSuppressed shouldBe empty
      calls.closes shouldBe 1
    }
  }

  private val propagatingFailures: Seq[(String, () => Throwable)] = Seq(
    "control throwable" -> (() => new ControlThrowable {}),
    "interrupted exception" -> (() => new InterruptedException("interruption sentinel"))
  )

  for ((name, newFailure) <- propagatingFailures; processingFails <- Seq(false, true)) {
    property(s"close $name propagates when ordinary processing failure is $processingFails") {
      withStore { (store, calls) =>
        val sentinel = newFailure()
        val processingFailure = new IllegalStateException("processing failed")
        calls.closeFailure = Some(sentinel)
        val thrown = intercept[Throwable] {
          store.processSnapshot { _ =>
            if (processingFails) throw processingFailure
            42
          }
        }
        thrown should be theSameInstanceAs sentinel
        processingFailure.getSuppressed shouldBe empty
        calls.closes shouldBe 1
        store.lock.isWriteLocked shouldBe false
      }
    }
  }

  for ((name, newFailure) <- propagatingFailures; closingFails <- Seq(false, true)) {
    property(s"processing $name propagates when ordinary close failure is $closingFails") {
      withStore { (store, calls) =>
        val sentinel = newFailure()
        val closingFailure = new java.io.IOException("snapshot close failed")
        if (closingFails) calls.closeFailure = Some(closingFailure)
        val thrown = intercept[Throwable] {
          store.processSnapshot(_ => throw sentinel)
        }
        thrown should be theSameInstanceAs sentinel
        sentinel.getSuppressed.toSeq shouldBe (if (closingFails) Seq(closingFailure) else Seq.empty)
        calls.closes shouldBe 1
        store.lock.isWriteLocked shouldBe false
      }
    }
  }

  property("snapshot reads stay stable while processing writes with the lock released") {
    withStore { (store, calls) =>
      val key = Array[Byte](1)
      val before = Array[Byte](2)
      val after = Array[Byte](3)
      store.update(Array[Byte](4), Seq.empty, Seq(key -> before)).get
      val result = store.processSnapshot { reader =>
        store.lock.isWriteLocked shouldBe false
        reader.get(key).toSeq shouldBe before.toSeq
        store.update(Array[Byte](5), Seq.empty, Seq(key -> after)).get
        reader.get(key).toSeq
      }
      result.get shouldBe before.toSeq
      store.get(key).get.toSeq shouldBe after.toSeq
      calls.acquisitions shouldBe 1
      calls.closes shouldBe 1
      store.lock.isWriteLocked shouldBe false
    }
  }
}
