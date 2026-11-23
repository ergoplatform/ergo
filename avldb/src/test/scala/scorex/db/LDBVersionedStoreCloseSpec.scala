package scorex.db

import java.util.concurrent.{CountDownLatch, TimeUnit}
import java.util.concurrent.atomic.{AtomicBoolean, AtomicReference}

import org.scalatest.matchers.should.Matchers
import org.scalatest.propspec.AnyPropSpec
import scorex.crypto.authds.avltree.batch.benchmark.LDBVersionedStoreBenchmark.getRandomTempDir

import scala.util.{Success, Try}

class LDBVersionedStoreCloseSpec extends AnyPropSpec with Matchers {

  private val testKey = Array[Byte](9)

  private def storeWithKey(): LDBVersionedStore = {
    val store = new LDBVersionedStore(getRandomTempDir, 2)
    store.update(Array[Byte](1), Seq.empty, Seq(testKey -> Array[Byte](1, 2, 3))).get
    store
  }

  // Starts a thread that calls close() once the processing has read once, and returns its "close returned" latch.
  private def closeAfterFirstRead(store: LDBVersionedStore, inProcessing: CountDownLatch,
                                  closerError: AtomicReference[Throwable]): (Thread, CountDownLatch) = {
    val closeReturned = new CountDownLatch(1)
    val closer = new Thread(() => {
      inProcessing.await()
      try store.close() catch { case t: Throwable => closerError.set(t) }
      closeReturned.countDown()
    })
    closer.start()
    (closer, closeReturned)
  }

  // Runs processSnapshot on this thread and close() on another once the processing has read once.
  private def closeDuringProcessing(store: LDBVersionedStore, readAfterClose: Boolean) = {
    val inProcessing = new CountDownLatch(1)
    val closerError = new AtomicReference[Throwable]()
    val (closer, closeReturned) = closeAfterFirstRead(store, inProcessing, closerError)
    val returnedDuringProcessing = new AtomicBoolean(false)
    val secondRead = new AtomicReference[Try[Array[Byte]]]()
    val result = store.processSnapshot { reader =>
      reader.get(testKey)
      inProcessing.countDown()
      // wait for close() to return; with close waiting for the snapshot it cannot, and the wait times out
      returnedDuringProcessing.set(closeReturned.await(5, TimeUnit.SECONDS))
      if (readAfterClose) secondRead.set(Try(reader.get(testKey)))
      42
    }
    closer.join(10000)
    closerError.get() shouldBe null
    (result, returnedDuringProcessing.get(), secondRead.get(), closer.isAlive)
  }

  property("close waits until a snapshot taken by processSnapshot is released") {
    val (result, returnedDuringProcessing, _, closerAlive) = closeDuringProcessing(storeWithKey(), readAfterClose = false)
    returnedDuringProcessing shouldBe false
    result.isSuccess shouldBe true
    closerAlive shouldBe false
  }

  property("a snapshot read after close has started fails instead of reaching a closed database") {
    val (result, returnedDuringProcessing, secondRead, closerAlive) =
      closeDuringProcessing(storeWithKey(), readAfterClose = true)
    returnedDuringProcessing shouldBe false
    secondRead.isFailure shouldBe true
    result.isSuccess shouldBe true
    closerAlive shouldBe false
  }

  property("processSnapshot started while close waits is refused, and close still finishes") {
    val store = storeWithKey()
    val inProcessing = new CountDownLatch(1)
    val closerError = new AtomicReference[Throwable]()
    val (closer, closeReturned) = closeAfterFirstRead(store, inProcessing, closerError)
    val returnedDuringProcessing = new AtomicBoolean(false)
    val lateRan = new AtomicBoolean(false)
    val lateHeldLock = new AtomicBoolean(false)
    val late = new AtomicReference[Try[Int]]()
    val lateCaller = new Thread(() => {
      late.set(Try(store.processSnapshot { _ => lateRan.set(true); 7 }).flatten)
      lateHeldLock.set(store.lock.isWriteLockedByCurrentThread)
    })
    val result = store.processSnapshot { reader =>
      reader.get(testKey)
      inProcessing.countDown()
      // close has started once a snapshot read fails (or close has returned): checked every 5 ms, for at most 5 s
      val closeStarted =
        Iterator.continually(closeReturned.await(5, TimeUnit.MILLISECONDS) || Try(reader.get(testKey)).isFailure)
          .take(1000).exists(identity)
      returnedDuringProcessing.set(closeReturned.getCount == 0)
      if (closeStarted) lateCaller.start()
      lateCaller.join(5000)
      42
    }
    closer.join(10000)
    closerError.get() shouldBe null
    returnedDuringProcessing.get() shouldBe false
    late.get().isFailure shouldBe true
    lateRan.get() shouldBe false
    lateHeldLock.get() shouldBe false
    result.isSuccess shouldBe true
    closer.isAlive shouldBe false
  }

  property("processSnapshot on a closed store fails without running the processing") {
    val store = storeWithKey()
    store.close()
    val ran = new AtomicBoolean(false)
    val result = Try(store.processSnapshot { _ => ran.set(true); 42 }).flatten
    result.isFailure shouldBe true
    ran.get() shouldBe false
  }

  property("processSnapshot refused before taking a snapshot releases the write lock and returns a Failure") {
    val store = storeWithKey()
    store.close()
    val returned = Try(store.processSnapshot(_ => 42))
    store.lock.isWriteLockedByCurrentThread shouldBe false
    returned.map(_.isFailure) shouldBe Success(true)
  }

  property("processSnapshot without a concurrent close still reads its snapshot") {
    val store = storeWithKey()
    store.processSnapshot(reader => reader.get(testKey).toSeq).get shouldBe Seq[Byte](1, 2, 3)
    store.close()
  }
}
