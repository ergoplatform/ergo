package org.ergoplatform.db

import java.io.File
import java.lang.reflect.{InvocationHandler, Method, Proxy}
import java.util.concurrent.{ConcurrentHashMap, CountDownLatch, TimeUnit}
import java.util.concurrent.atomic.{AtomicInteger, AtomicReference}
import org.iq80.leveldb.{DB, DBFactory, Options}
import org.ergoplatform.utils.ErgoCorePropertyTest
import scorex.db.StoreRegistry

class StoreRegistryLifecycleSpec extends ErgoCorePropertyTest {
  import StoreRegistryLifecycleFixtures._

  private val timeoutSeconds = 10L

  private def await(latch: CountDownLatch): Unit =
    assert(latch.await(timeoutSeconds, TimeUnit.SECONDS), "lifecycle latch timed out")

  private class Worker(name: String, body: () => Unit) {
    val done = new CountDownLatch(1)
    private val failure = new AtomicReference[Throwable]()
    val thread = new Thread(new Runnable {
      override def run(): Unit = {
        try body()
        catch { case error: Throwable => failure.set(error) }
        finally done.countDown()
      }
    }, name)
    thread.setDaemon(true)
    thread.start()

    def finish(): Unit = {
      thread.join(TimeUnit.SECONDS.toMillis(timeoutSeconds))
      if (thread.isAlive) {
        thread.interrupt()
        thread.join(TimeUnit.SECONDS.toMillis(1))
      }
      assert(!thread.isAlive, s"$name did not stop")
      Option(failure.get()).foreach(throw _)
    }
  }

  private def awaitQueued(registry: StoreRegistry, worker: Worker): Unit = {
    val deadline = System.nanoTime() + TimeUnit.SECONDS.toNanos(timeoutSeconds)
    while (!registry.lock.hasQueuedThread(worker.thread) && System.nanoTime() < deadline) {
      assert(worker.done.getCount != 0, "worker completed before queuing on the registry lock")
      Thread.`yield`()
    }
    assert(registry.lock.hasQueuedThread(worker.thread), "worker did not queue on the registry lock")
    assert(worker.done.getCount != 0, "queued worker completed while native close was blocked")
  }

  property("registry lifecycle reuses an open handle and reopens after normal close") {
    val factory = new CountingFactory()
    val registry = StoreRegistry(factory)
    val path = new File("registry-lifecycle-normal")
    val first = registry.open(path, new Options())
    registry.open(path, new Options()) should be theSameInstanceAs first
    assert(factory.opens.get() == 1)
    first.close()
    assert(factory.closeCount(1) == 1)
    assert(!registry.map.contains(path))
    val second = registry.open(path, new Options())
    try {
      second should not be theSameInstanceAs(first)
      assert(factory.opens.get() == 2)
      registry.map(path) should be theSameInstanceAs second
    } finally second.close()
  }

  property("registry lifecycle stale close preserves the replacement handle") {
    val factory = new CountingFactory()
    val registry = StoreRegistry(factory)
    val path = new File("registry-lifecycle-stale")
    val first = registry.open(path, new Options())
    first.close()
    val second = registry.open(path, new Options())
    try {
      first.close()
      assert(factory.closeCount(1) == 1)
      registry.map(path) should be theSameInstanceAs second
      registry.open(path, new Options()) should be theSameInstanceAs second
      assert(factory.opens.get() == 2)
    } finally second.close()
  }

  property("registry lifecycle open waits for native close before creating a replacement") {
    val closeEntered = new CountDownLatch(1)
    val releaseClose = new CountDownLatch(1)
    val factory = new CountingFactory(id => if (id == 1) {
      closeEntered.countDown()
      await(releaseClose)
    })
    val registry = StoreRegistry(factory)
    val path = new File("registry-lifecycle-ordering")
    val first = registry.open(path, new Options())
    val replacement = new AtomicReference[DB]()
    val closer = new Worker("registry-lifecycle-closer", () => first.close())
    var opener: Option[Worker] = None
    try {
      await(closeEntered)
      opener = Some(new Worker("registry-lifecycle-opener", () => {
        replacement.set(registry.open(path, new Options()))
      }))
      awaitQueued(registry, opener.get)
      assert(factory.opens.get() == 1)
      assert(factory.closeCount(1) == 1)
    } finally {
      releaseClose.countDown()
      try closer.finish()
      finally {
        try opener.foreach(_.finish())
        finally Option(replacement.get()).foreach(_.close())
      }
    }
    assert(factory.opens.get() == 2)
    replacement.get() should not be theSameInstanceAs(first)
  }

  property("registry lifecycle overlapping close callers forward native close once") {
    val closeEntered = new CountDownLatch(1)
    val releaseClose = new CountDownLatch(1)
    val closeEntries = new AtomicInteger()
    val factory = new CountingFactory(_ => {
      if (closeEntries.incrementAndGet() == 1) {
        closeEntered.countDown()
        await(releaseClose)
      }
    })
    val registry = StoreRegistry(factory)
    val path = new File("registry-lifecycle-overlap")
    val db = registry.open(path, new Options())
    val first = new Worker("registry-lifecycle-first-close", () => db.close())
    var second: Option[Worker] = None
    try {
      await(closeEntered)
      second = Some(new Worker("registry-lifecycle-second-close", () => db.close()))
      awaitQueued(registry, second.get)
      assert(factory.closeCount(1) == 1)
    } finally {
      releaseClose.countDown()
      try first.finish()
      finally second.foreach(_.finish())
    }
    assert(factory.closeCount(1) == 1)
    assert(!registry.map.contains(path))
  }

  property("registry lifecycle failed close unregisters once and rethrows the original failure") {
    val expected = new IllegalStateException("fake close failure")
    val factory = new CountingFactory(id => if (id == 1) throw expected)
    val registry = StoreRegistry(factory)
    val path = new File("registry-lifecycle-failed-close")
    val db = registry.open(path, new Options())
    intercept[IllegalStateException](db.close()) should be theSameInstanceAs expected
    assert(!registry.map.contains(path))
    db.close()
    assert(factory.closeCount(1) == 1)
    assert(factory.opens.get() == 1)
    val replacement = registry.open(path, new Options())
    try {
      replacement should not be theSameInstanceAs(db)
      assert(factory.opens.get() == 2)
      db.close()
      registry.map(path) should be theSameInstanceAs replacement
      assert(factory.closeCount(1) == 1)
    } finally replacement.close()
  }
}

private[db] object StoreRegistryLifecycleFixtures {
  final class CountingFactory(onClose: Int => Unit = _ => ()) extends DBFactory {
    val opens = new AtomicInteger()
    private val closes = new ConcurrentHashMap[Integer, AtomicInteger]()

    def closeCount(id: Int): Int = closes.get(id).get()

    override def open(path: File, options: Options): DB = {
      val id = opens.incrementAndGet()
      val count = new AtomicInteger()
      closes.put(id, count)
      Proxy.newProxyInstance(classOf[DB].getClassLoader, Array[Class[_]](classOf[DB]), new InvocationHandler {
        override def invoke(proxy: AnyRef, method: Method, arguments: Array[AnyRef]): AnyRef = method.getName match {
          case "close" =>
            count.incrementAndGet()
            onClose(id)
            null
          case "toString" => s"CountingDB($id)"
          case "hashCode" => Integer.valueOf(System.identityHashCode(proxy))
          case "equals" => java.lang.Boolean.valueOf(proxy eq arguments(0))
          case name => throw new UnsupportedOperationException(s"Unexpected fake DB operation: $name")
        }
      }).asInstanceOf[DB]
    }

    override def destroy(path: File, options: Options): Unit =
      throw new UnsupportedOperationException("destroy")

    override def repair(path: File, options: Options): Unit =
      throw new UnsupportedOperationException("repair")
  }
}
