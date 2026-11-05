package scorex.db

import java.io.{EOFException, File}
import java.lang.reflect.{InvocationHandler, InvocationTargetException, Method, Proxy}
import java.nio.ByteBuffer
import java.nio.file.Files
import java.util.concurrent.{Callable, CountDownLatch, Executors, TimeUnit}
import java.util.concurrent.atomic.{AtomicInteger, AtomicReference}

import org.iq80.leveldb.{DB, DBException, WriteBatch, WriteOptions}
import org.scalatest.matchers.should.Matchers
import org.scalatest.propspec.AnyPropSpec

import scala.util.Try

/** Exercises the journal-enabled store using ordinary data and classified local write outcomes. */
class LDBVersionedStoreRecoverySpec extends AnyPropSpec with Matchers {
  private def bytes(n: Int): Array[Byte] = Array(n.toByte)
  private def versions(store: LDBVersionedStore): Seq[Seq[Byte]] =
    store.rollbackVersions().toSeq.map(_.toSeq)

  private final class Writes {
    val error = new DBException("local write result unavailable")
    val closeError = new java.io.IOException("local batch cleanup failed")
    private var selected: Option[(String, Int, Boolean)] = None
    private var closeSelected: Option[String] = None
    private var openSelected: Option[String] = None
    private var databaseCloseSelected: Option[String] = None
    private var batchFailureSelected: Option[(String, String)] = None
    @volatile private var writeGate: Option[(String, CountDownLatch, CountDownLatch)] = None
    private val wrappedBatches = new java.util.IdentityHashMap[AnyRef, AnyRef]()
    private var matchingWrites = 0
    var syncWrites = Vector.empty[Boolean]
    var openedDatabases = Vector.empty[String]
    var closedDatabases = Vector.empty[String]
    var closedBatches = Vector.empty[String]
    val snapshots = new AtomicInteger()

    def arm(name: String, occurrence: Int, afterWrite: Boolean): Unit = {
      selected = Some((name, occurrence, afterWrite))
      matchingWrites = 0
    }

    def armBatchClose(name: String): Unit = closeSelected = Some(name)
    def armOpen(name: String): Unit = openSelected = Some(name)
    def armDatabaseClose(name: String): Unit = databaseCloseSelected = Some(name)
    def armBatchFailure(name: String, method: String): Unit = batchFailureSelected = Some(name -> method)
    def holdNextWrite(name: String): (CountDownLatch, CountDownLatch) = {
      val entered = new CountDownLatch(1)
      val release = new CountDownLatch(1)
      writeGate = Some((name, entered, release))
      entered -> release
    }

    def open(dir: File, name: String): DB = {
      if (openSelected.contains(name)) {
        openSelected = None
        throw error
      }
      val underlying = LDBVersionedStore.openDatabase(dir, name)
      openedDatabases :+= name
      Proxy.newProxyInstance(classOf[DB].getClassLoader, Array(classOf[DB]), new InvocationHandler {
        override def invoke(proxy: Any, method: Method, args: Array[AnyRef]): AnyRef = {
          val callArgs = Option(args).getOrElse(Array.empty[AnyRef])
          def delegate(): AnyRef = try {
            val forwarded = if (method.getName == "write" && callArgs.nonEmpty && wrappedBatches.containsKey(callArgs(0))) {
              callArgs.updated(0, wrappedBatches.get(callArgs(0)))
            } else callArgs
            method.invoke(underlying, forwarded: _*)
          } catch {
            case e: InvocationTargetException => throw e.getCause
          }
          if (method.getName == "close") {
            val result = delegate()
            closedDatabases :+= name
            if (databaseCloseSelected.contains(name)) {
              databaseCloseSelected = None
              throw closeError
            }
            result
          } else if (method.getName == "getSnapshot") {
            snapshots.incrementAndGet()
            delegate()
          } else if (method.getName == "createWriteBatch" && batchFailureSelected.contains(name -> "createWriteBatch")) {
            batchFailureSelected = None
            throw error
          } else if (method.getName == "createWriteBatch" &&
            (closeSelected.contains(name) || batchFailureSelected.exists(_._1 == name))) {
            val failClose = closeSelected.contains(name)
            if (failClose) closeSelected = None
            val actual = delegate()
            val wrapped = Proxy.newProxyInstance(classOf[WriteBatch].getClassLoader, Array(classOf[WriteBatch]),
              new InvocationHandler {
                override def invoke(proxy: Any, method: Method, args: Array[AnyRef]): AnyRef = {
                  if (batchFailureSelected.contains(name -> method.getName)) {
                    batchFailureSelected = None
                    throw error
                  }
                  val result = try method.invoke(actual, Option(args).getOrElse(Array.empty[AnyRef]): _*) catch {
                    case e: InvocationTargetException => throw e.getCause
                  }
                  if (method.getName == "close") {
                    closedBatches :+= name
                    if (failClose) throw closeError
                  }
                  result
                }
              })
            wrappedBatches.put(wrapped, actual)
            wrapped
          } else if (method.getName == "write" && callArgs.length == 2) {
            writeGate.filter(_._1 == name).foreach { case (_, entered, release) =>
              writeGate = None
              entered.countDown()
              if (!release.await(5, TimeUnit.SECONDS)) throw new IllegalStateException("Write gate timed out")
            }
            syncWrites :+= callArgs(1).asInstanceOf[WriteOptions].sync()
            val fault = selected.filter(_._1 == name).filter { _ =>
              matchingWrites += 1
              matchingWrites == selected.get._2
            }
            fault match {
              case Some((_, _, afterWrite)) =>
                selected = None
                if (afterWrite) delegate()
                throw error
              case None => delegate()
            }
          } else delegate()
        }
      }).asInstanceOf[DB]
    }
  }

  private def withDirectory(test: File => Unit): Unit = {
    val dir = Files.createTempDirectory("versioned-store-recovery").toFile
    try test(dir) finally {
      def remove(file: File): Unit = {
        Option(file.listFiles()).foreach(_.foreach(remove))
        if (!file.delete()) file.deleteOnExit()
      }
      remove(dir)
    }
  }

  private def seed(store: LDBVersionedStore): Unit = {
    store.update(bytes(1), Nil, Seq(bytes(10) -> bytes(11))).get
    store.update(bytes(2), Nil, Seq(bytes(10) -> bytes(12), bytes(20) -> bytes(21))).get
  }

  private def unavailable(store: LDBVersionedStore): Unit = {
    intercept[IllegalStateException](store.get(bytes(10)))
    intercept[IllegalStateException](store.lastVersionID)
    intercept[IllegalStateException](store.rollbackVersions())
    intercept[IllegalStateException](store.versionIdExists(bytes(2)))
    intercept[IllegalStateException](store.getAll)
    intercept[IllegalStateException](store.getRange(bytes(1), bytes(99)))
    intercept[IllegalStateException](store.processAll((_, _) => ()))
    store.processSnapshot(_.get(bytes(10))).isFailure shouldBe true
    store.update(bytes(3), Nil, Nil).isFailure shouldBe true
    store.rollbackTo(bytes(1)).isFailure shouldBe true
    intercept[IllegalStateException](store.clean(0))
    intercept[IllegalStateException](store.setKeepVersions(0))
  }

  property("ordinary updates, an empty version, rollback and reopening share one version identity") {
    withDirectory { dir =>
      var store = new LDBVersionedStore(dir, 2)
      try {
        seed(store)
        store.update(bytes(2), Iterator.empty, Iterator(bytes(10) -> bytes(13))).get
        store.update(bytes(3), Iterator.empty, Iterator.empty).get
        store.close()
        store = new LDBVersionedStore(dir, 2)
        versions(store) shouldBe Seq(3, 2, 1).map(n => bytes(n).toSeq)
        store.get(bytes(10)).get.toSeq shouldBe bytes(13).toSeq
        store.rollbackTo(bytes(1)).get
        store.close()
        store = new LDBVersionedStore(dir, 2)
        versions(store) shouldBe Seq(bytes(1).toSeq)
        store.get(bytes(10)).get.toSeq shouldBe bytes(11).toSeq
        store.get(bytes(20)) shouldBe None
        store.update(bytes(4), Seq(bytes(10)), Seq(bytes(30) -> bytes(31))).get
        store.rollbackTo(bytes(1)).get
        store.get(bytes(10)).get.toSeq shouldBe bytes(11).toSeq
        store.get(bytes(30)) shouldBe None
      } finally store.close()
    }
  }

  property("retention preserves its oldest rollback anchor across reopening and zero retention") {
    withDirectory { dir =>
      var store = new LDBVersionedStore(dir, 1)
      try {
        seed(store)
        store.update(bytes(3), Nil, Seq(bytes(10) -> bytes(13))).get
        versions(store) shouldBe Seq(bytes(3).toSeq, bytes(2).toSeq)
        store.close()
        store = new LDBVersionedStore(dir, 1)
        store.rollbackTo(bytes(2)).get
        store.get(bytes(10)).get.toSeq shouldBe bytes(12).toSeq
        store.setKeepVersions(0) shouldBe 1
        store.close()
        store = new LDBVersionedStore(dir, 0)
        store.rollbackTo(bytes(2)).get
        store.update(bytes(4), Nil, Seq(bytes(10) -> bytes(14))).get
        store.close()
        store = new LDBVersionedStore(dir, 0)
        versions(store) shouldBe Seq(bytes(4).toSeq)
        store.setKeepVersions(2) shouldBe 0
        store.update(bytes(5), Nil, Seq(bytes(10) -> bytes(15))).get
        store.close()
        store = new LDBVersionedStore(dir, 2)
        store.rollbackTo(bytes(4)).get
        store.get(bytes(10)).get.toSeq shouldBe bytes(14).toSeq
        store.clean(0)
        store.getKeepVersions shouldBe 2
      } finally store.close()
    }
  }

  for {
    (database, occurrence) <- Seq("ldb_journal" -> 1, "ldb_main" -> 1, "ldb_undo" -> 1, "ldb_journal" -> 2)
    afterWrite <- Seq(false, true)
  } property(s"update recovers after $database write $occurrence with applied=$afterWrite") {
    withDirectory { dir =>
      val writes = new Writes
      var store = new LDBVersionedStore(dir, 2, writes.open)
      try {
        seed(store)
        writes.arm(database, occurrence, afterWrite)
        store.update(bytes(3), Seq(bytes(20)), Seq(bytes(10) -> bytes(13))).failed.get shouldBe writes.error
        unavailable(store)
        store.close()
        store = new LDBVersionedStore(dir, 2, writes.open)
        val committed = database != "ldb_journal" || occurrence != 1 || afterWrite
        store.lastVersionID.get.toSeq shouldBe bytes(if (committed) 3 else 2).toSeq
        store.get(bytes(10)).get.toSeq shouldBe bytes(if (committed) 13 else 12).toSeq
        store.get(bytes(20)).map(_.toSeq) shouldBe (if (committed) None else Some(bytes(21).toSeq))
        store.rollbackTo(bytes(1)).get
        store.get(bytes(10)).get.toSeq shouldBe bytes(11).toSeq
        writes.syncWrites.nonEmpty shouldBe true
        writes.syncWrites.forall(identity) shouldBe true
      } finally store.close()
    }
  }

  for {
    operation <- Seq("rollback", "clean", "setKeepVersions")
    database <- Seq("ldb_main", "ldb_undo", "ldb_journal")
    afterWrite <- Seq(false, true)
  } property(s"$operation recovers after $database completion with applied=$afterWrite") {
    withDirectory { dir =>
      val writes = new Writes
      var store = new LDBVersionedStore(dir, 2, writes.open)
      try {
        seed(store)
        store.update(bytes(3), Nil, Seq(bytes(10) -> bytes(13))).get
        writes.arm(database, if (database == "ldb_journal") 2 else 1, afterWrite)
        operation match {
          case "rollback" => store.rollbackTo(bytes(1)).failed.get shouldBe writes.error
          case "clean" => intercept[DBException](store.clean(0)) shouldBe writes.error
          case _ => intercept[DBException](store.setKeepVersions(0)) shouldBe writes.error
        }
        unavailable(store)
        store.close()
        store = new LDBVersionedStore(dir, 2, writes.open)
        val target = if (operation == "rollback") 1 else 3
        versions(store) shouldBe Seq(bytes(target).toSeq)
        store.get(bytes(10)).get.toSeq shouldBe bytes(if (operation == "rollback") 11 else 13).toSeq
        store.getKeepVersions shouldBe 2
        store.update(bytes(4), Nil, Seq(bytes(10) -> bytes(14))).get
        store.rollbackTo(bytes(target)).get
        store.close()
        store = new LDBVersionedStore(dir, 2, writes.open)
        versions(store) shouldBe Seq(bytes(target).toSeq)
      } finally store.close()
    }
  }

  property("a preflight rejection leaves the store usable and returned versions cannot change metadata") {
    withDirectory { dir =>
      val store = new LDBVersionedStore(dir, 2)
      try {
        seed(store)
        store.rollbackTo(bytes(9)).isFailure shouldBe true
        val current = store.lastVersionID.get
        current(0) = 99.toByte
        val retained = store.rollbackVersions().toSeq
        retained.head(0) = 99.toByte
        versions(store) shouldBe Seq(bytes(2).toSeq, bytes(1).toSeq)
        store.update(bytes(3), Nil, Seq(bytes(10) -> bytes(13))).get
      } finally store.close()
    }
  }

  property("an interrupted recovery preserves its plan and releases database handles for another reopening") {
    withDirectory { dir =>
      val writes = new Writes
      val original = new LDBVersionedStore(dir, 2, writes.open)
      try {
        seed(original)
        writes.arm("ldb_main", 1, afterWrite = false)
        original.update(bytes(3), Nil, Seq(bytes(10) -> bytes(13))).isFailure shouldBe true
      } finally original.close()
      writes.arm("ldb_undo", 1, afterWrite = true)
      intercept[DBException](new LDBVersionedStore(dir, 2, writes.open)) shouldBe writes.error
      val recovered = new LDBVersionedStore(dir, 2, writes.open)
      try {
        recovered.lastVersionID.get.toSeq shouldBe bytes(3).toSeq
        recovered.get(bytes(10)).get.toSeq shouldBe bytes(13).toSeq
        recovered.rollbackTo(bytes(1)).get
        recovered.get(bytes(10)).get.toSeq shouldBe bytes(11).toSeq
      } finally recovered.close()
    }
  }

  for (writeFails <- Seq(false, true)) property(s"batch cleanup preserves the write result with failure=$writeFails") {
    withDirectory { dir =>
      val writes = new Writes
      var store = new LDBVersionedStore(dir, 2, writes.open)
      try {
        seed(store)
        writes.armBatchClose("ldb_main")
        if (writeFails) writes.arm("ldb_main", 1, afterWrite = false)
        val result = store.update(bytes(3), Nil, Seq(bytes(10) -> bytes(13)))
        if (writeFails) {
          result.failed.get shouldBe writes.error
          writes.error.getSuppressed.toSeq should contain(writes.closeError)
        } else result.get shouldBe (())
        store.close()
        store = new LDBVersionedStore(dir, 2, writes.open)
        store.lastVersionID.get.toSeq shouldBe bytes(3).toSeq
        store.get(bytes(10)).get.toSeq shouldBe bytes(13).toSeq
      } finally store.close()
    }
  }

  private def rawDatabase[T](dir: File, name: String)(test: DB => T): T = {
    val database = LDBVersionedStore.openDatabase(dir, name)
    try test(database) finally database.close()
  }

  private def pendingUpdate(dir: File): Writes = {
    val writes = new Writes
    val store = new LDBVersionedStore(dir, 2, writes.open)
    try {
      seed(store)
      writes.arm("ldb_main", 1, afterWrite = false)
      store.update(bytes(3), Nil, Seq(bytes(10) -> bytes(13))).failed.get shouldBe writes.error
    } finally store.close()
    writes
  }

  private def replaceInt(data: Array[Byte], offset: Int, value: Int): Array[Byte] =
    ByteBuffer.wrap(data.clone()).putInt(offset, value).array()

  private def replaceLong(data: Array[Byte], offset: Int, value: Long): Array[Byte] =
    ByteBuffer.wrap(data.clone()).putLong(offset, value).array()

  private def metadataEnd(data: Array[Byte]): Int = {
    val buffer = ByteBuffer.wrap(data)
    val count = buffer.getInt(20)
    var offset = 24
    for (_ <- 0 until count) offset += 4 + buffer.getInt(offset) + 8
    offset
  }

  private final case class RejectedField(name: String, key: Int, alter: Array[Byte] => Array[Byte],
                                         reason: String, errorClass: Class[_ <: Throwable] = classOf[IllegalArgumentException])

  private val journalInvalidFields: Seq[RejectedField] = Seq(
    RejectedField("unknown committed format", 0, data => replaceInt(data, 0, 2),
      "Unsupported versioned-store journal format"),
    RejectedField("negative transaction", 0, data => replaceLong(data, 4, -1L),
      "Negative versioned-store journal sequence"),
    RejectedField("negative LSN", 0, data => replaceLong(data, 12, -1L),
      "Negative versioned-store journal sequence"),
    RejectedField("negative version count", 0, data => replaceInt(data, 20, -1),
      "Incomplete versioned-store version list"),
    RejectedField("oversized version count", 0, data => replaceInt(data, 20, Int.MaxValue),
      "Incomplete versioned-store version list"),
    RejectedField("negative version length", 0, data => replaceInt(data, 24, -1),
      "Incomplete versioned-store journal field"),
    RejectedField("negative version start", 0, data => replaceLong(data, 29, -1L), "Invalid versioned-store LSN"),
    // Changing only the last start preserves ordering, isolating the upper LSN bound.
    RejectedField("last version start beyond assigned LSN", 0,
      data => replaceLong(data, metadataEnd(data) - 8, ByteBuffer.wrap(data).getLong(12) + 2L), "Invalid versioned-store LSN"),
    RejectedField("unordered version starts", 0, data => replaceLong(data, 29, 3L), "Unordered versioned-store LSNs"),
    RejectedField("truncated committed metadata", 0, data => data.dropRight(1), null, classOf[EOFException]),
    RejectedField("trailing committed bytes", 0, data => data :+ 0.toByte, "Trailing versioned-store journal bytes"),
    RejectedField("unknown pending format", 1, data => replaceInt(data, 0, 2), "Unsupported versioned-store journal format"),
    RejectedField("negative change count", 1, data => replaceInt(data, metadataEnd(data), -1),
      "Incomplete versioned-store change list"),
    RejectedField("oversized change count", 1, data => replaceInt(data, metadataEnd(data), Int.MaxValue),
      "Incomplete versioned-store change list"),
    RejectedField("negative change key length", 1, data => replaceInt(data, metadataEnd(data) + 4, -1),
      "Incomplete versioned-store journal field"),
    RejectedField("truncated pending metadata", 1, data => data.take(23), null, classOf[EOFException]),
    // Remove a key byte as well as the final presence flag, so the length check decides rejection.
    RejectedField("truncated pending changes", 1, data => data.dropRight(2), "Incomplete versioned-store journal field"),
    RejectedField("trailing pending bytes", 1, data => data :+ 0.toByte, "Trailing versioned-store journal bytes")
  )

  for (field <- journalInvalidFields) property(s"new journal rejects ${field.name} before participant writes") {
    withDirectory { dir =>
      val writes = pendingUpdate(dir)
      val changed = rawDatabase(dir, "ldb_journal") { journal =>
        val changed = field.alter(journal.get(bytes(field.key)))
        journal.put(bytes(field.key), changed, new WriteOptions().sync(true))
        changed
      }
      val writesBeforeOpen = writes.syncWrites.size
      val opening = Try(new LDBVersionedStore(dir, 2, writes.open))
      opening.foreach(_.close())
      val rejection = opening.failed.get
      rejection.getClass shouldBe field.errorClass
      val expectedMessage = if (field.errorClass == classOf[IllegalArgumentException]) s"requirement failed: ${field.reason}"
        else field.reason
      rejection.getMessage shouldBe expectedMessage
      writes.syncWrites.size shouldBe writesBeforeOpen
      writes.closedDatabases.takeRight(3) shouldBe Seq("ldb_journal", "ldb_undo", "ldb_main")
      rawDatabase(dir, "ldb_journal")(_.get(bytes(field.key)).toSeq) shouldBe changed.toSeq
      rawDatabase(dir, "ldb_main")(_.get(bytes(10)).toSeq) shouldBe bytes(12).toSeq
    }
  }

  for (missingPredecessor <- Seq(false, true)) property(s"new journal requires a consecutive predecessor with missing=$missingPredecessor") {
    withDirectory { dir =>
      val writes = pendingUpdate(dir)
      rawDatabase(dir, "ldb_journal") { journal =>
        if (missingPredecessor) journal.delete(bytes(0), new WriteOptions().sync(true))
        else {
          val pending = journal.get(bytes(1))
          val committedSequence = ByteBuffer.wrap(journal.get(bytes(0))).getLong(4)
          journal.put(bytes(1), replaceLong(pending, 4, committedSequence + 2L), new WriteOptions().sync(true))
        }
      }
      val writesBeforeOpen = writes.syncWrites.size
      val rejection = intercept[IllegalArgumentException](new LDBVersionedStore(dir, 2, writes.open))
      rejection.getMessage shouldBe (if (missingPredecessor) "requirement failed: Pending recovery has no committed predecessor"
        else "requirement failed: Pending recovery does not follow committed metadata")
      writes.syncWrites.size shouldBe writesBeforeOpen
      rawDatabase(dir, "ldb_journal")(_.get(bytes(1))) should not be null
      rawDatabase(dir, "ldb_main")(_.get(bytes(10)).toSeq) shouldBe bytes(12).toSeq
    }
  }

  for (database <- Seq("ldb_main", "ldb_undo", "ldb_journal")) {
    property(s"database acquisition failure at $database closes every earlier handle and preserves its error") {
      withDirectory { dir =>
        val writes = new Writes
        writes.armOpen(database)
        writes.armDatabaseClose("ldb_main")
        intercept[DBException](new LDBVersionedStore(dir, 2, writes.open)) shouldBe writes.error
        writes.closedDatabases shouldBe writes.openedDatabases.reverse
        if (database != "ldb_main") writes.error.getSuppressed.toSeq should contain(writes.closeError)
        val reopened = new LDBVersionedStore(dir, 2)
        try reopened.lastVersionID shouldBe None finally reopened.close()
      }
    }
  }

  for (afterWrite <- Seq(false, true)) property(s"journal initialization failure closes handles with applied=$afterWrite") {
    withDirectory { dir =>
      val writes = new Writes
      writes.arm("ldb_journal", 1, afterWrite)
      writes.armDatabaseClose("ldb_undo")
      intercept[DBException](new LDBVersionedStore(dir, 2, writes.open)) shouldBe writes.error
      writes.closedDatabases shouldBe Seq("ldb_journal", "ldb_undo", "ldb_main")
      writes.error.getSuppressed.toSeq should contain(writes.closeError)
      val reopened = new LDBVersionedStore(dir, 2)
      try reopened.lastVersionID shouldBe None finally reopened.close()
    }
  }

  for (method <- Seq("createWriteBatch", "put")) property(s"participant batch $method failure preserves a recoverable plan") {
    withDirectory { dir =>
      val writes = new Writes
      var store = new LDBVersionedStore(dir, 2, writes.open)
      try {
        seed(store)
        writes.armBatchFailure("ldb_main", method)
        store.update(bytes(3), Nil, Seq(bytes(10) -> bytes(13))).failed.get shouldBe writes.error
        if (method == "put") writes.closedBatches should contain("ldb_main")
        unavailable(store)
        store.close()
        store = new LDBVersionedStore(dir, 2, writes.open)
        store.lastVersionID.get.toSeq shouldBe bytes(3).toSeq
        store.get(bytes(10)).get.toSeq shouldBe bytes(13).toSeq
      } finally store.close()
    }
  }

  for {
    database <- Seq("ldb_main", "ldb_undo", "ldb_journal")
    afterWrite <- Seq(false, true)
    if database != "ldb_undo" || !afterWrite // The applied undo case is covered above.
  } property(s"interrupted replay at $database remains recoverable with applied=$afterWrite") {
    withDirectory { dir =>
      val writes = pendingUpdate(dir)
      writes.arm(database, 1, afterWrite)
      intercept[DBException](new LDBVersionedStore(dir, 2, writes.open)) shouldBe writes.error
      writes.closedDatabases.takeRight(3) shouldBe Seq("ldb_journal", "ldb_undo", "ldb_main")
      val recovered = new LDBVersionedStore(dir, 2)
      try {
        recovered.lastVersionID.get.toSeq shouldBe bytes(3).toSeq
        recovered.get(bytes(10)).get.toSeq shouldBe bytes(13).toSeq
        recovered.rollbackTo(bytes(1)).get
        recovered.get(bytes(10)).get.toSeq shouldBe bytes(11).toSeq
      } finally recovered.close()
    }
  }

  property("a new snapshot queued behind an ambiguous write is rejected before snapshot acquisition") {
    withDirectory { dir =>
      val writes = new Writes
      val store = new LDBVersionedStore(dir, 2, writes.open)
      val executor = Executors.newFixedThreadPool(2)
      seed(store)
      val (writeEntered, releaseWrite) = writes.holdNextWrite("ldb_main")
      try {
        writes.arm("ldb_main", 1, afterWrite = true)
        val writer = executor.submit(new Callable[Try[Unit]] {
          override def call(): Try[Unit] = store.update(bytes(3), Nil, Seq(bytes(10) -> bytes(13)))
        })
        writeEntered.await(5, TimeUnit.SECONDS) shouldBe true
        val readerThread = new AtomicReference[Thread]()
        val readerStarted = new CountDownLatch(1)
        val reader = executor.submit(new Callable[Try[Array[Byte]]] {
          override def call(): Try[Array[Byte]] = {
            readerThread.set(Thread.currentThread())
            readerStarted.countDown()
            store.processSnapshot(_.get(bytes(10)))
          }
        })
        readerStarted.await(5, TimeUnit.SECONDS) shouldBe true
        val deadline = System.nanoTime() + TimeUnit.SECONDS.toNanos(5)
        while (!store.lock.hasQueuedThread(readerThread.get()) && System.nanoTime() < deadline) Thread.`yield`()
        store.lock.hasQueuedThread(readerThread.get()) shouldBe true
        writes.snapshots.get() shouldBe 0
        releaseWrite.countDown()
        writer.get(5, TimeUnit.SECONDS).failed.get shouldBe writes.error
        reader.get(5, TimeUnit.SECONDS).failed.get shouldBe a[IllegalStateException]
        writes.snapshots.get() shouldBe 0
      } finally {
        releaseWrite.countDown()
        executor.shutdownNow()
        executor.awaitTermination(5, TimeUnit.SECONDS) shouldBe true
        store.close()
      }
    }
  }

  property("an acquired snapshot keeps its committed view while later snapshot requests are quarantined") {
    withDirectory { dir =>
      val writes = new Writes
      val store = new LDBVersionedStore(dir, 2, writes.open)
      val executor = Executors.newSingleThreadExecutor()
      val acquired = new CountDownLatch(1)
      val resumeSnapshot = new CountDownLatch(1)
      try {
        seed(store)
        val reader = executor.submit(new Callable[Try[Array[Byte]]] {
          override def call(): Try[Array[Byte]] = store.processSnapshot { snapshot =>
            acquired.countDown()
            if (!resumeSnapshot.await(5, TimeUnit.SECONDS)) throw new IllegalStateException("Snapshot gate timed out")
            snapshot.get(bytes(10))
          }
        })
        acquired.await(5, TimeUnit.SECONDS) shouldBe true
        writes.arm("ldb_main", 1, afterWrite = true)
        store.update(bytes(3), Nil, Seq(bytes(10) -> bytes(13))).failed.get shouldBe writes.error
        store.processSnapshot(_.get(bytes(10))).isFailure shouldBe true
        writes.snapshots.get() shouldBe 1
        resumeSnapshot.countDown()
        reader.get(5, TimeUnit.SECONDS).get.toSeq shouldBe bytes(12).toSeq
      } finally {
        resumeSnapshot.countDown()
        executor.shutdownNow()
        executor.awaitTermination(5, TimeUnit.SECONDS) shouldBe true
        store.close()
      }
    }
  }

  for (withHistory <- Seq(false, true)) property(s"healthy legacy database adoption preserves data with history=$withHistory") {
    withDirectory { dir =>
      val main = LDBVersionedStore.openDatabase(dir, "ldb_main")
      val undo = LDBVersionedStore.openDatabase(dir, "ldb_undo")
      try {
        main.put(bytes(10), bytes(12), new WriteOptions().sync(true))
        if (withHistory) {
          // Existing encoding: version length, key length, version, key, optional previous value.
          undo.put(ByteBuffer.allocate(8).putLong(~1L).array(), Array[Byte](1, 1, 1, 10), new WriteOptions().sync(true))
          undo.put(ByteBuffer.allocate(8).putLong(~2L).array(), Array[Byte](1, 1, 2, 10, 11), new WriteOptions().sync(true))
        } else main.put(scorex.crypto.hash.Blake2b256("last_version"), bytes(2), new WriteOptions().sync(true))
      } finally {
        undo.close()
        main.close()
      }
      var store = new LDBVersionedStore(dir, 2)
      try {
        store.lastVersionID.get.toSeq shouldBe bytes(2).toSeq
        store.get(bytes(10)).get.toSeq shouldBe bytes(12).toSeq
        store.update(bytes(3), Nil, Seq(bytes(10) -> bytes(13))).get
        store.close()
        store = new LDBVersionedStore(dir, 2)
        store.rollbackTo(bytes(if (withHistory) 1 else 2)).get
        store.get(bytes(10)).get.toSeq shouldBe bytes(if (withHistory) 11 else 12).toSeq
        if (withHistory) store.getAll.map(_._1.toSeq).toSeq shouldBe Seq(bytes(10).toSeq)
      } finally store.close()
    }
  }
}
