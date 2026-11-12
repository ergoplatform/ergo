package scorex.crypto.authds.avltree.batch

import java.io.File
import java.security.MessageDigest

import com.google.common.primitives.Ints
import org.iq80.leveldb.Options
import org.scalatest.matchers.should.Matchers
import org.scalatest.propspec.AnyPropSpec
import scorex.crypto.authds.avltree.batch.helpers.TestHelper
import scorex.crypto.authds.{ADKey, ADValue}
import scorex.crypto.hash.{Blake2b256, Digest32}
import scorex.db.{LDBFactory, LDBVersionedStore}
import scorex.util.ByteArrayBuilder
import scorex.util.encode.Base16
import scorex.util.serialization.VLQByteBufferWriter

import scala.annotation.tailrec
import scala.collection.mutable
import scala.util.Try

/**
  * The bytes VersionedLDBAVLStorage hands to the store are what a node persists for its state tree, so a change to
  * ProverNodeSerializer.toBytes, serializedVisitedNodes or update must leave them identical, order included (the
  * order of the pairs fixes the undo records' sequence numbers).
  *
  * A fixed sequence of batches (empty tree, a no-op batch, a single insert, leaf values of 0 bytes up to over 2^24
  * bytes, a deep tree, mixed batches, a batch removing thousands of nodes, a rollback, a reopen from disk and
  * removal back to the empty tree) is applied through PersistentBatchAVLProver to a VersionedLDBAVLStorage over a
  * store that records every update call. The SHA-256 of the recorded arguments, and of both LevelDB databases after
  * close, are compared with the values this sequence gives on v6.0.11 (808189de5).
  */
class StoredNodeBytesSpecification extends AnyPropSpec with Matchers with TestHelper {

  override protected val KL = 32
  override protected val VL = 8 // unused: the prover below takes values of any length

  // Recorded on v6.0.11 (808189de5), Scala 2.12, by this file.
  private val ExpectedUpdateCalls = 20
  private val ExpectedArgumentsSha256 = "f0654315ff7c430692d31f1f8ed3db52d4073231d57e0b46bcacb0fb0f44793b"
  private val ExpectedMainDbSha256 = "06175fca1685d0f0cdc02dd9bc1cac26617846612753b42176c3122c243aa702"
  private val ExpectedUndoDbSha256 = "d628a960e8f9cf4253bbcfc8f538a856a1eb48c308729a72ba20848d6585577c"

  private case class CallSize(removed: Int, written: Int)

  private case class DbDigest(sha256: String, entries: Long)

  private class RecordingStore(dir: File, keep: Int) extends LDBVersionedStore(dir, initialKeepVersions = keep) {
    val digest: MessageDigest = MessageDigest.getInstance("SHA-256")
    private val sizes = mutable.ArrayBuffer.empty[CallSize]

    def calls: Int = sizes.size
    def removed: Long = sizes.map(_.removed.toLong).sum
    def written: Long = sizes.map(_.written.toLong).sum
    def maxRemovedInOneCall: Int = sizes.map(_.removed).foldLeft(0)((a, b) => math.max(a, b))

    private def feed(bytes: Array[Byte]): Unit = {
      digest.update(Ints.toByteArray(bytes.length))
      digest.update(bytes)
    }

    override def update(versionID: VersionID,
                        toRemove: TraversableOnce[Array[Byte]],
                        toUpdate: TraversableOnce[(Array[Byte], Array[Byte])]): Try[Unit] = {
      val rem = mutable.ArrayBuffer.empty[Array[Byte]]
      toRemove.foreach(k => rem += k)
      val upd = mutable.ArrayBuffer.empty[(Array[Byte], Array[Byte])]
      toUpdate.foreach(kv => upd += kv)
      sizes += CallSize(rem.length, upd.length)
      feed(versionID)
      digest.update(Ints.toByteArray(rem.length))
      rem.foreach(feed)
      digest.update(Ints.toByteArray(upd.length))
      upd.foreach { case (k, v) => feed(k); feed(v) }
      super.update(versionID, rem, upd)
    }
  }

  private def key(i: Int): ADKey = ADKey @@ Blake2b256("k" + i).take(KL)

  // Blake2b256("v" + i), then the hash of each previous block, concatenated and cut to `length` bytes
  private def value(i: Int, length: Int): ADValue = {
    val out = new Array[Byte](length)
    val blocks = Iterator.iterate(Blake2b256("v" + i): Array[Byte])(block => Blake2b256(block))
    (0 until length by Blake2b256.DigestSize).iterator.zip(blocks).foreach { case (pos, block) =>
      System.arraycopy(block, 0, out, pos, math.min(block.length, length - pos))
    }
    ADValue @@ out
  }

  // shaped like the node's per-block metadata: a handful of pairs, passed as a List, a Vector or empty
  private def meta(batch: Int): Seq[(Array[Byte], Array[Byte])] = batch % 3 match {
    case 0 => List.tabulate(4)(j => (Blake2b256(s"m$batch/$j"): Array[Byte], value(batch * 10 + j, 8 + j * 25): Array[Byte]))
    case 1 => Vector.tabulate(2)(j => (Blake2b256(s"m$batch/$j"): Array[Byte], value(batch * 10 + j, 33): Array[Byte]))
    case _ => Seq.empty
  }

  private def newProver(): BatchAVLProver[D, HF] = new BatchAVLProver[D, HF](KL, None)

  private def openProver(store: LDBVersionedStore): PersistentBatchAVLProver[D, HF] =
    PersistentBatchAVLProver.create[D, HF](newProver(), new VersionedLDBAVLStorage(store), paranoidChecks = true).get

  // One stretch of the batch sequence on one prover; `batches` numbers the commits across stretches.
  private class Batches(val prover: PersistentBatchAVLProver[D, HF], batches: Iterator[Int]) {
    def commit(): Unit = prover.generateProofAndUpdateStorage(meta(batches.next()))
    def ins(i: Int, len: Int): Unit = prover.performOneOperation(Insert(key(i), value(i, len))).get
    def rem(i: Int): Unit = prover.performOneOperation(Remove(key(i))).get
    def upd(i: Int, len: Int): Unit = prover.performOneOperation(Update(key(i), value(i + 100000, len))).get
  }

  private def dbSha256(dir: File, name: String): DbDigest = {
    val db = LDBFactory.factory.open(new File(dir, name), new Options())
    try {
      val md = MessageDigest.getInstance("SHA-256")
      val it = db.iterator()
      try {
        it.seekToFirst()
        val entries = Iterator.continually(it).takeWhile(_.hasNext).map(_.next())
        val n = entries.foldLeft(0L) { (count, e) =>
          md.update(Ints.toByteArray(e.getKey.length)); md.update(e.getKey)
          md.update(Ints.toByteArray(e.getValue.length)); md.update(e.getValue)
          count + 1
        }
        DbDigest(Base16.encode(md.digest()), n)
      } finally it.close()
    } finally db.close()
  }

  private def writerBytes(node: ProverNodes[Digest32]): Array[Byte] = {
    val w = new VLQByteBufferWriter(new ByteArrayBuilder())
    VersionedLDBAVLStorage.noStoreSerializer.serialize(node, w)
    w.result().toBytes
  }

  // Every node reachable from `top`: toBytes equals the writer serialization and survives a parse round trip
  // (the parsed internal node is a ProxyInternalNode, so that case is covered too). Returns the classes seen.
  private def checkAllNodes(top: ProverNodes[Digest32]): Map[String, Int] = {
    val seen = mutable.Map.empty[String, Int].withDefaultValue(0)
    val ser = VersionedLDBAVLStorage.noStoreSerializer

    def check(node: ProverNodes[Digest32]): Unit = {
      seen(node.getClass.getSimpleName) += 1
      val expected = writerBytes(node)
      val actual = ser.toBytes(node)
      withClue(s"${node.getClass.getSimpleName} ${Base16.encode(node.label)}: ") {
        Base16.encode(actual) shouldBe Base16.encode(expected)
        val parsed = ser.parseBytes(actual)
        seen("parsed:" + parsed.getClass.getSimpleName) += 1
        Base16.encode(ser.toBytes(parsed)) shouldBe Base16.encode(expected)
        Base16.encode(writerBytes(parsed)) shouldBe Base16.encode(expected)
      }
    }

    @tailrec
    def visit(stack: List[ProverNodes[Digest32]]): Unit = stack match {
      case Nil =>
      case node :: rest =>
        check(node)
        node match {
          case n: InternalProverNode[Digest32] => visit(n.left :: n.right :: rest)
          case _ => visit(rest)
        }
    }

    visit(List(top))
    seen.toMap
  }

  private def byName(counts: collection.Map[String, Int]): String =
    counts.toSeq.sortBy { case (name, _) => name }.mkString(",")

  property("stored node bytes and their order equal v6.0.11's for a fixed batch sequence") {
    println(s"[bytes-eq] factory=${LDBFactory.factory}")
    val dir = getRandomTempDir
    val batchNumbers = Iterator.from(0)
    val classes = mutable.Map.empty[String, Int].withDefaultValue(0)

    val store1 = new RecordingStore(dir, 10)
    val run1 = new Batches(openProver(store1), batchNumbers)
    // P1: no-op batch on the empty tree (the top node is re-written though not new)
    run1.commit()
    // P2: a single insert
    run1.ins(0, 8); run1.commit()
    // P3: leaf value lengths across every byte of the 4-byte length prefix
    val lengths = Seq(0, 1, 127, 128, 255, 256, 65535, 65536, 70000, (1 << 24) + 3)
    lengths.zipWithIndex.foreach { case (len, j) => run1.ins(1 + j, len) }
    run1.commit()
    // P4: a deep tree, 4000 inserts in batches of 500
    (100 until 4100).grouped(500).foreach { g => g.foreach(i => run1.ins(i, 8 + i % 41)); run1.commit() }
    val afterP4 = run1.prover.digest
    // P5: a mixed batch
    (100 until 400).foreach(i => run1.upd(i, 8 + i % 7))
    (5000 until 5300).foreach(i => run1.ins(i, 16))
    (400 until 700).foreach(run1.rem)
    run1.commit()
    // P6: one batch removing thousands of nodes
    (700 until 2700).foreach(run1.rem)
    run1.commit()
    checkAllNodes(run1.prover.avlProver.topNode).foreach { case (c, n) => classes(c) += n }
    // P7: rollback to the end of P4, then a different batch
    run1.prover.rollback(afterP4).get
    (6000 until 6300).foreach(i => run1.ins(i, 12))
    run1.commit()
    store1.close()

    // P8: reopen from disk (lazily loaded top node), a no-op batch, then a batch
    val store2 = new RecordingStore(dir, 10)
    val run2 = new Batches(openProver(store2), batchNumbers)
    run2.commit()
    (7000 until 7100).foreach(i => run2.ins(i, 20))
    run2.commit()
    // P9: remove every key back to the empty tree
    val live = (0 to lengths.size) ++ (100 until 4100) ++ (6000 until 6300) ++ (7000 until 7100)
    live.grouped(1500).foreach { g => g.foreach(run2.rem); run2.commit() }
    checkAllNodes(run2.prover.avlProver.topNode).foreach { case (c, n) => classes(c) += n }
    store2.close()

    // one stream over both store instances: the first instance's digest continues into the second's
    val total = MessageDigest.getInstance("SHA-256")
    total.update(store1.digest.digest())
    total.update(store2.digest.digest())
    val argumentsSha = Base16.encode(total.digest())
    val calls = store1.calls + store2.calls
    val removed = store1.removed + store2.removed
    val written = store1.written + store2.written
    val maxRemoved = math.max(store1.maxRemovedInOneCall, store2.maxRemovedInOneCall)
    val mainDb = dbSha256(dir, "ldb_main")
    val undoDb = dbSha256(dir, "ldb_undo")
    println(s"[bytes-eq] calls=$calls removed=$removed written=$written maxRemovedInOneCall=$maxRemoved " +
      s"mainEntries=${mainDb.entries} undoEntries=${undoDb.entries} finalDigest=${Base16.encode(run2.prover.digest)}")
    println(s"[bytes-eq] argumentsSha256=$argumentsSha")
    println(s"[bytes-eq] mainDbSha256=${mainDb.sha256}")
    println(s"[bytes-eq] undoDbSha256=${undoDb.sha256}")
    println(s"[bytes-eq] nodeClassesChecked=${byName(classes)}")

    // the sequence reaches the shapes it claims to
    maxRemoved should be > 2000
    lengths.max should be > (1 << 24)

    calls shouldBe ExpectedUpdateCalls
    argumentsSha shouldBe ExpectedArgumentsSha256
    mainDb.sha256 shouldBe ExpectedMainDbSha256
    undoDb.sha256 shouldBe ExpectedUndoDbSha256
  }

  property("toBytes equals the writer serialization for every node of a deep tree and of a reopened tree") {
    val dir = getRandomTempDir
    val store = new LDBVersionedStore(dir, initialKeepVersions = 10)
    val prover = openProver(store)
    (0 until 3000).foreach(i => prover.performOneOperation(Insert(key(i), value(i, i % 300))).get)
    // before persisting: every node is new
    val fresh = checkAllNodes(prover.avlProver.topNode)
    prover.generateProofAndUpdateStorage()
    store.close()
    val store2 = new LDBVersionedStore(dir, initialKeepVersions = 10)
    val reopened = openProver(store2)
    val loaded = checkAllNodes(reopened.avlProver.topNode)
    store2.close()
    println(s"[bytes-eq] fresh=${byName(fresh)} loaded=${byName(loaded)}")
    loaded.getOrElse("ProxyInternalProverNode", 0) should be > 1000
    fresh.getOrElse("ProverLeaf", 0) should be > 3000
  }
}
