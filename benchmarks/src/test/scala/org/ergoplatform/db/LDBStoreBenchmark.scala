package org.ergoplatform.db

import com.google.common.primitives.Longs
import org.ergoplatform.db.LDBFactory.factory
import org.ergoplatform.modifiers.history.BlockTransactions
import org.ergoplatform.settings.Algos
import org.ergoplatform.utils.generators.ErgoTransactionGenerators
import org.iq80.leveldb.Options
import org.scalameter.KeyValue
import org.scalameter.api.{Bench, Gen, _}
import org.scalameter.picklers.Implicits._
import scorex.crypto.hash.Digest32
import scorex.testkit.utils.FileUtils
import scorex.util.idToBytes

import scala.util.Random

object LDBStoreBenchmark
  extends Bench.ForkedTime
    with ErgoTransactionGenerators
    with FileUtils {

  private val options = new Options()
  options.createIfMissing(true)
  private val db0 = factory.open(createTempDir, options)
  private val db1 = factory.open(createTempDir, options)

  private def storeVLDB() = new VersionedLDBKVStore(db0, keepVersions = 400)
  private def storeLDB() = new LDBKVStore(db1)

  private val modsNumGen = Gen.enumeration("modifiers number")(1000, 10000)

  val txsGen: Gen[Seq[BlockTransactions]] = modsNumGen.map { num =>
    (0 to num).flatMap { _ =>
      invalidBlockTransactionsGen(defaultMinerPk, 10).sample
    }
  }

  val txsWithDbGen: Gen[(Seq[BlockTransactions], LDBKVStore)] = txsGen.map { bts =>
    val toInsert = bts.map(bt => idToBytes(bt.headerId) -> bt.bytes)
    val db = storeLDB()
    toInsert.grouped(5).foreach(db.insert)
    bts -> storeLDB
  }

  private val config = Seq[KeyValue](
    exec.minWarmupRuns -> 1,
    exec.maxWarmupRuns -> 2,
    exec.benchRuns -> 4,
    exec.requireGC -> true
  )

  private def randomVersion: Digest32 = Algos.hash(Longs.toByteArray(Random.nextLong()))

  private def benchWriteLDB(bts: Seq[BlockTransactions]): Unit = {
    val toInsert = bts.map(bt => idToBytes(bt.headerId) -> bt.bytes)
    val db = storeLDB()
    toInsert.grouped(5).foreach(db.insert)
  }

  private def benchReadLDB(bts: Seq[BlockTransactions], db: LDBKVStore): Unit = {
    bts.foreach { bt => db.get(idToBytes(bt.headerId)) }
  }

  private def benchWriteVLDB(bts: Seq[BlockTransactions]): Unit = {
    val toInsert = bts.map(bt => idToBytes(bt.headerId) -> bt.bytes)
    val db = storeVLDB()
    db.insert(toInsert)(randomVersion)
  }

  private def benchWriteReadVLDB(bts: Seq[BlockTransactions]): Unit = {
    val toInsert = bts.map(bt => idToBytes(bt.headerId) -> bt.bytes)
    val db = storeVLDB()
    db.insert(toInsert)(randomVersion)
    bts.foreach { bt => db.get(idToBytes(bt.headerId)) }
  }

  performance of "LDBStore vs LSMStore" in {
    performance of "LDBStore write" in {
      using(txsGen) config (config: _*) in (bts => benchWriteLDB(bts))
    }
    performance of "LDBStore read" in {
      using(txsWithDbGen) config (config: _*) in { case (bts, db) => benchReadLDB(bts, db) }
    }

    performance of "VLDBStore write" in {
      using(txsGen) config (config: _*) in (bts => benchWriteVLDB(bts))
    }
    performance of "VLDBStore write/read" in {
      using(txsGen) config (config: _*) in (bts => benchWriteReadVLDB(bts))
    }
  }

}
