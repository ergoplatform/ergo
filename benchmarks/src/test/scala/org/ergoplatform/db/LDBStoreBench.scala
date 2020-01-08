package org.ergoplatform.db

import com.google.common.primitives.Longs
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
import scorex.db.LDBFactory.factory
import scorex.db.{LDBKVStore, LDBVersionedStore}

import scala.util.Random

object LDBStoreBench
  extends Bench.ForkedTime
    with ErgoTransactionGenerators
    with FileUtils {

  private val options = new Options()
  options.createIfMissing(true)
  private val db0 = factory.open(createTempDir, options)
  private val db1 = factory.open(createTempDir, options)

  private def storeLDB() = new LDBKVStore(db1)
  private def storeLVDB() = new LDBVersionedStore(createTempDir, keepVersions = 400)

  private val modsNumGen = Gen.enumeration("modifiers number")(1000)

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
    exec.maxWarmupRuns -> 1,
    exec.benchRuns -> 10,
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

  private def benchWriteLVDB(bts: Seq[BlockTransactions]): Unit = {
    val toInsert = bts.map(bt => idToBytes(bt.headerId) -> bt.bytes)
    val db = storeLVDB()
    db.update(randomVersion, List.empty, toInsert)
  }

  private def benchWriteReadLVDB(bts: Seq[BlockTransactions]): Unit = {
    val toInsert = bts.map(bt => idToBytes(bt.headerId) -> bt.bytes)
    val db = storeLVDB()
    db.update(randomVersion, List.empty, toInsert)
    bts.foreach { bt => db.get(idToBytes(bt.headerId)) }
  }


  performance of "LDBStore vs LSMStore" in {
    performance of "LDBStore write" in {
      using(txsGen) config(config: _*) in (bts => benchWriteLDB(bts))
    }
    performance of "LDBStore read" in {
      using(txsWithDbGen) config(config: _*) in { case (bts, db) => benchReadLDB(bts, db) }
    }

    performance of "LVDBStore write" in {
      using(txsGen) config(config: _*) in (bts => benchWriteLVDB(bts))
    }

    performance of "LVDBStore write/read" in {
      using(txsGen) config(config: _*) in (bts => benchWriteReadLVDB(bts))
    }
  }
}
