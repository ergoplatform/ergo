package org.ergoplatform.db

import akka.util.ByteString
import com.google.common.primitives.Longs
import io.iohk.iodb.Store.{K, V}
import io.iohk.iodb.{ByteArrayWrapper, LSMStore}
import org.ergoplatform.db.LDBFactory.factory
import org.ergoplatform.modifiers.history.BlockTransactions
import org.ergoplatform.settings.Algos
import org.ergoplatform.utils.generators.ErgoTransactionGenerators
import org.iq80.leveldb.Options
import org.scalameter.KeyValue
import org.scalameter.api.{Bench, Gen, _}
import org.scalameter.picklers.Implicits._
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

  private val storeVLDB = new VersionedLDBKVStore(db0, keepVersions = 400)
  private val storeLDB = new LDBKVStore(db1)
  private val storeLSM = new LSMStore(createTempDir)

  private val modsNumGen = Gen.enumeration("modifiers number")(10, 100)

  val txsGen: Gen[Seq[BlockTransactions]] = modsNumGen.map { num =>
    (0 to num).flatMap { _ =>
      invalidBlockTransactionsGen.sample
    }
  }

  private val config = Seq[KeyValue](
    exec.minWarmupRuns -> 5,
    exec.maxWarmupRuns -> 10,
    exec.benchRuns -> 10,
    exec.requireGC -> true
  )

  private def randomVersion = ByteString(Algos.hash(Longs.toByteArray(Random.nextLong())))

  private def benchWriteLDB(bts: Seq[BlockTransactions]): Unit = {
    val toInsert = bts.map(bt => ByteString(idToBytes(bt.headerId)) -> ByteString(bt.bytes))
    storeLDB.insert(toInsert)
  }

  private def benchWriteVLDB(bts: Seq[BlockTransactions]): Unit = {
    val toInsert = bts.map(bt => ByteString(idToBytes(bt.headerId)) -> ByteString(bt.bytes))
    storeVLDB.insert(toInsert)(randomVersion)
  }

  private def benchWriteReadVLDB(bts: Seq[BlockTransactions]): Unit = {
    val toInsert = bts.map(bt => ByteString(idToBytes(bt.headerId)) -> ByteString(bt.bytes))
    storeVLDB.insert(toInsert)(randomVersion)
    bts.foreach { bt => storeVLDB.get(ByteString(idToBytes(bt.headerId))) }
  }

  private def benchGetAllVLDB: Seq[(ByteString, ByteString)] = storeVLDB.getAll

  private def benchWriteLSM(bts: Seq[BlockTransactions]): Unit = {
    val toInsert = bts.map(bt => ByteArrayWrapper(idToBytes(bt.headerId)) -> ByteArrayWrapper(bt.bytes))
    storeLSM.update(Random.nextLong(), Seq.empty, toInsert)
  }

  private def benchWriteReadLSM(bts: Seq[BlockTransactions]): Unit = {
    val toInsert = bts.map(bt => ByteArrayWrapper(idToBytes(bt.headerId)) -> ByteArrayWrapper(bt.bytes))
    storeLSM.update(Random.nextLong(), Seq.empty, toInsert)
    bts.foreach { bt => storeLSM.get(ByteArrayWrapper(idToBytes(bt.headerId))) }
  }

  private def benchGetAllLSM: Iterator[(K, V)] = storeLSM.getAll

  performance of "LDBStore vs LSMStore" in {
    performance of "LDBStore write" in {
      using(txsGen) config (config: _*) in (bts => benchWriteLDB(bts))
    }

    performance of "VLDBStore write" in {
      using(txsGen) config (config: _*) in (bts => benchWriteVLDB(bts))
    }
    performance of "VLDBStore write/read" in {
      using(txsGen) config (config: _*) in (bts => benchWriteReadVLDB(bts))
    }
    performance of "VLDBStore getAll" in {
      using(txsGen) config (config: _*) in (_ => benchGetAllVLDB)
    }

    performance of "LSMStore write" in {
      using(txsGen) config (config: _*) in (bts => benchWriteLSM(bts))
    }
    performance of "LSMStore write/read" in {
      using(txsGen) config (config: _*) in (bts => benchWriteReadLSM(bts))
    }
    performance of "LSMStore getAll" in {
      using(txsGen) config (config: _*) in (_ => benchGetAllLSM)
    }
  }

}
