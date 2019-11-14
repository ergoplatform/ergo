package scorex.crypto.authds.avltree.batch.benchmark

import scorex.crypto.authds._
import scorex.crypto.authds.avltree.batch.helpers.FileHelper
import scorex.crypto.authds.avltree.batch.{VersionedIODBAVLStorage, _}
import scorex.crypto.hash.{Blake2b256, Digest32}
import scorex.utils.Random
import scorex.db.LDBVersionedStore

object BatchingBenchmark extends App with FileHelper {
  val KeyLength = 26
  val ValueLength = 8
  val LabelLength = 32
  val InitialMods = 0
  val NumMods = 200000


  implicit val hf = Blake2b256
  type HF = Blake2b256.type

  val store = new LDBVersionedStore(getRandomTempDir, keepVersions = 10)
  val storage = new VersionedIODBAVLStorage(store, NodeParameters(KeyLength, Some(ValueLength), LabelLength))
  require(storage.isEmpty)
  val mods = generateModifications()
  var digest: ADDigest = ADDigest @@ Array[Byte]()
  var numInserts = 0

  println(s"NumInserts = $numInserts")
  println("Step, In-memory prover time(s.), Persistent prover time(s.), Rollback time(s.)")

  bench()

  def bench(): Unit = {
    val prover = new BatchAVLProver[Digest32, HF](KeyLength, Some(ValueLength), None)
    val persProver = PersistentBatchAVLProver.create[Digest32, HF](
      new BatchAVLProver[Digest32, HF](KeyLength, Some(ValueLength), None),
      storage,
      paranoidChecks = true).get

    val Step = 2000
    digest = persProver.digest
    require(persProver.digest sameElements prover.digest)
    (0 until(NumMods, Step)) foreach { i =>
      oneStep(i, Step, i / 2, persProver, prover)
    }
  }


  def oneStep(i: Int, step: Int, toPrint: Int, persProver: PersistentBatchAVLProver[Digest32, HF],
              prover: BatchAVLProver[Digest32, HF]): Unit = {

    System.gc()
    val converted = mods.slice(i, i + step)

    val (persProverTime, _) = time {
      converted.foreach(c => persProver.performOneOperation(c))
      persProver.digest
      persProver.generateProofAndUpdateStorage()
    }

    if (scala.util.Random.nextInt(50) == 49) {
      println("rollback: ")
      val (rollbackTime, _) = time {
        persProver.rollback(digest).get
        persProver.digest
      }
      println("rollback time: " + rollbackTime)
      converted.foreach(c => persProver.performOneOperation(c))
      persProver.generateProofAndUpdateStorage()
    }

    val (proverTime, _) = time {
      converted.foreach(c => prover.performOneOperation(c))
      prover.generateProof
      prover.digest
    }

    digest = persProver.digest
    assert(prover.digest sameElements digest)

    println(s"$toPrint, $proverTime, $persProverTime") //, $rollbackTime")
  }

  def time[R](block: => R): (Double, R) = {
    val t0 = System.currentTimeMillis()
    val result = block // call-by-name
    val t1 = System.currentTimeMillis()
    ((t1 - t0) / 1000.0, result)
  }

  def generateModifications(): Array[Modification] = {
    val mods = new Array[Modification](NumMods)

    for (i <- 0 until NumMods) {
      if (i == 0 || i < InitialMods || i % 2 == 0) {
        // with prob ~.5 insert a new one, with prob ~.5 update an existing one
        mods(i) = Insert(ADKey @@ Random.randomBytes(KeyLength), ADValue @@ Random.randomBytes(8))
        numInserts += 1
      } else {
        val j = Random.randomBytes(3)
        mods(i) = Update(mods((j(0).toInt.abs + j(1).toInt.abs * 128 + j(2).toInt.abs * 128 * 128) % i).key,
          ADValue @@ Random.randomBytes(8))
      }
    }
    mods
  }
}
