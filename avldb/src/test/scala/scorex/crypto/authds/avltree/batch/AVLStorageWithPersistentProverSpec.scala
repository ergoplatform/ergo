package scorex.crypto.authds.avltree.batch

import io.iohk.iodb.{LSMStore, Store}
import org.scalatest.{Matchers, PropSpec}
import scorex.crypto.authds.avltree.batch.benchmark.IODBBenchmark.getRandomTempDir
import scorex.crypto.authds.{ADDigest, ADKey, ADValue, SerializedAdProof}
import scorex.crypto.hash.{Blake2b256, Digest32}
import scorex.utils.Random
import scorex.db.LDBVersionedStore

import scala.util.{Failure, Success, Try}

class AVLStorageWithPersistentProverSpec extends PropSpec with Matchers {

  type HF = Blake2b256.type
  implicit val hf: HF = Blake2b256

  val stateStore: Store = new LDBVersionedStore(getRandomTempDir, 10)

  private lazy val np =
    NodeParameters(keySize = 32, valueSize = None, labelSize = 32)

  protected lazy val storage = new VersionedIODBAVLStorage(stateStore, np)

  protected lazy val persistentProver: PersistentBatchAVLProver[Digest32, HF] =
    PersistentBatchAVLProver.create(
      new BatchAVLProver[Digest32, HF](
        keyLength = 32, valueLengthOpt = None), storage).get

  def genProof(mods: Seq[Modification], rollBackTo: ADDigest): Try[(SerializedAdProof, ADDigest)] = {

    def rollback(): Try[Unit] = Try(
      persistentProver.rollback(rollBackTo).ensuring(_.isSuccess && persistentProver.digest.sameElements(rollBackTo))
    ).flatten

    Try {
      if (!(persistentProver.digest.sameElements(rollBackTo) &&
        storage.version.get.sameElements(rollBackTo) &&
        stateStore.lastVersionID.get.data.sameElements(rollBackTo))) Failure(new Error("Bad state version."))

      mods.foldLeft[Try[Option[ADValue]]](Success(None)) { case (t, m) =>
        t.flatMap(_ => {
          val opRes = persistentProver.performOneOperation(m)
          opRes
        })
      }.get

      val proof = persistentProver.generateProofAndUpdateStorage()
      val digest = persistentProver.digest

      proof -> digest
    } match {
      case Success(result) => rollback().map(_ => result)
      case Failure(e) => rollback().flatMap(_ => Failure(e))
    }
  }

  def applyModifications(mods: Seq[Modification]): Unit =
    mods.foreach(m => {
      persistentProver.performOneOperation(m).ensuring(_.isSuccess, "Mod application failed.")
    })

  private val iMods32 = (0 until 100)
    .map(i => Insert(ADKey @@ Random.randomBytes(), ADValue @@ Array.fill(32)(i.toByte)))

  private val iMods64 = (0 until 100)
    .map(i => Insert(ADKey @@ Random.randomBytes(), ADValue @@ Array.fill(64)(i.toByte)))

  private val iMods128 = (0 until 100)
    .map(i => Insert(ADKey @@ Random.randomBytes(), ADValue @@ Array.fill(128)(i.toByte)))

  private val mods32 = (0 until 50)
    .map(i => Insert(ADKey @@ Random.randomBytes(), ADValue @@ Array.fill(32)(i.toByte)))

  private val mods64 = (0 until 150)
    .map(i => Insert(ADKey @@ Random.randomBytes(), ADValue @@ Array.fill(64)(i.toByte)))

  private val mods128 = (0 until 50)
    .map(i => Insert(ADKey @@ Random.randomBytes(), ADValue @@ Array.fill(128)(i.toByte)))

  property("Proof Digest should equal Actual Digest after mods application. 32-bytes values at genesis, then 128") {

    // Setting up initial state.
    applyModifications(iMods32)

    persistentProver.generateProofAndUpdateStorage()

    lazy val afterGenesisDigest = persistentProver.digest

    val proof = genProof(mods128, afterGenesisDigest)

    // Applying mods with greater values.
    applyModifications(mods128)

    persistentProver.generateProofAndUpdateStorage()

    proof.isSuccess shouldBe true

    proof.get._2.sameElements(persistentProver.digest) shouldBe true

    mods128.forall(m => persistentProver.unauthenticatedLookup(m.key).isDefined) shouldBe true
  }

  property("Modifiers should be found in storage after sequential application of modifiers of variable size.") {

    applyModifications(iMods128)

    persistentProver.generateProofAndUpdateStorage()

    applyModifications(mods64)

    applyModifications(mods32)

    persistentProver.generateProofAndUpdateStorage()

    iMods128.forall(m => persistentProver.unauthenticatedLookup(m.key).isDefined) shouldBe true

    mods64.forall(m => persistentProver.unauthenticatedLookup(m.key).isDefined) shouldBe true

    mods32.forall(m => persistentProver.unauthenticatedLookup(m.key).isDefined) shouldBe true
  }
}
