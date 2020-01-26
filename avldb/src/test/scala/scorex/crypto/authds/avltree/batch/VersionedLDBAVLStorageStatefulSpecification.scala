package scorex.crypto.authds.avltree.batch

import com.google.common.primitives.Longs
import org.scalacheck.Test.Parameters
import org.scalacheck.commands.Commands
import org.scalacheck.{Gen, Prop}
import org.scalatest.PropSpec
import scorex.crypto.authds.avltree.batch.helpers.TestHelper
import scorex.crypto.authds._
import scorex.crypto.hash.Digest32
import scorex.utils.{Random => RandomBytes}

import scala.util.{Failure, Random, Success, Try}

class VersionedLDBAVLStorageStatefulSpecification extends PropSpec {
  val params = Parameters.default
    .withMinSize(10)
    .withMaxSize(50)
    .withMinSuccessfulTests(15)

  property("LDBAVLStorage: rollback in stateful environment") {
    WithLDB.property().check(params)
  }
}

object WithLDB extends VersionedLDBAVLStorageStatefulCommands with TestHelper {

  override protected val KL = 32
  override protected val VL = 8
  override protected val LL = 32

  override protected def createStatefulProver: PersistentBatchAVLProver[Digest32, HF] = {
    createPersistentProver(keepVersions)
  }
}

trait VersionedLDBAVLStorageStatefulCommands extends Commands { this: TestHelper =>

  override type State = Operations
  override type Sut = PersistentBatchAVLProver[Digest32, HF]
  val keepVersions = 1000

  private val MAXIMUM_GENERATED_OPERATIONS = 20
  private val MINIMUM_OPERATIONS_LENGTH = 10
  private val UPDATE_FRACTION = 2
  private val REMOVE_FRACTION = 4

  override def canCreateNewSut(newState: State,
                               initSuts: Traversable[State],
                               runningSuts: Traversable[Sut]): Boolean = true

  override def newSut(state: State): Sut = createStatefulProver

  protected def createStatefulProver: PersistentBatchAVLProver[Digest32, HF]

  override def destroySut(sut: Sut): Unit = ()

  override def initialPreCondition(state: State): Boolean = state.operations.isEmpty

  override def genInitialState: Gen[State] = Gen.const(Operations())

  override def genCommand(state: State): Gen[Command] = {
    val appendsCommandsLength = Random.nextInt(MAXIMUM_GENERATED_OPERATIONS) + MINIMUM_OPERATIONS_LENGTH

    val keys = (0 until appendsCommandsLength).map { _ => ADKey @@ RandomBytes.randomBytes(KL) }.toList
    val removedKeys = state.operations.filter(_.isInstanceOf[Remove]).map(_.key).distinct
    val prevKeys = state.operations.map(_.key).distinct.filterNot(k1 => removedKeys.exists { k2 => k1.sameElements(k2) })
    val uniqueKeys = keys.filterNot(prevKeys.contains).distinct
    val updateKeys = Random.shuffle(prevKeys).take(safeDivide(prevKeys.length, UPDATE_FRACTION))
    val removeKeys = Random.shuffle(prevKeys).take(safeDivide(prevKeys.length, REMOVE_FRACTION))

    val appendCommands = uniqueKeys.map { k => Insert(k, ADValue @@ Longs.toByteArray(nextPositiveLong)) }
    val updateCommands = updateKeys.map { k => UpdateLongBy(k, nextPositiveLong) }
    val removeCommands = removeKeys.map { k => Remove(k) }

    val all = appendCommands ++ updateCommands ++ removeCommands

    Gen.frequency(
      3 -> BackAndForthCheck(all),
      3 -> BackAndForthTwoTimesCheck(all),
      2 -> RollbackMoreThanOneVersionRandomly(all),
      2 -> ApplyAndRollback(all),
      1 -> BackAndForthDoubleCheck(all),
      1 -> SimpleCheck(all)
    )
  }

  private def safeDivide(base: Int, fraction: Int): Int = if (base > fraction) base / fraction else 0

  private def nextPositiveLong: Long = Random.nextInt(Int.MaxValue).toLong

  case class Operations(operations: List[Operation] = List.empty[Operation]) {
    def include(ops: List[Operation]): Operations = Operations(operations ++ ops)
  }

  case class BackAndForthCheck(ops: List[Operation]) extends Command {

    override type Result = ResultData

    override def run(sut: PersistentBatchAVLProver[Digest32, HF]): Result = {
      val digest = sut.digest
      ops.foreach(sut.performOneOperation)
      sut.checkTree(postProof = false)
      val proof = sut.generateProofAndUpdateStorage()
      sut.checkTree(postProof = true)

      sut.rollback(digest).get
      val updatedDigest = sut.digest
      require(digest.sameElements(updatedDigest))
      ops.foreach(sut.performOneOperation)
      sut.checkTree(postProof = false)
      val sameProof = sut.generateProofAndUpdateStorage()
      sut.checkTree(postProof = true)
      val updatedPostDigest = sut.digest

      ResultData(updatedDigest, updatedPostDigest, proof, proof.sameElements(sameProof))
    }

    override def nextState(state: Operations): Operations = state.include(ops)

    override def preCondition(state: Operations): Boolean = true

    override def postCondition(state: Operations, result: Try[Result]): Prop = {

      val propBoolean = result match {
        case Success(data) =>
          val verifier = new BatchAVLVerifier[Digest32, HF](data.digest, data.proof, KL, Some(VL))
          ops.foreach(verifier.performOneOperation)
          data.consistent && verifier.digest.exists(_.sameElements(data.postDigest))
        case Failure(_) =>
          false
      }
      Prop.propBoolean(propBoolean)
    }

    case class ResultData(digest: ADDigest, postDigest: ADDigest, proof: SerializedAdProof, consistent: Boolean)

  }

  case class BackAndForthTwoTimesCheck(ops: List[Operation]) extends Command {

    override type Result = ResultData

    override def run(sut: PersistentBatchAVLProver[Digest32, HF]): Result = {

      val (firstBatch, secondBatch) = ops.splitAt(ops.length / 2)

      val digest1 = sut.digest
      firstBatch.foreach(sut.performOneOperation)
      sut.checkTree(postProof = false)
      val proof1 = sut.generateProofAndUpdateStorage()
      sut.checkTree(postProof = true)
      val digest2 = sut.digest
      secondBatch.foreach(sut.performOneOperation)
      sut.checkTree(postProof = false)
      val proof2 = sut.generateProofAndUpdateStorage()
      sut.checkTree(postProof = true)

      sut.rollback(digest1).get
      val updatedDigest = sut.digest
      require(digest1.sameElements(updatedDigest))
      ops.foreach(sut.performOneOperation)
      sut.checkTree(postProof = false)
      sut.checkTree(postProof = true)
      val updatedPostDigest = sut.digest

      ResultData(digest1, digest2, updatedPostDigest, proof1, proof2)
    }

    override def nextState(state: Operations): Operations = state.include(ops)

    override def preCondition(state: Operations): Boolean = true

    override def postCondition(state: Operations, result: Try[Result]): Prop = {
      val propBoolean = result match {
        case Success(data) =>
          val (firstBatch, secondBatch) = ops.splitAt(ops.length / 2)
          val verifier1 = new BatchAVLVerifier[Digest32, HF](data.digest1, data.proof1, KL, Some(VL))
          val verifier2 = new BatchAVLVerifier[Digest32, HF](data.digest2, data.proof2, KL, Some(VL))
          firstBatch.foreach(verifier1.performOneOperation)
          secondBatch.foreach(verifier2.performOneOperation)
          verifier1.digest.exists(_.sameElements(data.digest2)) &&
            verifier2.digest.exists(_.sameElements(data.postDigest))
        case Failure(_) =>
          false
      }
      Prop.propBoolean(propBoolean)
    }

    case class ResultData(digest1: ADDigest,
                          digest2: ADDigest,
                          postDigest: ADDigest,
                          proof1: SerializedAdProof,
                          proof2: SerializedAdProof)

  }

  case class BackAndForthDoubleCheck(ops: List[Operation]) extends Command {

    override type Result = ResultData

    override def run(sut: PersistentBatchAVLProver[Digest32, HF]): Result = {
      val digest = sut.digest
      ops.foreach(sut.performOneOperation)
      sut.checkTree(postProof = false)
      val proof = sut.generateProofAndUpdateStorage()
      sut.checkTree(postProof = true)
      val postDigest = sut.digest

      sut.rollback(digest).get
      val digest2 = sut.digest
      require(digest.sameElements(digest2))
      ops.foreach(sut.performOneOperation)
      sut.checkTree(postProof = false)
      val proof2 = sut.generateProofAndUpdateStorage()
      sut.checkTree(postProof = true)
      val postDigest2 = sut.digest

      ResultData(digest, postDigest, digest2, postDigest2, proof, proof2)
    }

    override def nextState(state: Operations): Operations = state.include(ops)

    override def preCondition(state: Operations): Boolean = true

    override def postCondition(state: Operations, result: Try[Result]): Prop = {

      val propBoolean = result match {
        case Success(data) =>
          val verifier1 = new BatchAVLVerifier[Digest32, HF](data.digest, data.proof, KL, Some(VL))
          val verifier2 = new BatchAVLVerifier[Digest32, HF](data.digest2, data.proof2, KL, Some(VL))
          ops.foreach(verifier1.performOneOperation)
          ops.foreach(verifier2.performOneOperation)
          val verifiedFirstDataSet = verifier1.digest.exists(_.sameElements(data.postDigest))
          val verifiedSecondDataSet = verifier2.digest.exists(_.sameElements(data.postDigest2))
          verifiedFirstDataSet && verifiedSecondDataSet && data.proof.sameElements(data.proof2)
        case Failure(_) =>
          false
      }
      Prop.propBoolean(propBoolean)
    }

    case class ResultData(digest: ADDigest,
                          postDigest: ADDigest,
                          digest2: ADDigest,
                          postDigest2: ADDigest,
                          proof: SerializedAdProof,
                          proof2: SerializedAdProof)

  }

  case class ApplyAndRollback(ops: List[Operation]) extends UnitCommand {

    override def run(sut: PersistentBatchAVLProver[Digest32, HF]): Unit = {
      val digest = sut.digest
      ops.foreach(sut.performOneOperation)
      sut.checkTree(postProof = false)
      sut.generateProofAndUpdateStorage()
      sut.checkTree(postProof = true)
      sut.rollback(digest).get
      require(digest.sameElements(sut.digest))
    }

    override def nextState(state: Operations): Operations = state

    override def preCondition(state: Operations): Boolean = true

    override def postCondition(state: Operations, result: Boolean): Prop = Prop.propBoolean(result)

  }

  case class RollbackMoreThanOneVersionRandomly(ops: List[Operation]) extends UnitCommand {

    private val STEP_SIZE = 5

    override def run(sut: PersistentBatchAVLProver[Digest32, HF]): Result = {
      val splitOps = splitOpsIntoBatches

      val digest = sut.digest
      splitOps.foreach { operations =>
        operations.foreach(sut.performOneOperation)
        sut.checkTree(postProof = false)
        sut.generateProofAndUpdateStorage()
        sut.checkTree(postProof = true)
      }

      sut.rollback(digest).get
      val updatedDigest = sut.digest
      require(digest.sameElements(updatedDigest))
      splitOps.foreach { operations =>
        operations.foreach(sut.performOneOperation)
        sut.checkTree(postProof = false)
        sut.generateProofAndUpdateStorage()
        sut.checkTree(postProof = true)
      }
    }

    private def splitOpsIntoBatches: List[List[Operation]] = Range(0, ops.length, STEP_SIZE).map { i =>
      ops.slice(i, i + STEP_SIZE)
    }.toList

    override def nextState(state: Operations): Operations = state.include(ops)

    override def preCondition(state: Operations): Boolean = true

    override def postCondition(state: Operations, result: Boolean): Prop = Prop.propBoolean(result)
  }

  case class SimpleCheck(ops: List[Operation]) extends Command {

    override type Result = ResultData

    override def run(sut: PersistentBatchAVLProver[Digest32, HF]): Result = {
      val digest = sut.digest
      ops.foreach(sut.performOneOperation)
      sut.checkTree(postProof = false)
      val proof = sut.generateProofAndUpdateStorage()
      sut.checkTree(postProof = true)
      val postDigest = sut.digest
      ResultData(digest, postDigest, proof)
    }

    override def nextState(state: Operations): Operations = state.include(ops)

    override def preCondition(state: Operations): Boolean = true

    override def postCondition(state: Operations, result: Try[Result]): Prop = {
      val propBoolean = result match {
        case Success(data) =>
          val verifier = new BatchAVLVerifier[Digest32, HF](data.digest, data.proof, KL, Some(VL))
          ops.foreach(verifier.performOneOperation)
          verifier.digest.exists(_.sameElements(data.postDigest))
        case Failure(_) =>
          false
      }
      Prop.propBoolean(propBoolean)
    }

    case class ResultData(digest: ADDigest, postDigest: ADDigest, proof: SerializedAdProof)
  }
}
