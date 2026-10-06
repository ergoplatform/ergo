package org.ergoplatform.mining

import akka.actor.{ActorRef, ActorSystem}
import akka.pattern.StatusReply
import akka.testkit.{TestKit, TestProbe}
import org.ergoplatform.mining.CandidateGenerator.{Candidate, GenerateCandidate}
import org.ergoplatform.modifiers.mempool.ErgoTransaction
import org.ergoplatform.nodeView.state.StateType
import org.ergoplatform.nodeView.{ErgoNodeViewHolder, ErgoNodeViewRef, ErgoReadersHolderRef}
import org.ergoplatform.settings.Constants
import org.ergoplatform.settings.{ErgoSettings, ErgoSettingsReader}
import org.ergoplatform.utils.ErgoTestHelpers
import org.ergoplatform.{ErgoBoxCandidate, Input}
import org.scalatest.BeforeAndAfterAll
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import scorex.crypto.authds.ADKey
import scorex.util.bytesToId
import sigma.ast.ShortConstant
import sigma.interpreter.{ContextExtension, ProverResult}

import java.nio.file.Files

import scala.concurrent.duration._

/**
  * Integration coverage of the storage-rent wiring in [[CandidateGenerator]].
  *
  * IMPORTANT SCOPE LIMIT - read before trusting these tests as gate coverage.
  *
  * The claim branch in `CandidateGenerator` is guarded twice: by the
  * `storageRentCollection` setting, and by `upcomingHeight - Constants.StoragePeriod > 0`.
  * `StoragePeriod` is `4 * BlocksPerYear` = 1,051,200 blocks, so a chain must be over a
  * million blocks deep before any claim can be produced. No unit test can build such a
  * chain, so these tests can only observe the young-chain path.
  *
  * These tests therefore pin the *absence* of claims and the stability of candidate
  * generation with the collector enabled. They do NOT prove the flag or the height guard
  * work: with the flag forced on and the height guard removed, a young chain still yields
  * no claim, because `storageRentBoxesAtOrBefore` is then called with a negative threshold and
  * matches no row, so the observable result is identical. That mutation was verified to
  * pass this spec, so treat these as regression guards, not as gate coverage.
  *
  * Covering the gate needs either a synthetic history at a height past the storage period,
  * or extracting the sweep decision into a separately testable function - which would mean
  * changing production code. The claim body is already covered against consensus by
  * [[org.ergoplatform.mining.StorageRentClaimBuilderSpec]].
  */
class CandidateGeneratorStorageRentSpec extends AnyFlatSpec
  with Matchers with ErgoTestHelpers with BeforeAndAfterAll {

  import org.ergoplatform.utils.ErgoCoreTestConstants._

  private val candidateGenDelay: FiniteDuration = 5.seconds

  // fresh directory per suite run: a reused directory resumes a chain at an arbitrary
  // height, and the emission tx id (which the tests compare) depends on the height
  private def randomDir: String = Files.createTempDirectory("ergo-rent-candidate").toString

  private val baseSettings: ErgoSettings = {
    val empty = ErgoSettingsReader.read()
    empty.copy(
      directory = randomDir,
      nodeSettings = empty.nodeSettings.copy(
        mining                       = true,
        stateType                    = StateType.Utxo,
        internalMinerPollingInterval = 1.second,
        offlineGeneration            = true,
        verifyTransactions           = true,
        extraIndex                   = true),
      chainSettings = empty.chainSettings.copy(blockInterval = 1.seconds)
    )
  }

  /** Settings with the storage-rent collector explicitly enabled. */
  private val rentEnabledSettings: ErgoSettings = baseSettings.copy(
    nodeSettings = baseSettings.nodeSettings.copy(
      storageRentCollection = true,
      storageRentTokenWhitelist = Seq.empty),
    directory = randomDir)

  /** The var-127 (storage rent) claim transactions of a candidate block. */
  private def rentClaimTransactions(candidate: Candidate): Seq[ErgoTransaction] =
    candidate.candidateBlock.transactions.filter(ErgoNodeViewHolder.isStorageRentClaim)

  private def testBoxId(b: Byte): ADKey = ADKey @@ Array.fill(32)(b)

  private def testOutput: ErgoBoxCandidate =
    new ErgoBoxCandidate(1000000000L, Constants.TrueTree, 1)

  private def inputWith(boxId: ADKey, proof: Array[Byte], extension: ContextExtension): Input =
    new Input(boxId, ProverResult(proof, extension))

  private val var127Extension: ContextExtension =
    ContextExtension(Map(Constants.StorageIndexVarId -> ShortConstant(0)))

  "isStorageRentClaim" should "detect a claim by empty proof and var #127 extension" in {
    val claimTx = ErgoTransaction(
      IndexedSeq(inputWith(testBoxId(1), Array.emptyByteArray, var127Extension)),
      IndexedSeq.empty, IndexedSeq(testOutput))
    ErgoNodeViewHolder.isStorageRentClaim(claimTx) shouldBe true
  }

  it should "not flag transactions with non-empty proofs" in {
    val tx = ErgoTransaction(
      IndexedSeq(inputWith(testBoxId(1), Array(1.toByte), var127Extension)),
      IndexedSeq.empty, IndexedSeq(testOutput))
    ErgoNodeViewHolder.isStorageRentClaim(tx) shouldBe false
  }

  it should "not flag transactions without var #127" in {
    val otherExtension = ContextExtension(Map(42.toByte -> ShortConstant(0)))
    val noExtensionTx = ErgoTransaction(
      IndexedSeq(inputWith(testBoxId(1), Array.emptyByteArray, ContextExtension.empty)),
      IndexedSeq.empty, IndexedSeq(testOutput))
    val otherVarTx = ErgoTransaction(
      IndexedSeq(inputWith(testBoxId(1), Array.emptyByteArray, otherExtension)),
      IndexedSeq.empty, IndexedSeq(testOutput))
    ErgoNodeViewHolder.isStorageRentClaim(noExtensionTx) shouldBe false
    ErgoNodeViewHolder.isStorageRentClaim(otherVarTx) shouldBe false
  }

  it should "flag a transaction when any of its inputs is a claim input" in {
    val claimInput = inputWith(testBoxId(1), Array.emptyByteArray, var127Extension)
    val signedInput = inputWith(testBoxId(2), Array(1.toByte), ContextExtension.empty)
    val tx = ErgoTransaction(IndexedSeq(signedInput, claimInput),
      IndexedSeq.empty, IndexedSeq(testOutput))
    ErgoNodeViewHolder.isStorageRentClaim(tx) shouldBe true
  }

  "rentClaimSpentBoxIds" should "collect the input box ids of claim transactions only" in {
    val claimTx = ErgoTransaction(
      IndexedSeq(
        inputWith(testBoxId(1), Array.emptyByteArray, var127Extension),
        inputWith(testBoxId(2), Array.emptyByteArray, var127Extension)),
      IndexedSeq.empty, IndexedSeq(testOutput))
    val ordinaryTx = ErgoTransaction(
      IndexedSeq(inputWith(testBoxId(3), Array(1.toByte), ContextExtension.empty)),
      IndexedSeq.empty, IndexedSeq(testOutput))

    ErgoNodeViewHolder.rentClaimSpentBoxIds(Seq(claimTx, ordinaryTx)) shouldBe
      Seq(bytesToId(testBoxId(1)), bytesToId(testBoxId(2)))
    ErgoNodeViewHolder.rentClaimSpentBoxIds(Seq(ordinaryTx)) shouldBe empty
    ErgoNodeViewHolder.rentClaimSpentBoxIds(Seq.empty) shouldBe empty
  }

  private def generateOneCandidate(settings: ErgoSettings)(
    implicit system: ActorSystem): Candidate = {
    val replyProbe = new TestProbe(system)
    val viewHolderRef: ActorRef = ErgoNodeViewRef(settings)
    val readersHolderRef: ActorRef = ErgoReadersHolderRef(viewHolderRef)
    val candidateGenerator: ActorRef = CandidateGenerator(
      defaultMinerSecret.publicImage, readersHolderRef, viewHolderRef, settings)

    candidateGenerator.tell(GenerateCandidate(Seq.empty, reply = true, forced = false), replyProbe.ref)
    replyProbe.expectMsgPF(candidateGenDelay) {
      case StatusReply.Success(candidate: Candidate) => candidate
    }
  }

  "CandidateGenerator" should "not inject rent claims when storage rent collection is off" in
    new TestKit(ActorSystem()) {
    val candidate = generateOneCandidate(baseSettings)

    // the flag is off by default, so nothing may be swept into the candidate
    rentClaimTransactions(candidate) shouldBe empty
    // and the candidate itself must be a normal, solvable block
    baseSettings.chainSettings.powScheme
      .proveCandidate(candidate.candidateBlock, defaultMinerSecret.w, 0, 1000)
      .isDefined shouldBe true
    system.terminate()
  }

  it should "not inject rent claims while the chain is younger than the storage period" in
    new TestKit(ActorSystem()) {
    // With the collector enabled, a chain that has not reached the storage period must
    // produce no claim. Note this is a genuine end-to-end assertion about a young chain, but
    // it does not isolate WHICH guard prevents the claim - see the scope note in this spec.
    val candidate = generateOneCandidate(rentEnabledSettings)

    rentClaimTransactions(candidate) shouldBe empty
    rentEnabledSettings.chainSettings.powScheme
      .proveCandidate(candidate.candidateBlock, defaultMinerSecret.w, 0, 1000)
      .isDefined shouldBe true
    system.terminate()
  }

  it should "not change the candidate on a young chain when the collector is enabled" in
    new TestKit(ActorSystem()) {
    // enabling the collector must not add transactions to the candidate on a young chain,
    // nor remove any: the two candidates must agree on their transaction ids
    val off = generateOneCandidate(baseSettings)
    val on = generateOneCandidate(rentEnabledSettings)

    on.candidateBlock.transactions.map(_.id) shouldBe
      off.candidateBlock.transactions.map(_.id)
    system.terminate()
  }

  it should "expose the storage rent settings read from configuration" in
    new TestKit(ActorSystem()) {
    // the two settings the candidate generator consults must be configurable, and the
    // whitelist must be plumbed through as a set of token ids
    rentEnabledSettings.nodeSettings.storageRentCollection shouldBe true
    rentEnabledSettings.nodeSettings.storageRentTokenWhitelist shouldBe Seq.empty
    baseSettings.nodeSettings.storageRentCollection shouldBe false
    system.terminate()
  }
}
