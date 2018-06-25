package org.ergoplatform.utils

import org.bouncycastle.util.BigIntegers
import org.ergoplatform.ErgoBox.{BoxId, R3}
import org.ergoplatform.mining.EquihashSolution
import org.ergoplatform.mining.difficulty.RequiredDifficulty
import org.ergoplatform.modifiers.ErgoFullBlock
import org.ergoplatform.modifiers.history.{ADProofs, BlockTransactions, Header}
import org.ergoplatform.modifiers.mempool.{ErgoTransaction, TransactionIdsForHeader}
import org.ergoplatform.modifiers.state.{Insertion, StateChanges, UTXOSnapshotChunk}
import org.ergoplatform.nodeView.history.ErgoSyncInfo
import org.ergoplatform.nodeView.mempool.ErgoMemPool
import org.ergoplatform.nodeView.state.{BoxHolder, ErgoStateContext}
import org.ergoplatform.settings.Constants
import org.ergoplatform.{ErgoBox, ErgoBoxCandidate, Input}
import org.scalacheck.{Arbitrary, Gen}
import org.scalatest.Matchers
import scapi.sigma.DLogProtocol.DLogProverInput
import scorex.core.ModifierId
import scorex.crypto.authds.{ADDigest, ADKey, SerializedAdProof}
import scorex.crypto.hash.Digest32
import scorex.testkit.generators.CoreGenerators
import sigmastate.SBoolean
import sigmastate.Values.{IntConstant, TrueLeaf, Value}
import sigmastate.interpreter.{ContextExtension, SerializedProverResult}

trait ErgoGenerators extends CoreGenerators with Matchers {

  lazy val trueLeafGen: Gen[Value[SBoolean.type]] = Gen.const(TrueLeaf)
  lazy val smallPositiveInt: Gen[Int] = Gen.choose(1, 5)

  lazy val noProofGen: Gen[SerializedProverResult] =
    Gen.const(SerializedProverResult(Array.emptyByteArray, ContextExtension(Map())))

  lazy val ergoPropositionGen: Gen[Value[SBoolean.type]] = for {
    seed <- genBytes(32)
  } yield DLogProverInput(BigIntegers.fromUnsignedByteArray(seed)).publicImage

  lazy val ergoStateContextGen: Gen[ErgoStateContext] = for {
    height <- positiveIntGen
    digest <- stateRootGen
  } yield ErgoStateContext(height, digest)

  lazy val ergoBoxGen: Gen[ErgoBox] = for {
    prop <- ergoPropositionGen
    value <- positiveIntGen
    reg <- positiveIntGen
    transactionId: Array[Byte] <- genBytes(Constants.ModifierIdSize)
    boxId: Short <- Arbitrary.arbitrary[Short]
  } yield ErgoBox(value, prop, Map(R3 -> IntConstant(reg)), transactionId, boxId)

  lazy val ergoBoxGenNoProp: Gen[ErgoBox] = for {
    prop <- trueLeafGen
    value <- positiveIntGen
    reg <- positiveIntGen
    transactionId: Array[Byte] <- genBytes(Constants.ModifierIdSize)
    boxId: Short <- Arbitrary.arbitrary[Short]
  } yield ErgoBox(value, prop, Map(R3 -> IntConstant(reg)), transactionId, boxId)

  lazy val ergoBoxCandidateGen: Gen[ErgoBoxCandidate] = for {
    prop <- trueLeafGen
    value <- positiveIntGen
  } yield new ErgoBoxCandidate(value, prop)

  lazy val inputGen: Gen[Input] = for {
    boxId <- boxIdGen
    spendingProof <- noProofGen
  } yield Input(boxId, spendingProof)

  lazy val invalidErgoTransactionGen: Gen[ErgoTransaction] = for {
    from: IndexedSeq[Input] <- smallInt.flatMap(i => Gen.listOfN(i + 1, inputGen).map(_.toIndexedSeq))
    to: IndexedSeq[ErgoBoxCandidate] <- smallInt.flatMap(i => Gen.listOfN(i, ergoBoxCandidateGen).map(_.toIndexedSeq))
  } yield ErgoTransaction(from, to)

  lazy val positiveIntGen: Gen[Int] = Gen.choose(1, Int.MaxValue)

  lazy val boxesHolderGen: Gen[BoxHolder] = Gen.listOfN(2000, ergoBoxGenNoProp).map(l => BoxHolder(l))

  lazy val stateChangesGen: Gen[StateChanges] = ergoBoxGenNoProp
    .map(b => StateChanges(Seq(), Seq(Insertion(b))))

  lazy val ergoSyncInfoGen: Gen[ErgoSyncInfo] = for {
    ids <- Gen.nonEmptyListOf(modifierIdGen).map(_.take(ErgoSyncInfo.MaxBlockIds))
  } yield ErgoSyncInfo(ids)

  lazy val transactionIdsForHeaderGen: Gen[TransactionIdsForHeader] = for {
    idGenerator <- genBytes(Constants.ModifierIdSize)
    maxLength = 100
    toTake <- Gen.chooseNum(1, 100)
    ids <- Gen.listOfN(maxLength, idGenerator).map(_.take(toTake))
  } yield TransactionIdsForHeader(ModifierId @@ ids)

  lazy val digest32Gen: Gen[Digest32] = {
    val x = Digest32 @@ genBytes(32)
    x
  }

  lazy val boxIdGen: Gen[BoxId] = {
    val x = ADKey @@ genBytes(Constants.ModifierIdSize)
    x
  }

  lazy val stateRootGen: Gen[ADDigest] = {
    val x = ADDigest @@ genBytes(Constants.ModifierIdSize + 1)
    x
  }

  lazy val serializedAdProofGen: Gen[SerializedAdProof] = {
    val x = SerializedAdProof @@ genBoundedBytes(32, 32 * 1024)
    x
  }

  lazy val invalidHeaderGen: Gen[Header] = for {
    version <- Arbitrary.arbitrary[Byte]
    parentId <- modifierIdGen
    stateRoot <- stateRootGen
    adRoot <- digest32Gen
    transactionsRoot <- digest32Gen
    requiredDifficulty <- Arbitrary.arbitrary[BigInt]
    height <- Gen.choose(1, Int.MaxValue)
    equihashSolutions <- Gen.listOfN(EquihashSolution.length, Arbitrary.arbitrary[Int])
    interlinks <- Gen.nonEmptyListOf(modifierIdGen).map(_.take(128))
    timestamp <- positiveLongGen
    extensionHash <- digest32Gen
  } yield Header(version, parentId, interlinks, adRoot, stateRoot, transactionsRoot, timestamp,
    RequiredDifficulty.encodeCompactBits(requiredDifficulty), height, extensionHash, EquihashSolution(equihashSolutions))

  lazy val invalidBlockTransactionsGen: Gen[BlockTransactions] = for {
    headerId <- modifierIdGen
    txs <- Gen.nonEmptyListOf(invalidErgoTransactionGen)
  } yield BlockTransactions(headerId, txs)

  lazy val randomADProofsGen: Gen[ADProofs] = for {
    headerId <- modifierIdGen
    proof <- serializedAdProofGen
  } yield ADProofs(headerId, proof)

  lazy val randomUTXOSnapshotChunkGen: Gen[UTXOSnapshotChunk] = for {
    index: Short <- Arbitrary.arbitrary[Short]
    stateElements: Seq[ErgoBox] <- Gen.listOf(ergoBoxGenNoProp)
  } yield UTXOSnapshotChunk(stateElements, index)

  lazy val invalidErgoFullBlockGen: Gen[ErgoFullBlock] = for {
    header <- invalidHeaderGen
    txs <- invalidBlockTransactionsGen
    proof <- randomADProofsGen
  } yield ErgoFullBlock(header, txs, Some(proof))

  lazy val emptyMemPoolGen: Gen[ErgoMemPool] =
    Gen.resultOf({ _: Unit => ErgoMemPool.empty })(Arbitrary(Gen.const(())))
}
