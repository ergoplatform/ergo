package org.ergoplatform.utils

import org.ergoplatform.ErgoBox.{BoxId, NonMandatoryIdentifier, R3}
import org.ergoplatform.{ErgoBox, ErgoBoxCandidate, Input}
import org.ergoplatform.mining.difficulty.RequiredDifficulty
import org.ergoplatform.mining.{DefaultFakePowScheme, EquihashSolution}
import org.ergoplatform.modifiers.ErgoFullBlock
import org.ergoplatform.modifiers.history.{ADProofs, BlockTransactions, Header}
import org.ergoplatform.modifiers.mempool.{ErgoTransaction, TransactionIdsForHeader}
import org.ergoplatform.modifiers.state.{Insertion, StateChanges, UTXOSnapshotChunk}
import org.ergoplatform.nodeView.WrappedUtxoState
import org.ergoplatform.nodeView.history.ErgoSyncInfo
import org.ergoplatform.nodeView.mempool.ErgoMemPool
import org.ergoplatform.nodeView.state.{BoxHolder, UtxoState}
import org.ergoplatform.settings.Constants
import org.scalacheck.{Arbitrary, Gen}
import org.scalatest.Matchers
import scorex.core.ModifierId
import scorex.crypto.authds.{ADDigest, ADKey, SerializedAdProof}
import scorex.crypto.hash.Digest32
import scorex.testkit.generators.CoreGenerators
import sigmastate.{SBoolean, SType}
import sigmastate.Values.{EvaluatedValue, IntConstant, TrueLeaf, Value}
import sigmastate.interpreter.{ContextExtension, SerializedProverResult}

import scala.annotation.tailrec
import scala.util.{Random, Try}

trait ErgoGenerators extends CoreGenerators with Matchers {

  lazy val trueLeafGen: Gen[Value[SBoolean.type]] = Gen.const(TrueLeaf)
  lazy val smallPositiveInt: Gen[Int] = Gen.choose(1, 5)

  lazy val noProofGen: Gen[SerializedProverResult] =
    Gen.const(SerializedProverResult(Array.emptyByteArray, ContextExtension(Map())))

  lazy val ergoBoxGen: Gen[ErgoBox] = for {
    prop <- trueLeafGen
    value <- positiveIntGen
    reg <- positiveIntGen
    transactionId: Array[Byte] <- genBytesList(Constants.ModifierIdSize)
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


  lazy val boxesHolderGen: Gen[BoxHolder] = Gen.listOfN(2000, ergoBoxGen).map(l => BoxHolder(l))

  lazy val stateChangesGen: Gen[StateChanges] = ergoBoxGen
    .map(b => StateChanges(Seq(Insertion(b))))

  lazy val ergoSyncInfoGen: Gen[ErgoSyncInfo] = for {
    ids <- Gen.nonEmptyListOf(modifierIdGen).map(_.take(ErgoSyncInfo.MaxBlockIds))
  } yield ErgoSyncInfo(ids)

  lazy val transactionIdsForHeaderGen: Gen[TransactionIdsForHeader] = for {
    idGenerator <- genBytesList(Constants.ModifierIdSize)
    maxLength = 100
    toTake <- Gen.chooseNum(1, 100)
    ids <- Gen.listOfN(maxLength, idGenerator).map(_.take(toTake))
  } yield TransactionIdsForHeader(ModifierId @@ ids)

  lazy val digest32Gen: Gen[Digest32] = {
    val x = Digest32 @@ genBytesList(32)
    x
  }

  lazy val boxIdGen: Gen[BoxId] = {
    val x = ADKey @@ genBytesList(Constants.ModifierIdSize)
    x
  }

  lazy val stateRootGen: Gen[ADDigest] = {
    val x = ADDigest @@ genBytesList(Constants.ModifierIdSize + 1)
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

  def noProofInput(id: ErgoBox.BoxId): Input =
    Input(id, SerializedProverResult(Array.emptyByteArray, ContextExtension.empty))

  def outputForAnyone(value: Long): ErgoBoxCandidate = new ErgoBoxCandidate(value, TrueLeaf)

  def validTransactionsFromBoxHolder(boxHolder: BoxHolder): (Seq[ErgoTransaction], BoxHolder) =
    validTransactionsFromBoxHolder(boxHolder, new Random)

  def validTransactionsFromBoxHolder(boxHolder: BoxHolder, rnd: Random): (Seq[ErgoTransaction], BoxHolder) = {
    @tailrec
    def loop(txRemain: Int,
             stateBoxes: Seq[ErgoBox],
             selfBoxes: Seq[ErgoBox],
             acc: Seq[ErgoTransaction]): Seq[ErgoTransaction] = {
      if (txRemain > 1) {
        val (consumedBoxesFromState, remainedBoxes) = stateBoxes.splitAt(Try(rnd.nextInt(stateBoxes.size)).getOrElse(0))
        val (consumedSelfBoxes, remainedSelfBoxes) = selfBoxes.splitAt(Try(rnd.nextInt(selfBoxes.size)).getOrElse(0))
        val inputs = (consumedSelfBoxes ++ consumedBoxesFromState).map(_.id).map(noProofInput).toIndexedSeq
        val outputs = (consumedSelfBoxes ++ consumedBoxesFromState).map(_.value).map(outputForAnyone).toIndexedSeq
        val tx = new ErgoTransaction(inputs, outputs)
        loop(txRemain - 1, remainedBoxes, remainedSelfBoxes ++ tx.outputs, tx +: acc)
      } else {
        // take all remaining boxes from state and return transactions set
        val inputs = stateBoxes.map(_.id).map(noProofInput).toIndexedSeq
        val outputs = stateBoxes.map(_.value).map(outputForAnyone).toIndexedSeq
        val tx = new ErgoTransaction(inputs, outputs)
        tx +: acc
      }
    }

    val (boxes, bs) = boxHolder.take(rnd.nextInt(100) + 1)
    val txCount = rnd.nextInt(10) + 1
    loop(txCount, boxes, Seq.empty, Seq.empty) -> bs
  }


  def validTransactionsFromUtxoState(wus: WrappedUtxoState): Seq[ErgoTransaction] = {
    val num = 10

    val spentBoxesCounts = (1 to num).map(_ => scala.util.Random.nextInt(20) + 1)

    val boxes = wus.takeBoxes(spentBoxesCounts.sum)

    boxes.foreach(b => wus.boxById(b.id) should not be None)

    val (_, txs) = spentBoxesCounts.foldLeft(boxes -> Seq[ErgoTransaction]()) { case ((bxs, ts), fromBoxes) =>
      val (bxsFrom, remainder) = bxs.splitAt(fromBoxes)
      val inputs = bxsFrom.map(_.id).map(noProofInput).toIndexedSeq
      val outputs = bxsFrom.map(_.value).map(outputForAnyone).toIndexedSeq
      val tx = new ErgoTransaction(inputs, outputs)
      (remainder, tx +: ts)
    }
    txs
  }

  def validFullBlock(parentOpt: Option[Header], utxoState: UtxoState, boxHolder: BoxHolder): ErgoFullBlock =
    validFullBlock(parentOpt: Option[Header], utxoState: UtxoState, boxHolder: BoxHolder, new Random)


  def validFullBlock(parentOpt: Option[Header], utxoState: UtxoState, boxHolder: BoxHolder, rnd: Random): ErgoFullBlock = {
    val (transactions, _) = validTransactionsFromBoxHolder(boxHolder, rnd)
    validFullBlock(parentOpt, utxoState, transactions)
  }

  def validFullBlock(parentOpt: Option[Header],
                     utxoState: WrappedUtxoState): ErgoFullBlock = {
    val transactions = validTransactionsFromUtxoState(utxoState)
    transactions.flatMap(_.inputs.map(_.boxId)).foreach(bid => utxoState.boxById(bid) should not be None)
    validFullBlock(parentOpt, utxoState, transactions)
  }

  def validFullBlock(parentOpt: Option[Header],
                     utxoState: UtxoState,
                     transactions: Seq[ErgoTransaction],
                     n: Char = 48,
                     k: Char = 5
                    ): ErgoFullBlock = {

    val (adProofBytes, updStateDigest) = utxoState.proofsForTransactions(transactions).get

    val time = System.currentTimeMillis()

    DefaultFakePowScheme.proveBlock(parentOpt, Constants.InitialNBits, updStateDigest, adProofBytes,
      transactions, time, digest32Gen.sample.get).get
  }

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
    stateElements: Seq[ErgoBox] <- Gen.listOf(ergoBoxGen)
  } yield UTXOSnapshotChunk(stateElements, index)

  lazy val invalidErgoFullBlockGen: Gen[ErgoFullBlock] = for {
    header <- invalidHeaderGen
    txs <- invalidBlockTransactionsGen
    proof <- randomADProofsGen
  } yield ErgoFullBlock(header, txs, Some(proof))

  lazy val emptyMemPoolGen: Gen[ErgoMemPool] =
    Gen.resultOf({ _: Unit => ErgoMemPool.empty })(Arbitrary(Gen.const(())))
}
