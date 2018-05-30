package org.ergoplatform.utils

import io.iohk.iodb.ByteArrayWrapper
import org.ergoplatform.ErgoBox.{BoxId, R3}
import org.ergoplatform.local.ErgoMiner
import org.ergoplatform.mining.difficulty.RequiredDifficulty
import org.ergoplatform.mining.emission.CoinsEmission
import org.ergoplatform.mining.{DefaultFakePowScheme, EquihashSolution}
import org.ergoplatform.modifiers.ErgoFullBlock
import org.ergoplatform.modifiers.history.{ADProofs, BlockTransactions, Header}
import org.ergoplatform.modifiers.mempool.{ErgoTransaction, TransactionIdsForHeader}
import org.ergoplatform.modifiers.state.{Insertion, Removal, StateChanges, UTXOSnapshotChunk}
import org.ergoplatform.nodeView.WrappedUtxoState
import org.ergoplatform.nodeView.history.ErgoSyncInfo
import org.ergoplatform.nodeView.mempool.ErgoMemPool
import org.ergoplatform.nodeView.state.{BoxHolder, ErgoState, UtxoState}
import org.ergoplatform.settings.{Algos, Constants}
import org.ergoplatform.{ErgoBox, ErgoBoxCandidate, Input}
import org.scalacheck.{Arbitrary, Gen}
import org.scalatest.Matchers
import scorex.core.ModifierId
import scorex.crypto.authds.{ADDigest, ADKey, SerializedAdProof}
import scorex.crypto.hash.Digest32
import scorex.testkit.generators.CoreGenerators
import sigmastate.SBoolean
import sigmastate.Values.{IntConstant, LongConstant, TrueLeaf, Value}
import sigmastate.interpreter.{ContextExtension, SerializedProverResult}

import scala.annotation.tailrec
import scala.util.{Random, Try}

trait ErgoGenerators extends CoreGenerators with Matchers {

  val emission: CoinsEmission = new CoinsEmission()

  lazy val trueLeafGen: Gen[Value[SBoolean.type]] = Gen.const(TrueLeaf)
  lazy val smallPositiveInt: Gen[Int] = Gen.choose(1, 5)

  lazy val noProofGen: Gen[SerializedProverResult] =
    Gen.const(SerializedProverResult(Array.emptyByteArray, ContextExtension(Map())))

  lazy val ergoBoxGenNoProp: Gen[ErgoBox] = for {
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


  lazy val boxesHolderGen: Gen[BoxHolder] = Gen.listOfN(2000, ergoBoxGenNoProp).map(l => BoxHolder(l))

  lazy val stateChangesGen: Gen[StateChanges] = ergoBoxGenNoProp
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

  @tailrec
  private def validTransactionsFromBoxes(txRemain: Int,
                                         stateBoxes: Seq[ErgoBox],
                                         selfBoxes: Seq[ErgoBox],
                                         acc: Seq[ErgoTransaction],
                                         rnd: Random): (Seq[ErgoTransaction], Seq[ErgoBox]) = {
    def genOuts(remainingAmount: Long,
                acc: IndexedSeq[ErgoBoxCandidate],
                limit: Int): IndexedSeq[ErgoBoxCandidate] = {
      val newAmount = rnd.nextLong() % (remainingAmount + acc.map(_.value).sum)
      if (newAmount >= remainingAmount || limit <= 1) {
        acc :+ outputForAnyone(remainingAmount)
      } else {
        genOuts(remainingAmount - newAmount, acc :+ outputForAnyone(newAmount), limit - 1)
      }
    }

    stateBoxes.find(_ == ErgoState.genesisEmissionBox) match {
      case Some(emissionBox) if txRemain > 0 =>
        // Extract money to anyoneCanSpend output and forget about emission box for tests
        val tx = ErgoMiner.createCoinbase(0, Seq.empty, emissionBox, TrueLeaf, emission)
        val remainedBoxes = stateBoxes.filter(_ != ErgoState.genesisEmissionBox)
        val newSelfBoxes = selfBoxes ++ tx.outputs.filter(_.proposition == TrueLeaf)
        validTransactionsFromBoxes(txRemain - 1, remainedBoxes, newSelfBoxes, tx +: acc, rnd)

      case _ =>
        if (txRemain > 1) {
          val (consumedSelfBoxes, remainedSelfBoxes) = selfBoxes.splitAt(Try(rnd.nextInt(selfBoxes.size) + 1).getOrElse(0))
          val (consumedBoxesFromState, remainedBoxes) = stateBoxes.splitAt(Try(rnd.nextInt(stateBoxes.size) + 1).getOrElse(0))
          val inputs = (consumedSelfBoxes ++ consumedBoxesFromState).map(_.id).map(noProofInput).toIndexedSeq
          assert(inputs.nonEmpty, "Trying to create transaction with no inputs")
          val totalAmount = (consumedSelfBoxes ++ consumedBoxesFromState).map(_.value).sum
          val outputs = genOuts(totalAmount, IndexedSeq.empty, rnd.nextInt(10) + 1)
          val tx = new ErgoTransaction(inputs, outputs)
          validTransactionsFromBoxes(txRemain - 1, remainedBoxes, remainedSelfBoxes ++ tx.outputs, tx +: acc, rnd)
        } else {
          // take all remaining boxes from state and return transactions set
          val (consumedSelfBoxes, remainedSelfBoxes) = selfBoxes.splitAt(1)
          val inputs = (consumedSelfBoxes ++ stateBoxes).map(_.id).map(noProofInput).toIndexedSeq
          assert(inputs.nonEmpty, "Trying to create transaction with no inputs")
          val totalAmount = (consumedSelfBoxes ++ stateBoxes).map(_.value).sum
          val outputs = genOuts(totalAmount, IndexedSeq.empty, rnd.nextInt(10) + 1)
          val tx = new ErgoTransaction(inputs, outputs)
          ((tx +: acc).reverse, remainedSelfBoxes ++ tx.outputs)
        }
    }
  }

  def validTransactionsFromBoxHolder(boxHolder: BoxHolder, rnd: Random): (Seq[ErgoTransaction], BoxHolder) = {
    val (boxes, drainedBh) = boxHolder.take(rnd.nextInt(100) + 1)
    assert(boxes.nonEmpty, s"Was unable to take at least 1 box from box holder $boxHolder")
    val (txs, createdBoxes) = validTransactionsFromBoxes(rnd.nextInt(10) + 1, boxes, Seq.empty, Seq.empty, rnd)
    txs.foreach(_.statelessValidity.get)
    val bs = new BoxHolder(drainedBh.boxes ++ createdBoxes.map(b => ByteArrayWrapper(b.id) -> b))
    txs -> bs
  }


  def validTransactionsFromUtxoState(wus: WrappedUtxoState, rnd: Random = new Random): Seq[ErgoTransaction] = {
    val num = 1 + rnd.nextInt(10)

    val allBoxes = wus.takeBoxes(num + rnd.nextInt(100))
    val anyoneCanSpendBoxes = allBoxes.filter(_.proposition == TrueLeaf)
    val boxes = if (anyoneCanSpendBoxes.nonEmpty) anyoneCanSpendBoxes else allBoxes

    validTransactionsFromBoxes(num, boxes, Seq.empty, Seq.empty, rnd)._1
  }

  def validFullBlock(parentOpt: Option[Header], utxoState: UtxoState, boxHolder: BoxHolder): ErgoFullBlock =
    validFullBlock(parentOpt: Option[Header], utxoState: UtxoState, boxHolder: BoxHolder, new Random)


  def validFullBlock(parentOpt: Option[Header], utxoState: UtxoState, boxHolder: BoxHolder, rnd: Random): ErgoFullBlock = {
    validFullBlock(parentOpt, utxoState, validTransactionsFromBoxHolder(boxHolder, rnd)._1)
  }

  def validFullBlock(parentOpt: Option[Header],
                     utxoState: WrappedUtxoState): ErgoFullBlock = {
    validFullBlock(parentOpt, utxoState, validTransactionsFromUtxoState(utxoState))
  }

  def validFullBlock(parentOpt: Option[Header],
                     utxoState: UtxoState,
                     transactions: Seq[ErgoTransaction],
                     n: Char = 48,
                     k: Char = 5
                    ): ErgoFullBlock = {
    transactions.foreach(_.statelessValidity shouldBe 'success)
    transactions.nonEmpty shouldBe true
    ErgoState.boxChanges(transactions).operations.foreach {
      case Removal(boxId: ADKey) => assert(utxoState.boxById(boxId).isDefined, s"Box ${Algos.encode(boxId)} missed")
      case _ =>
    }

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
