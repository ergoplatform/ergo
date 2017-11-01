package org.ergoplatform.utils

import org.ergoplatform.modifiers.ErgoFullBlock
import org.ergoplatform.modifiers.history.{ADProofs, BlockTransactions, DefaultFakePowScheme, Header}
import org.ergoplatform.modifiers.mempool.{AnyoneCanSpendTransaction, TransactionIdsForHeader}
import org.ergoplatform.modifiers.mempool.proposition.{AnyoneCanSpendNoncedBox, AnyoneCanSpendProposition}
import org.ergoplatform.modifiers.state.UTXOSnapshotChunk
import org.ergoplatform.nodeView.WrappedUtxoState
import org.ergoplatform.nodeView.history.ErgoSyncInfo
import org.ergoplatform.nodeView.state.{BoxHolder, UtxoState}
import org.ergoplatform.settings.Constants
import org.scalacheck.{Arbitrary, Gen}
import scorex.core.ModifierId
import scorex.core.transaction.state.{BoxStateChanges, Insertion}
import scorex.crypto.authds.{ADDigest, SerializedAdProof}
import scorex.crypto.hash.Digest32
import scorex.testkit.generators.CoreGenerators

trait ErgoGenerators extends CoreGenerators {

  lazy val anyoneCanSpendProposition: Gen[AnyoneCanSpendProposition.M] = Gen.const(AnyoneCanSpendProposition)

  lazy val invalidAnyoneCanSpendTransactionGen: Gen[AnyoneCanSpendTransaction] = for {
    from: IndexedSeq[Long] <- smallInt.flatMap(i => Gen.listOfN(i + 1, positiveLongGen).map(_.toIndexedSeq))
    to: IndexedSeq[Long] <- smallInt.flatMap(i => Gen.listOfN(i, positiveLongGen).map(_.toIndexedSeq))
  } yield AnyoneCanSpendTransaction(from, to)

  lazy val positiveIntGen: Gen[Int] = Gen.choose(1, Int.MaxValue)

  lazy val anyoneCanSpendBoxGen: Gen[AnyoneCanSpendNoncedBox] = for {
    nonce <- positiveLongGen
    value <- positiveIntGen
  } yield AnyoneCanSpendNoncedBox(nonce, value)

  lazy val boxesHolderGen: Gen[BoxHolder] = Gen.listOfN(2000, anyoneCanSpendBoxGen).map(l => BoxHolder(l))

  lazy val stateChangesGen: Gen[BoxStateChanges[AnyoneCanSpendProposition.type, AnyoneCanSpendNoncedBox]] = anyoneCanSpendBoxGen
    .map(b => BoxStateChanges[AnyoneCanSpendProposition.type, AnyoneCanSpendNoncedBox](Seq(Insertion(b))))

  lazy val ergoSyncInfoGen: Gen[ErgoSyncInfo] = for {
    answer <- Arbitrary.arbitrary[Boolean]
    ids <- Gen.nonEmptyListOf(modifierIdGen).map(_.take(ErgoSyncInfo.MaxBlockIds))
    fullBlockOpt <- Gen.option(modifierIdGen)
  } yield ErgoSyncInfo(answer, ids, fullBlockOpt)

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
    nonce <- Arbitrary.arbitrary[Int]
    requiredDifficulty <- Arbitrary.arbitrary[Int]
    height <- Gen.choose(1, Int.MaxValue)
    equihashSolutions <- genBytesList(Constants.ModifierIdSize)
    height <- Gen.choose(1, Int.MaxValue)
    interlinks <- Gen.nonEmptyListOf(modifierIdGen).map(_.take(128))
    timestamp <- positiveLongGen
    votes <- genBytesList(5)
  } yield Header(version, parentId, interlinks, adRoot, stateRoot, transactionsRoot, timestamp, requiredDifficulty, height, votes, nonce,
    equihashSolutions)


  def validTransactionsFromBoxHolder(boxHolder: BoxHolder): (Seq[AnyoneCanSpendTransaction], BoxHolder) = {
    val num = 10

    val spentBoxesCounts = (1 to num).map(_ => scala.util.Random.nextInt(10) + 1)

    val (boxes, bs) = boxHolder.take(spentBoxesCounts.sum)

    val (_, txs) = spentBoxesCounts.foldLeft((boxes, Seq[AnyoneCanSpendTransaction]())) { case ((bxs, ts), fromBoxes) =>
      val (bxsFrom, remainder) = bxs.splitAt(fromBoxes)
      val newBoxes = bxsFrom.map(_.value)
      val tx = new AnyoneCanSpendTransaction(bxsFrom.map(_.nonce).toIndexedSeq, newBoxes.toIndexedSeq)
      (remainder, tx +: ts)
    }
    txs -> bs
  }


  def validTransactionsFromUtxoState(wus: WrappedUtxoState): Seq[AnyoneCanSpendTransaction] = {
    val num = 10

    val spentBoxesCounts = (1 to num).map(_ => scala.util.Random.nextInt(20) + 1)

    val boxes = wus.takeBoxes(spentBoxesCounts.sum)

      boxes.foreach { b =>
        require(wus.boxById(b.id).isDefined)
      }


    val (_, txs) = spentBoxesCounts.foldLeft(boxes -> Seq[AnyoneCanSpendTransaction]()) { case ((bxs, ts), fromBoxes) =>
      val (bxsFrom, remainder) = bxs.splitAt(fromBoxes)
      val spentBoxNonces = bxsFrom.map(_.nonce).toIndexedSeq
      val newBoxes = bxsFrom.map(_.value).toIndexedSeq
      val tx = new AnyoneCanSpendTransaction(spentBoxNonces, newBoxes)
      (remainder, tx +: ts)
    }
    txs
  }


  def validFullBlock(parentOpt: Option[Header], utxoState: UtxoState, boxHolder: BoxHolder): ErgoFullBlock = {
    val (transactions, _) = validTransactionsFromBoxHolder(boxHolder)
    validFullBlock(parentOpt, utxoState, transactions)
  }

  def validFullBlock(parentOpt: Option[Header],
                     utxoState: WrappedUtxoState): ErgoFullBlock = {
    val transactions = validTransactionsFromUtxoState(utxoState)
    transactions.flatMap(_.boxIdsToOpen).foreach { bid =>
      require(utxoState.boxById(bid).isDefined)
    }
    validFullBlock(parentOpt, utxoState, transactions)
  }

  def validFullBlock(parentOpt: Option[Header],
                     utxoState: UtxoState,
                     transactions: Seq[AnyoneCanSpendTransaction],
                     n: Char = 48,
                     k: Char = 5
                    ): ErgoFullBlock = {

    val (adProofBytes, updStateDigest) = utxoState.proofsForTransactions(transactions).get

    val time = System.currentTimeMillis()

    DefaultFakePowScheme.proveBlock(parentOpt, Constants.InitialNBits, updStateDigest, adProofBytes,
      transactions, time, Array.fill(5)(0.toByte))
  }

  lazy val invalidBlockTransactionsGen: Gen[BlockTransactions] = for {
    headerId <- modifierIdGen
    txs <- Gen.nonEmptyListOf(invalidAnyoneCanSpendTransactionGen)
  } yield BlockTransactions(headerId, txs)

  lazy val randomADProofsGen: Gen[ADProofs] = for {
    headerId <- modifierIdGen
    proof <- serializedAdProofGen
  } yield ADProofs(headerId, proof)

  lazy val randomUTXOSnapshotChunkGen: Gen[UTXOSnapshotChunk] = for {
    index: Short <- Arbitrary.arbitrary[Short]
    stateElements: Seq[AnyoneCanSpendNoncedBox] <- Gen.listOf(anyoneCanSpendBoxGen)
  } yield UTXOSnapshotChunk(stateElements, index)

  lazy val invalidErgoFullBlockGen: Gen[ErgoFullBlock] = for {
    header <- invalidHeaderGen
    txs <- invalidBlockTransactionsGen
    proof <- randomADProofsGen
  } yield ErgoFullBlock(header, txs, Some(proof))
}
