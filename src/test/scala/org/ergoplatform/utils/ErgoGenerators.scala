package org.ergoplatform.utils

import org.ergoplatform.modifiers.ErgoFullBlock
import org.ergoplatform.modifiers.history.{ADProof, BlockTransactions, Header}
import org.ergoplatform.modifiers.mempool.AnyoneCanSpendTransaction
import org.ergoplatform.modifiers.mempool.proposition.{AnyoneCanSpendNoncedBox, AnyoneCanSpendProposition}
import org.ergoplatform.nodeView.history.ErgoSyncInfo
import org.ergoplatform.settings.Constants
import org.scalacheck.{Arbitrary, Gen}
import scorex.core.transaction.state.{BoxStateChanges, Insertion}
import scorex.testkit.CoreGenerators

import scala.collection.mutable

class BoxesStorage(initialBoxes: Seq[AnyoneCanSpendNoncedBox]) {
  private val boxes = mutable.Map[Array[Byte], AnyoneCanSpendNoncedBox](initialBoxes.map(b => b.id -> b): _*)

  def removeBoxes(ids: Seq[Array[Byte]]): Unit = {
    ids.foreach(id => boxes.remove(id))
  }

  def addBoxes(bs: Seq[AnyoneCanSpendNoncedBox]): Unit = {
    bs.foreach(bs => boxes.put(bs.id, bs))
  }

  def take(howMany: Int): Seq[AnyoneCanSpendNoncedBox] = boxes.take(howMany).values.toSeq
}

object BoxesStorage {
  val howMany = 1000000

  /*todo: impl
  def generate(): BoxesStorage = {
    (1 to howMany) map {_ =>
      AnyoneCanSpendNoncedBox()
    }
  }*/
}

trait ErgoGenerators extends CoreGenerators {

  val anyoneCanSpendProposition = AnyoneCanSpendProposition

  lazy val pGen: Gen[(AnyoneCanSpendProposition.type, Long)] = for {
    long <- positiveLongGen
  } yield (anyoneCanSpendProposition, long)

  lazy val invalidAnyoneCanSpendTransactionGen: Gen[AnyoneCanSpendTransaction] = for {
    from: IndexedSeq[Long] <- smallInt.flatMap(i => Gen.listOfN(i + 1, positiveLongGen).map(_.toIndexedSeq))
    to: IndexedSeq[Long]   <- smallInt.flatMap(i => Gen.listOfN(i, positiveLongGen).map(_.toIndexedSeq))
  } yield AnyoneCanSpendTransaction(from, to)

  lazy val anyoneCanSpendBoxGen: Gen[AnyoneCanSpendNoncedBox] = for {
    nonce <- positiveLongGen
    value <- positiveLongGen
  } yield AnyoneCanSpendNoncedBox(nonce, value)

  lazy val stateChangesGen: Gen[BoxStateChanges[AnyoneCanSpendProposition.type, AnyoneCanSpendNoncedBox]] = anyoneCanSpendBoxGen
    .map(b => BoxStateChanges[AnyoneCanSpendProposition.type, AnyoneCanSpendNoncedBox](Seq(Insertion(b))))

  lazy val ergoSyncInfoGen: Gen[ErgoSyncInfo] = for {
    answer <- Arbitrary.arbitrary[Boolean]
    idGenerator <- genBytesList(Constants.ModifierIdSize)
    ids <- Gen.nonEmptyListOf(idGenerator).map(_.take(ErgoSyncInfo.MaxBlockIds))
    fullBlockOpt <- Gen.option(idGenerator)
  } yield ErgoSyncInfo(answer, ids, fullBlockOpt)

  lazy val invalidHeaderGen: Gen[Header] = for {
    version <- Arbitrary.arbitrary[Byte]
    parentId <- genBytesList(Constants.ModifierIdSize)
    stateRoot <- genBytesList(Constants.ModifierIdSize)
    adRoot <- genBytesList(Constants.ModifierIdSize)
    transactionsRoot <- genBytesList(Constants.ModifierIdSize)
    nonce <- Arbitrary.arbitrary[Int]
    requiredDifficulty <- Arbitrary.arbitrary[Int]
    interlinks <- Gen.nonEmptyListOf(genBytesList(Constants.ModifierIdSize)).map(_.take(128))
    timestamp <- positiveLongGen
    extensionHash <- genBytesList(Constants.ModifierIdSize)
    votes <- genBytesList(5)
  } yield Header(version, parentId, interlinks, adRoot, stateRoot, transactionsRoot, timestamp, nonce,
    requiredDifficulty, extensionHash, votes)

/*
todo: finish

  def validHeaderGen(parent: Header): Gen[Header] = {
    val parentId = parent.id
    for {
      version <- Arbitrary.arbitrary[Byte]
      stateRoot <- ???
      adRoot <- ???
      transactionsRoot <- genBytesList(Constants.ModifierIdSize)
      nonce <- Arbitrary.arbitrary[Int]
      interlinks <- Gen.nonEmptyListOf(genBytesList(Constants.ModifierIdSize)).map(_.take(128))
      timestamp <- positiveLongGen
      extensionHash <- genBytesList(Constants.ModifierIdSize)
      votes <- genBytesList(5)
    } yield Header(version, parentId, interlinks, adRoot, stateRoot, transactionsRoot, timestamp, nonce, extensionHash, votes)
  }*/

  lazy val invalidBlockTransactionsGen: Gen[BlockTransactions] = for {
    headerId <- genBytesList(Constants.ModifierIdSize)
    txs <- Gen.nonEmptyListOf(invalidAnyoneCanSpendTransactionGen)
  } yield BlockTransactions(headerId, txs)

  def validTransactionsGen(boxesStorage: BoxesStorage): Gen[AnyoneCanSpendTransaction] = {
    val num = 100

    val spentBoxesCounts = (1 to num).map(_ => scala.util.Random.nextInt(10) + 1)

    for {
      from: IndexedSeq[Long] <- smallInt.flatMap(i => Gen.listOfN(i + 1, positiveLongGen).map(_.toIndexedSeq))
      to: IndexedSeq[Long] <- smallInt.flatMap(i => Gen.listOfN(i, positiveLongGen).map(_.toIndexedSeq))
    } yield AnyoneCanSpendTransaction(from, to)
  }

  lazy val randomADProofsGen: Gen[ADProof] = for {
    headerId <- genBytesList(Constants.ModifierIdSize)
    proof <- genBoundedBytes(32, 32 * 1024)
  } yield ADProof(headerId, proof)

  lazy val invalidErgoFullBlockGen: Gen[ErgoFullBlock] = for {
    header <- invalidHeaderGen
    txs <- invalidBlockTransactionsGen
    proof <- randomADProofsGen
  } yield ErgoFullBlock(header, txs, Some(proof), None)
}
