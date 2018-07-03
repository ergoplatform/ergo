package org.ergoplatform.utils

import io.iohk.iodb.ByteArrayWrapper
import org.ergoplatform.ErgoBox.{NonMandatoryRegisterId, R4, TokenId}
import org.ergoplatform.{ErgoBox, ErgoBoxCandidate, Input}
import org.ergoplatform.modifiers.ErgoFullBlock
import org.ergoplatform.modifiers.history.BlockTransactions
import org.ergoplatform.modifiers.mempool.ErgoTransaction
import org.ergoplatform.modifiers.state.{Insertion, StateChanges, UTXOSnapshotChunk}
import org.ergoplatform.nodeView.state.BoxHolder
import org.ergoplatform.settings.Constants
import org.scalacheck.Arbitrary.arbByte
import org.scalacheck.{Arbitrary, Gen}
import scorex.crypto.hash.{Blake2b256, Digest32}
import sigmastate.{NoProof, SByte, SType, SigSerializer}
import sigmastate.Values.{ByteArrayConstant, CollectionConstant, EvaluatedValue, FalseLeaf, IntConstant, TrueLeaf}
import sigmastate.interpreter.{ContextExtension, SerializedProverResult}

import scala.collection.mutable
import scala.util.Random

import scala.collection.JavaConverters._
import scala.collection.mutable
import scala.util.Random

trait ErgoTransactionGenerators extends ErgoGenerators {

  lazy val ergoBoxGen: Gen[ErgoBox] = for {
    prop <- ergoPropositionGen
    value <- positiveIntGen
    reg <- positiveIntGen
    transactionId: Array[Byte] <- genBytes(Constants.ModifierIdSize)
    boxId: Short <- Arbitrary.arbitrary[Short]
  } yield ErgoBox(value, prop, Seq(), Map(R4 -> IntConstant(reg)), transactionId, boxId)

  val byteArrayConstGen: Gen[CollectionConstant[SByte.type]] = for {
    length <- Gen.chooseNum(1, 100)
    bytes <- Gen.listOfN(length, arbByte.arbitrary)
  } yield ByteArrayConstant(bytes.toArray)

  def additionalRegistersGen(cnt: Byte): Seq[Gen[(NonMandatoryRegisterId, EvaluatedValue[SType])]] = {
    (0 until cnt)
      .map(_ + ErgoBox.startingNonMandatoryIndex)
      .map(rI => ErgoBox.registerByIndex(rI.toByte).asInstanceOf[NonMandatoryRegisterId])
      .map { r =>
        for {
          arr <- byteArrayConstGen
          v <- Gen.oneOf(TrueLeaf, FalseLeaf, arr)
        } yield r -> v.asInstanceOf[EvaluatedValue[SType]]
      }
  }

  def additionalTokensGen(cnt: Byte): Seq[Gen[(TokenId, Long)]] =
    (0 until cnt).map { _ =>
      for {
        id <- Digest32 @@ boxIdGen
        amt <- Gen.oneOf(1, 500, 20000, 10000000, Long.MaxValue)
      } yield id -> amt
    }

  lazy val ergoBoxGenNoProp: Gen[ErgoBox] = for {
    prop <- trueLeafGen
    value <- positiveIntGen
    transactionId: Array[Byte] <- genBytes(Constants.ModifierIdSize)
    boxId: Short <- Arbitrary.arbitrary[Short]
    regNum <- Gen.chooseNum[Byte](0, ErgoBox.nonMandatoryRegistersCount)
    ar <- Gen.sequence(additionalRegistersGen(regNum))
    tokensCount <- Gen.chooseNum[Byte](0, ErgoBox.MaxTokens)
    tokens <- Gen.sequence(additionalTokensGen(tokensCount))
  } yield ErgoBox(value, prop, tokens.asScala, ar.asScala.toMap, transactionId, boxId)

  def ergoBoxGenForTokens(tokens: Seq[(TokenId, Long)]): Gen[ErgoBox] = for {
    prop <- trueLeafGen
    value <- positiveIntGen
    transactionId: Array[Byte] <- genBytes(Constants.ModifierIdSize)
    boxId: Short <- Arbitrary.arbitrary[Short]
    regNum <- Gen.chooseNum[Byte](0, ErgoBox.nonMandatoryRegistersCount)
    ar <- Gen.sequence(additionalRegistersGen(regNum))
  } yield ErgoBox(value, prop, tokens, ar.asScala.toMap, transactionId, boxId)

  lazy val ergoBoxCandidateGen: Gen[ErgoBoxCandidate] = for {
    prop <- trueLeafGen
    value <- positiveIntGen
    regNum <- Gen.chooseNum[Byte](0, ErgoBox.nonMandatoryRegistersCount)
    ar <- Gen.sequence(additionalRegistersGen(regNum))
    tokensCount <- Gen.chooseNum[Byte](0, ErgoBox.MaxTokens)
    tokens <- Gen.sequence(additionalTokensGen(tokensCount))
  } yield new ErgoBoxCandidate(value, prop, tokens.asScala, ar.asScala.toMap)

  lazy val inputGen: Gen[Input] = for {
    boxId <- boxIdGen
    spendingProof <- noProofGen
  } yield Input(boxId, spendingProof)

  lazy val invalidErgoTransactionGen: Gen[ErgoTransaction] = for {
    from: IndexedSeq[Input] <- smallInt.flatMap(i => Gen.listOfN(i + 1, inputGen).map(_.toIndexedSeq))
    to: IndexedSeq[ErgoBoxCandidate] <- smallInt.flatMap(i => Gen.listOfN(i, ergoBoxCandidateGen).map(_.toIndexedSeq))
  } yield ErgoTransaction(from, to)


  def validTransactionGen(boxesToSpend: Seq[ErgoBox]): Gen[ErgoTransaction] = {
    val inputSum = boxesToSpend.map(_.value).sum
    val assetsMap: mutable.Map[ByteArrayWrapper, Long] =
      mutable.Map(boxesToSpend.flatMap(_.additionalTokens).map { case (bs, amt) =>
        ByteArrayWrapper(bs) -> amt
      }: _*)

    //randomly creating a new asset
    if (Random.nextBoolean()) {
      assetsMap.put(ByteArrayWrapper(boxesToSpend.head.id), Random.nextInt(Int.MaxValue))
        .ensuring(assetsMap.size <= ErgoTransaction.MaxTokens)
    }

    lazy val tokensSheetIn = assetsMap.toMap

    val inputsCount = boxesToSpend.size
    val outputsCount = Math.min(Short.MaxValue, Math.max(inputsCount + 1, Random.nextInt(inputsCount * 2)))

    val outputAmounts = (1 to outputsCount).foldLeft(Seq[Long]() -> inputSum) { case ((amounts, remainder), idx) =>
      val amount = if (idx == outputsCount) {
        remainder
      } else {
        Random.nextInt((inputSum / inputsCount).toInt) + 1
      }
      (amounts :+ amount) -> (remainder - amount)
    }._1.toIndexedSeq

    val tokenAmounts: mutable.IndexedSeq[mutable.Map[ByteArrayWrapper, Long]] =
      mutable.IndexedSeq.fill(outputsCount)(mutable.Map[ByteArrayWrapper, Long]())

    var availableTokenSlots = (outputsCount * ErgoBox.MaxTokens).ensuring(_ >= assetsMap.size)

    if (assetsMap.nonEmpty) {
      do {
        val in = assetsMap.head
        val outIdx = Stream.from(1, 1).map(_ => Random.nextInt(tokenAmounts.size))
          .find(idx => tokenAmounts(idx).size < ErgoBox.MaxTokens).get
        val out = tokenAmounts(outIdx)
        val contains = out.contains(in._1)

        val amt = if (in._2 == 1 || (availableTokenSlots < assetsMap.size * 2 && !contains) || Random.nextBoolean()) {
          in._2
        } else {
          Math.max(1, Math.min((Random.nextDouble() * in._2).toLong, in._2))
        }

        if (amt == in._2) assetsMap.remove(in._1) else assetsMap.update(in._1, in._2 - amt)
        val updOut = if (contains) {
          val outAmt = out(in._1)
          out.update(in._1, outAmt + amt)
        } else {
          availableTokenSlots = availableTokenSlots - 1
          out.update(in._1, amt)
        }
        tokenAmounts(outIdx) = out
      } while (assetsMap.nonEmpty)
    }

    lazy val tokensSheetOut = tokenAmounts.flatMap(_.toSeq).groupBy(_._1).mapValues(_.map(_._2).sum)

    //println("intokens: " + tokensSheetOut)
    //println("outtokens: " + tokensSheetOut)

    val newBoxes = outputAmounts.zip(tokenAmounts.toIndexedSeq).map { case (amt, tokens) =>
      val normalizedTokens = tokens.toSeq.map(t => (Digest32 @@ t._1.data) -> t._2)
      ErgoBox(amt, TrueLeaf, normalizedTokens)
    }

    val noProof = SerializedProverResult(SigSerializer.toBytes(NoProof), ContextExtension.empty)
    val inputs = boxesToSpend.map(box => Input(box.id, noProof)).toIndexedSeq
    ErgoTransaction(inputs, newBoxes)
  }

  def disperseTokens(inputsCount: Int, tokensCount: Byte): Gen[IndexedSeq[Seq[(TokenId, Long)]]]  = {
    val tokenDistrib = mutable.IndexedSeq.fill(inputsCount)(Seq[(TokenId, Long)]())
    (1 to tokensCount).foreach { i =>
      val (id, amt) = Blake2b256(s"$i" + Random.nextString(5)) -> Random.nextInt(Int.MaxValue).toLong
      val idx = i % tokenDistrib.size
      val s = tokenDistrib(idx)
      tokenDistrib(idx) = s :+ (id, amt)
    }
    tokenDistrib
  }

  def validErgoTransactionGenTemplate(minAssets: Int): Gen[(IndexedSeq[ErgoBox], ErgoTransaction)] = for {
    inputsCount <- Gen.choose(1, 100)
    tokensCount <- Gen.choose(minAssets, Math.min(inputsCount * ErgoBox.MaxTokens, ErgoTransaction.MaxTokens - 1))
    tokensDistribution <- disperseTokens(inputsCount, tokensCount.toByte)
    from <- Gen.sequence(tokensDistribution.map(ergoBoxGenForTokens))
    tx <- validTransactionGen(from.asScala.toIndexedSeq)
  } yield from.asScala.toIndexedSeq -> tx

  lazy val validErgoTransactionGen = validErgoTransactionGenTemplate(0)
  lazy val validErgoTransactionWithAssetsGen = validErgoTransactionGenTemplate(1)

  lazy val boxesHolderGen: Gen[BoxHolder] = Gen.listOfN(2000, ergoBoxGenNoProp).map(l => BoxHolder(l))

  lazy val stateChangesGen: Gen[StateChanges] = ergoBoxGenNoProp
    .map(b => StateChanges(Seq(), Seq(Insertion(b))))

  lazy val invalidBlockTransactionsGen: Gen[BlockTransactions] = for {
    headerId <- modifierIdGen
    txs <- Gen.nonEmptyListOf(invalidErgoTransactionGen)
  } yield BlockTransactions(headerId, txs)

  lazy val randomUTXOSnapshotChunkGen: Gen[UTXOSnapshotChunk] = for {
    index: Short <- Arbitrary.arbitrary[Short]
    stateElements: Seq[ErgoBox] <- Gen.listOf(ergoBoxGenNoProp)
  } yield UTXOSnapshotChunk(stateElements, index)

  lazy val invalidErgoFullBlockGen: Gen[ErgoFullBlock] = for {
    header <- invalidHeaderGen
    txs <- invalidBlockTransactionsGen
    proof <- randomADProofsGen
  } yield ErgoFullBlock(header, txs, Some(proof))
}
