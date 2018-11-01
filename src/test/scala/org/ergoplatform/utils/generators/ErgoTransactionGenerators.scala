package org.ergoplatform.utils.generators

import io.iohk.iodb.ByteArrayWrapper
import org.ergoplatform.ErgoBox.{NonMandatoryRegisterId, R4, TokenId}
import org.ergoplatform.modifiers.ErgoFullBlock
import org.ergoplatform.modifiers.history.BlockTransactions
import org.ergoplatform.modifiers.mempool.{ErgoTransaction, UnsignedErgoTransaction}
import org.ergoplatform.modifiers.state.{Insertion, StateChanges, UTXOSnapshotChunk}
import org.ergoplatform.nodeView.state.BoxHolder
import org.ergoplatform.settings.Constants
import org.ergoplatform.{ErgoBox, ErgoBoxCandidate, Input}
import org.scalacheck.Arbitrary.arbByte
import org.scalacheck.{Arbitrary, Gen}
import scorex.crypto.hash.{Blake2b256, Digest32}
import scorex.util._
import sigmastate.Values.{ByteArrayConstant, CollectionConstant, EvaluatedValue, FalseLeaf, IntConstant, TrueLeaf, Value}
import sigmastate._
import sigmastate.interpreter.{ContextExtension, ProverResult}

import scala.annotation.tailrec
import scala.collection.JavaConverters._
import scala.collection.mutable
import scala.util.Random


trait ErgoTransactionGenerators extends ErgoGenerators {

  val creationHeightGen: Gen[Int] = Gen.choose(0, Int.MaxValue)

  val boxIndexGen: Gen[Short] = for {
    v <- Gen.chooseNum(0, Short.MaxValue)
  } yield v.toShort

  lazy val ergoBoxCandidateGen: Gen[ErgoBoxCandidate] = for {
    h <- creationHeightGen
    prop <- trueLeafGen
    ar <- additionalRegistersGen
    tokens <- additionalTokensGen
    value <- validValueGen(prop, tokens, ar)
  } yield new ErgoBoxCandidate(value, prop, tokens, ar, creationHeight = h)

  def ergoBoxGen(propGen: Gen[Value[SBoolean.type]] = ergoPropositionGen,
                 tokensGen: Gen[Seq[(TokenId, Long)]] = additionalTokensGen,
                 valueGenOpt: Option[Gen[Long]] = None): Gen[ErgoBox] = for {
    h <- creationHeightGen
    prop <- propGen
    transactionId: Array[Byte] <- genBytes(Constants.ModifierIdSize)
    boxId: Short <- boxIndexGen
    ar <- additionalRegistersGen
    tokens <- tokensGen
    value <- valueGenOpt.getOrElse(validValueGen(prop, tokens, ar, transactionId.toModifierId, boxId))
  } yield ErgoBox(value, prop, tokens, ar, transactionId.toModifierId, boxId, h)

  lazy val ergoBoxGen: Gen[ErgoBox] = ergoBoxGen()

  lazy val ergoBoxGenNoProp: Gen[ErgoBox] = ergoBoxGen(propGen = trueLeafGen)

  def ergoBoxGenForTokens(tokens: Seq[(TokenId, Long)],
                          propositionGen: Gen[Value[SBoolean.type]]): Gen[ErgoBox] = {
    ergoBoxGen(tokensGen = Gen.oneOf(tokens, tokens))
  }

  def unspendableErgoBoxGen(minValue: Long = 1, maxValue: Long = Long.MaxValue): Gen[ErgoBox] = {
    ergoBoxGen(propGen = falseLeafGen, valueGenOpt = Some(Gen.choose(minValue, maxValue)))
  }

  val byteArrayConstGen: Gen[CollectionConstant[SByte.type]] = for {
    length <- Gen.chooseNum(1, 100)
    bytes <- Gen.listOfN(length, arbByte.arbitrary)
  } yield ByteArrayConstant(bytes.toArray)

  def additionalRegistersGen: Gen[Map[NonMandatoryRegisterId, EvaluatedValue[SType]]] = for {
    cnt <- Gen.choose(0: Byte, ErgoBox.nonMandatoryRegistersCount)
    registers <- additionalRegistersGen(cnt)
  } yield registers

  def additionalRegistersGen(cnt: Byte): Gen[Map[NonMandatoryRegisterId, EvaluatedValue[SType]]] = {
    Gen.listOfN(cnt, evaluatedValueGen) map { values =>
      ErgoBox.nonMandatoryRegisters.take(cnt).zip(values).toMap
    }
  }

  def evaluatedValueGen: Gen[EvaluatedValue[SType]] = for {
    arr <- byteArrayConstGen
    v <- Gen.oneOf(TrueLeaf, FalseLeaf, arr)
  } yield v.asInstanceOf[EvaluatedValue[SType]]

  def additionalTokensGen: Gen[Seq[(TokenId, Long)]] = for {
    cnt <- Gen.chooseNum[Byte](0, ErgoBox.MaxTokens)
    assets <- additionalTokensGen(cnt)
  } yield assets

  def additionalTokensGen(cnt: Byte): Gen[Seq[(TokenId, Long)]] = Gen.listOfN(cnt, assetGen)

  def assetGen: Gen[(TokenId, Long)] = for {
    id <- boxIdGen
    amt <- Gen.oneOf(1, 500, 20000, 10000000, Long.MaxValue)
  } yield Digest32 @@ id -> amt

  lazy val inputGen: Gen[Input] = for {
    boxId <- boxIdGen
    spendingProof <- noProofGen
  } yield Input(boxId, spendingProof)

  lazy val invalidErgoTransactionGen: Gen[ErgoTransaction] = for {
    from: IndexedSeq[Input] <- smallInt.flatMap(i => Gen.listOfN(i + 1, inputGen).map(_.toIndexedSeq))
    to: IndexedSeq[ErgoBoxCandidate] <- smallInt.flatMap(i => Gen.listOfN(i, ergoBoxCandidateGen).map(_.toIndexedSeq))
  } yield ErgoTransaction(from, to)

  def validTransactionFromBoxes(boxesToSpend: Seq[ErgoBox], rnd: Random = new Random): ErgoTransaction = {
    require(boxesToSpend.nonEmpty, "At least one box is needed to generate a transaction")
    val inputSum = boxesToSpend.map(_.value).reduce(Math.addExact(_, _))
    val assetsMap: mutable.Map[ByteArrayWrapper, Long] =
      mutable.Map(boxesToSpend.flatMap(_.additionalTokens).map { case (bs, amt) =>
        ByteArrayWrapper(bs) -> amt
      }: _*)

    //randomly creating a new asset
    if (Random.nextBoolean()) assetsMap.put(ByteArrayWrapper(boxesToSpend.head.id), rnd.nextInt(Int.MaxValue))

    val inputsCount = boxesToSpend.size
    val outputsCount = Math.min(Short.MaxValue, Math.max(inputsCount + 1, rnd.nextInt(inputsCount * 2)))

    val outputAmounts = (1 to outputsCount).foldLeft(Seq[Long]() -> inputSum) { case ((amounts, remainder), idx) =>
      val amount = if (idx == outputsCount) {
        remainder
      } else {
        Math.abs(rnd.nextLong()) % (remainder / inputsCount)
      }
      (amounts :+ amount) -> (remainder - amount)
    }._1.toIndexedSeq

    val tokenAmounts: mutable.IndexedSeq[mutable.Map[ByteArrayWrapper, Long]] =
      mutable.IndexedSeq.fill(outputsCount)(mutable.Map[ByteArrayWrapper, Long]())

    var availableTokenSlots = Math.min(outputsCount * ErgoBox.MaxTokens, ErgoTransaction.MaxTokens)

    if (assetsMap.nonEmpty) {
      do {
        val in = assetsMap.head
        val outIdx = Stream.from(1, 1).map(_ => rnd.nextInt(tokenAmounts.size))
          .find(idx => tokenAmounts(idx).size < ErgoBox.MaxTokens).get
        val out = tokenAmounts(outIdx)
        val contains = out.contains(in._1)

        val amt = if (in._2 == 1 || (availableTokenSlots < assetsMap.size * 2 && !contains) || rnd.nextBoolean()) {
          in._2
        } else {
          Math.max(1, Math.min((rnd.nextDouble() * in._2).toLong, in._2))
        }

        if (amt == in._2) assetsMap.remove(in._1) else assetsMap.update(in._1, in._2 - amt)
        if (contains) {
          val outAmt = out(in._1)
          out.update(in._1, outAmt + amt)
        } else {
          availableTokenSlots = availableTokenSlots - 1
          out.update(in._1, amt)
        }
        tokenAmounts(outIdx) = out
      } while (assetsMap.nonEmpty && availableTokenSlots > 0)
    }

    val newBoxes = outputAmounts.zip(tokenAmounts.toIndexedSeq).map { case (amt, tokens) =>
      val normalizedTokens = tokens.toSeq.map(t => (Digest32 @@ t._1.data) -> t._2)
      ErgoBox(amt, TrueLeaf, normalizedTokens, creationHeight = 0)
    }

    val noProof = ProverResult(SigSerializer.toBytes(NoProof), ContextExtension.empty)
    val inputs = boxesToSpend.map(box => Input(box.id, noProof)).toIndexedSeq
    ErgoTransaction(inputs, newBoxes)
  }

  def disperseTokens(inputsCount: Int, tokensCount: Byte): Gen[IndexedSeq[Seq[(TokenId, Long)]]] = {
    val tokensDistribution = mutable.IndexedSeq.fill(inputsCount)(Seq[(TokenId, Long)]())
    (1 to tokensCount).foreach { i =>
      val (id, amt) = Blake2b256(s"$i" + Random.nextString(5)) -> (Random.nextInt(Int.MaxValue).toLong + 100)
      val idx = i % tokensDistribution.size
      val s = tokensDistribution(idx)
      tokensDistribution(idx) = s :+ (id, amt)
    }
    tokensDistribution.ensuring(_.forall(_.forall(_._2 > 0)))
  }

  def validErgoTransactionGenTemplate(minAssets: Int,
                                      maxAssets: Int = -1,
                                      minInputs: Int = 1,
                                      maxInputs: Int = 100,
                                      propositionGen: Gen[Value[SBoolean.type]] = trueLeafGen
                                     ): Gen[(IndexedSeq[ErgoBox], ErgoTransaction)] = for {
    inputsCount <- Gen.choose(minInputs, maxInputs)
    tokensCount <- Gen.choose(
      minAssets,
      Math.max(maxAssets, Math.min(inputsCount * ErgoBox.MaxTokens, ErgoTransaction.MaxTokens - 1)))
    tokensDistribution <- disperseTokens(inputsCount, tokensCount.toByte)
    from <- Gen.sequence(tokensDistribution.map(tokens => ergoBoxGenForTokens(tokens, propositionGen)))
    tx = validTransactionFromBoxes(from.asScala.toIndexedSeq)
  } yield from.asScala.toIndexedSeq -> tx

  lazy val validErgoTransactionGen: Gen[(IndexedSeq[ErgoBox], ErgoTransaction)] = validErgoTransactionGenTemplate(0)
  lazy val validErgoTransactionWithAssetsGen: Gen[(IndexedSeq[ErgoBox], ErgoTransaction)] =
    validErgoTransactionGenTemplate(1)

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
    extension <- extensionGen
    proof <- randomADProofsGen
  } yield ErgoFullBlock(header, txs, extension, Some(proof))
}

object ErgoTransactionGenerators extends ErgoTransactionGenerators
