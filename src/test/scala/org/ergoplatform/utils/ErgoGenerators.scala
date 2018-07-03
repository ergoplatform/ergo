package org.ergoplatform.utils

import io.iohk.iodb.ByteArrayWrapper
import org.bouncycastle.util.BigIntegers
import org.ergoplatform.ErgoBox.{BoxId, NonMandatoryRegisterId, R4, TokenId}
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
import org.scalacheck.Arbitrary.arbByte
import org.scalacheck.{Arbitrary, Gen}
import org.scalatest.Matchers
import scapi.sigma.DLogProtocol.DLogProverInput
import scorex.core.ModifierId
import scorex.crypto.authds.{ADDigest, ADKey, SerializedAdProof}
import scorex.crypto.hash.{Blake2b256, Digest32}
import scorex.testkit.generators.CoreGenerators
import sigmastate._
import sigmastate.Values.{ByteArrayConstant, CollectionConstant, EvaluatedValue, FalseLeaf, IntConstant, TrueLeaf, Value}
import sigmastate.interpreter.{ContextExtension, SerializedProverResult}

import scala.collection.JavaConverters._
import scala.collection.mutable
import scala.util.Random


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
      val (id, amt) = Blake2b256(s"$i" + Random.nextString(5)) -> (Random.nextDouble() * Long.MaxValue).toLong
      val idx = i % tokenDistrib.size
      val s = tokenDistrib(idx)
      tokenDistrib(idx) = s :+ (id, amt)
    }
    tokenDistrib
  }

  lazy val validErgoTransactionGen: Gen[(IndexedSeq[ErgoBox], ErgoTransaction)] = for {
    inputsCount <- Gen.choose(1, 100)
    tokensCount <- Gen.choose(0, Math.min(inputsCount * ErgoBox.MaxTokens, ErgoTransaction.MaxTokens - 1))
    tokensDistribution <- disperseTokens(inputsCount, tokensCount.toByte)
    from <- Gen.sequence(tokensDistribution.map(ergoBoxGenForTokens))
    tx <- validTransactionGen(from.asScala.toIndexedSeq)
  } yield from.asScala.toIndexedSeq -> tx


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
