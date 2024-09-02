package org.ergoplatform.utils.generators

import org.ergoplatform.ErgoBox.TokenId
import org.ergoplatform.modifiers.ErgoFullBlock
import org.ergoplatform.modifiers.history.BlockTransactions
import org.ergoplatform.modifiers.history.header.Header
import org.ergoplatform.modifiers.mempool.ErgoTransaction
import org.ergoplatform.nodeView.history.ErgoHistoryUtils._
import org.ergoplatform.nodeView.state.{ErgoStateContext, VotingData}
import org.ergoplatform.settings.Parameters._
import org.ergoplatform.settings.{Constants, Parameters}
import org.ergoplatform._
import org.scalacheck.Gen
import scorex.crypto.hash.Blake2b256
import scorex.util.ScorexLogging
import sigma.ast.ErgoTree
import sigma.data.ProveDlog
import sigma.eval.Extensions._
import sigmastate.eval.Extensions._

import scala.collection.JavaConverters._
import scala.collection.mutable
import scala.util.Random

object ErgoCoreTransactionGenerators extends ScorexLogging {
  import org.ergoplatform.utils.generators.ErgoCoreGenerators._
  import org.ergoplatform.utils.ErgoCoreTestConstants._
  import org.ergoplatform.wallet.utils.WalletGenerators._
  import org.ergoplatform.utils.generators.CoreObjectGenerators._

  implicit val addressEncoder: ErgoAddressEncoder =
    ErgoAddressEncoder(chainSettings.addressPrefix)

  val creationHeightGen: Gen[Int] = Gen.choose(0, Int.MaxValue / 2)

  lazy val ergoBoxCandidateGen: Gen[ErgoBoxCandidate] = for {
    h <- creationHeightGen
    prop <- trueLeafGen
    ar <- additionalRegistersGen
    tokens <- additionalTokensGen
    value <- validValueGen
  } yield new ErgoBoxCandidate(value, prop, h, tokens.toColl, ar)

  def ergoAddressGen: Gen[ErgoAddress] = proveDlogGen.map(P2PKAddress.apply)

  def ergoBoxCandidateGen(prop: ProveDlog): Gen[ErgoBoxCandidate] = for {
    h <- creationHeightGen
    ar <- additionalRegistersGen
    tokens <- additionalTokensGen
    value <- validValueGen
  } yield new ErgoBoxCandidate(value, ErgoTree.fromSigmaBoolean(prop), h, tokens.toColl, ar)

  lazy val ergoBoxGenNoProp: Gen[ErgoBox] = ergoBoxGen(propGen = trueLeafGen)

  def ergoBoxGenForTokens(tokens: Seq[(TokenId, Long)],
                          propositionGen: Gen[ErgoTree]): Gen[ErgoBox] = {
    ergoBoxGen(propGen = propositionGen, tokensGen = Gen.oneOf(tokens, tokens), heightGen = EmptyHistoryHeight)
  }

  def unspendableErgoBoxGen(minValue: Long = parameters.minValuePerByte * 200,
                            maxValue: Long = coinsTotal): Gen[ErgoBox] = {
    ergoBoxGen(propGen = falseLeafGen, valueGenOpt = Some(Gen.choose(minValue, maxValue)))
  }

  lazy val inputGen: Gen[Input] = for {
    boxId <- boxIdGen
    spendingProof <- noProofGen
  } yield Input(boxId, spendingProof)

  lazy val dataInputGen: Gen[DataInput] = for {
    boxId <- boxIdGen
  } yield DataInput(boxId)

  lazy val reallySmallInt: Gen[Int] = Gen.choose(0, 3)

  lazy val invalidErgoTransactionGen: Gen[ErgoTransaction] = for {
    from: IndexedSeq[Input] <- reallySmallInt.flatMap(i => Gen.listOfN(i + 1, inputGen).map(_.toIndexedSeq))
    dataInputs: IndexedSeq[DataInput] <- reallySmallInt.flatMap(i => Gen.listOfN(i + 1, dataInputGen).map(_.toIndexedSeq))
    to: IndexedSeq[ErgoBoxCandidate] <- reallySmallInt.flatMap(i => Gen.listOfN(i + 1, ergoBoxCandidateGen).map(_.toIndexedSeq))
  } yield ErgoTransaction(from, dataInputs, to)

  def invalidErgoTransactionGen(prop: ProveDlog): Gen[ErgoTransaction] = for {
    from: IndexedSeq[Input] <- reallySmallInt.flatMap(i => Gen.listOfN(i + 1, inputGen).map(_.toIndexedSeq))
    dataInputs: IndexedSeq[DataInput] <- reallySmallInt.flatMap(i => Gen.listOfN(i + 1, dataInputGen).map(_.toIndexedSeq))
    to: IndexedSeq[ErgoBoxCandidate] <- reallySmallInt.flatMap(i => Gen.listOfN(i + 1, ergoBoxCandidateGen(prop)).map(_.toIndexedSeq))
  } yield ErgoTransaction(from, dataInputs, to)

  def disperseTokens(inputsCount: Int, tokensCount: Byte): Gen[mutable.IndexedSeq[Seq[(TokenId, Long)]]] = {
    val tokensDistribution: mutable.IndexedSeq[Seq[(TokenId, Time)]] = mutable.IndexedSeq.fill(inputsCount)(Seq[(TokenId, Long)]())
    (1 to tokensCount).foreach { i =>
      val (id, amt) = Blake2b256(s"$i" + Random.nextString(5)).toTokenId -> (Random.nextInt(Int.MaxValue).toLong + 100)
      val idx = i % tokensDistribution.size
      val s = tokensDistribution(idx)
      tokensDistribution(idx) = s :+ (id -> amt)
    }
    tokensDistribution.ensuring(_.forall(_.forall(_._2 > 0)))
  }

  def boxesGenTemplate(minAssets: Int,
                       maxAssets: Int,
                       minInputs: Int,
                       maxInputs: Int,
                       propositionGen: Gen[ErgoTree]): Gen[(IndexedSeq[ErgoBox], ErgoTree)] = for {
    inputsCount <- Gen.choose(minInputs, maxInputs)
    tokensCount <- Gen.choose(
      minAssets,
      Math.max(maxAssets, inputsCount))
    tokensDistribution <- disperseTokens(inputsCount, tokensCount.toByte)
    from <- Gen.sequence(tokensDistribution.map(tokens => ergoBoxGenForTokens(tokens, propositionGen)))
    prop <- propositionGen
  } yield from.asScala.toIndexedSeq -> prop

  lazy val invalidBlockTransactionsGen: Gen[BlockTransactions] = for {
    headerId <- modifierIdGen
    txs <- Gen.nonEmptyListOf(invalidErgoTransactionGen)
  } yield BlockTransactions(headerId, Header.InitialVersion, txs.foldLeft(Seq.empty[ErgoTransaction])((acc, tx) =>
    if ((acc :+ tx).map(_.size).sum < (Parameters.MaxBlockSizeDefault - 150)) acc :+ tx else acc))

  def invalidBlockTransactionsGen(prop: ProveDlog, txQty: Int): Gen[BlockTransactions] = for {
    headerId <- modifierIdGen
    txs <- Gen.listOfN(txQty, invalidErgoTransactionGen(prop))
  } yield BlockTransactions(headerId, Header.InitialVersion, txs.foldLeft(Seq.empty[ErgoTransaction])((acc, tx) =>
    if ((acc :+ tx).map(_.size).sum < (Parameters.MaxBlockSizeDefault - 150)) acc :+ tx else acc))

  lazy val invalidErgoFullBlockGen: Gen[ErgoFullBlock] = for {
    header <- defaultHeaderGen
    txs <- invalidBlockTransactionsGen
    extension <- extensionGen
    proof <- randomADProofsGen
  } yield ErgoFullBlock(header, txs, extension, Some(proof))

  def invalidErgoFullBlockGen(prop: ProveDlog, txQty: Int): Gen[ErgoFullBlock] = for {
    header <- defaultHeaderGen
    txs <- invalidBlockTransactionsGen(prop, txQty)
    extension <- extensionGen
    proof <- randomADProofsGen
  } yield ErgoFullBlock(header, txs, extension, Some(proof))

  lazy val paramVoteGen: Gen[Byte] = for {
    paramVote <- Gen.oneOf(Seq(NoParameter, StorageFeeFactorIncrease, MinValuePerByteIncrease))
  } yield paramVote

  lazy val paramVotesGen: Gen[Array[Byte]] = for {
    firstVote <- paramVoteGen
  } yield Array(firstVote, NoParameter, NoParameter)

  lazy val ergoStateContextGen: Gen[ErgoStateContext] = for {
    size <- Gen.choose(0, Constants.LastHeadersInContext + 3)
    stateRoot <- stateRootGen
    blocks <- Gen.listOfN(size, invalidErgoFullBlockGen)
    votes <- Gen.listOfN(size, paramVotesGen)
  } yield {
    blocks match {
      case _ :: _ =>
        val sc = new ErgoStateContext(Seq(), None, startDigest, parameters, validationSettingsNoIl, VotingData.empty)(chainSettings)
        blocks.foldLeft(sc -> 1) { case ((c, h), b) =>
          val block = b.copy(header = b.header.copy(height = h, votes = votes(h - 1)))
          c.appendFullBlock(block).get -> (h + 1)
        }._1
      case _ =>
        ErgoStateContext.empty(stateRoot, chainSettings, parameters)
    }
  }

}
