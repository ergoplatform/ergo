package org.ergoplatform.utils.generators

import org.ergoplatform.modifiers.mempool.{ErgoTransaction, UnsignedErgoTransaction}
import org.ergoplatform.nodeView.state.wrapped.WrappedUtxoState
import org.ergoplatform.nodeView.state.{BoxHolder, ErgoStateContext}
import org.ergoplatform.nodeView.wallet.requests.{ExternalSecret, TransactionSigningRequest}
import org.ergoplatform.nodeView.wallet.{AugWalletTransaction, WalletTransaction}
import org.ergoplatform.sdk.wallet.secrets.{DhtSecretKey, DlogSecretKey}
import org.ergoplatform.utils.{BoxUtils, RandomLike, RandomWrapper}
import org.ergoplatform.sdk.wallet.Constants.MaxAssetsPerBox
import org.ergoplatform.wallet.interpreter.TransactionHintsBag
import org.ergoplatform._
import org.ergoplatform.wallet.Constants.ScanId
import org.scalacheck.Gen
import scorex.db.ByteArrayWrapper
import scorex.util.ScorexLogging
import scorex.util.encode.Base16
import sigma.ast.ErgoTree
import sigmastate.eval.Extensions._
import sigmastate.helpers.TestingHelpers._
import org.ergoplatform.settings.Constants.TrueTree

import scala.collection.mutable
import scala.util.Random

object ErgoNodeTransactionGenerators extends ScorexLogging {
  import org.ergoplatform.utils.generators.ErgoCoreGenerators._
  import org.ergoplatform.utils.ErgoCoreTestConstants._
  import org.ergoplatform.utils.ErgoNodeTestConstants._
  import org.ergoplatform.utils.generators.ErgoCoreTransactionGenerators._

  lazy val walletTransactionGen: Gen[WalletTransaction] = for {
    tx <- invalidErgoTransactionGen
    inclusionHeight <- Gen.posNum[Int]
    scanId <- ScanId @@ Gen.posNum[Short]
  } yield WalletTransaction(tx, inclusionHeight, Seq(scanId))

  def walletTransactionForScanGen(scanId: ScanId): Gen[WalletTransaction] = for {
    tx <- invalidErgoTransactionGen
    inclusionHeight <- Gen.posNum[Int]
  } yield WalletTransaction(tx, inclusionHeight, Seq(scanId))

  def augWalletTransactionForScanGen(scanId: ScanId, includeUnconfirmed: Boolean): Gen[AugWalletTransaction] = for {
    tx <- walletTransactionForScanGen(scanId)
    numConfirmation <- if (includeUnconfirmed) Gen.const(0) else Gen.posNum[Int]
  } yield AugWalletTransaction(tx, numConfirmation)

  lazy val augWalletTransactionGen: Gen[AugWalletTransaction] = for {
    tx <- walletTransactionGen
    numConfirmation <- Gen.posNum[Int]
  } yield AugWalletTransaction(tx, numConfirmation)

  /**
    * Generates a transaction that is valid if correct boxes were provided.
    * Generated transaction may still be invalid, if:
    * - default prover does not know how to sign at least one input
    * - number of assets exceeds MaxAssetsPerBox
    */
  def validUnsignedTransactionFromBoxes(boxesToSpend: IndexedSeq[ErgoBox],
                                        rnd: RandomLike = new RandomWrapper,
                                        issueNew: Boolean = true,
                                        outputsProposition: ErgoTree = TrueTree,
                                        dataBoxes: IndexedSeq[ErgoBox] = IndexedSeq()): UnsignedErgoTransaction = {
    require(boxesToSpend.nonEmpty, "At least one box is needed to generate a transaction")

    val inputSum = boxesToSpend.map(_.value).reduce(Math.addExact(_, _))
    val assetsMap: mutable.Map[ByteArrayWrapper, Long] =
      mutable.Map(boxesToSpend.flatMap(_.additionalTokens.toArray).map { case (bs, amt) =>
        ByteArrayWrapper(bs.toArray) -> amt
      }: _*)

    //randomly creating a new asset
    if (rnd.nextBoolean() && issueNew) {
      assetsMap.put(ByteArrayWrapper(boxesToSpend.head.id), rnd.nextInt(Int.MaxValue))
    }

    val minValue = BoxUtils.sufficientAmount(extendedParameters)

    require(inputSum >= minValue)
    val inputsCount = boxesToSpend.size
    val maxOutputs = Math.min(Short.MaxValue, inputSum / minValue).toInt
    val outputsCount = Math.min(maxOutputs, Math.max(inputsCount + 1, rnd.nextInt(inputsCount * 2)))
    require(outputsCount > 0, s"outputs count is not positive: $outputsCount")

    require(minValue * outputsCount <= inputSum)
    val outputPreamounts = (1 to outputsCount).map(_ => minValue).toBuffer

    var remainder = inputSum - minValue * outputsCount
    do {
      val idx = Random.nextInt(outputsCount)
      if (remainder < inputSum / inputsCount) {
        outputPreamounts.update(idx, outputPreamounts(idx) + remainder)
        remainder = 0
      } else {
        val value = Math.abs(rnd.nextLong()) % (remainder / outputsCount)
        outputPreamounts.update(idx, outputPreamounts(idx) + value)
        remainder = remainder - value
      }
    } while (remainder > 0)

    val outputAmounts = outputPreamounts.toIndexedSeq

    val tokenAmounts: mutable.IndexedSeq[mutable.Map[ByteArrayWrapper, Long]] =
      mutable.IndexedSeq.fill(outputsCount)(mutable.Map[ByteArrayWrapper, Long]())

    var availableTokenSlots = outputsCount * MaxAssetsPerBox

    if (assetsMap.nonEmpty) {
      do {
        val in = assetsMap.head
        val outIdx = Stream.from(1, 1).map(_ => rnd.nextInt(tokenAmounts.size))
          .find(idx => tokenAmounts(idx).size < MaxAssetsPerBox).get
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
      val normalizedTokens = tokens.toSeq.map(t => t._1.data.toTokenId -> t._2)
      testBox(amt, outputsProposition, 0, normalizedTokens)
    }
    val inputs = boxesToSpend.map(b => Input(b.id, emptyProverResult))
    val dataInputs = dataBoxes.map(b => DataInput(b.id))
    val unsignedTx = UnsignedErgoTransaction(inputs, dataInputs, newBoxes)
    require(unsignedTx.dataInputs.length == dataBoxes.length, s"${unsignedTx.dataInputs.length} == ${dataBoxes.length}")
    unsignedTx
  }

  def validTransactionFromBoxes(boxesToSpend: IndexedSeq[ErgoBox],
                                rnd: RandomLike = new RandomWrapper,
                                issueNew: Boolean = true,
                                outputsProposition: ErgoTree = TrueTree,
                                stateCtxOpt: Option[ErgoStateContext] = None,
                                dataBoxes: IndexedSeq[ErgoBox] = IndexedSeq()): ErgoTransaction = {
    val unsignedTx = validUnsignedTransactionFromBoxes(boxesToSpend, rnd, issueNew, outputsProposition, dataBoxes)
    defaultProver.sign(unsignedTx, boxesToSpend, dataBoxes, stateCtxOpt.getOrElse(emptyStateContext))
      .map(ErgoTransaction.apply)
      .getOrElse {
        log.debug(s"Going to generate a transaction with incorrect spending proofs: $unsignedTx")
        ErgoTransaction(boxesToSpend.map(b => Input(b.id, emptyProverResult)), unsignedTx.dataInputs, unsignedTx.outputs)
      }
  }

  def validErgoTransactionGenTemplate(minAssets: Int,
                                      maxAssets: Int = -1,
                                      maxInputs: Int = 100,
                                      propositionGen: Gen[ErgoTree] = trueLeafGen
                                     ): Gen[(IndexedSeq[ErgoBox], ErgoTransaction)] = {
    boxesGenTemplate(minAssets, maxAssets, maxInputs, maxInputs, propositionGen).map { case (boxes, prop) =>
      val tx = validTransactionFromBoxes(boxes, outputsProposition = prop)
      boxes -> tx
    }
  }

  def validUnsignedErgoTransactionGenTemplate(minAssets: Int,
                                              maxAssets: Int,
                                              maxInputs: Int,
                                              propositionGen: Gen[ErgoTree]): Gen[(IndexedSeq[ErgoBox], UnsignedErgoTransaction)] = {
    boxesGenTemplate(minAssets, maxAssets, maxInputs, maxInputs, propositionGen).map { case (boxes, prop) =>
      val utx = validUnsignedTransactionFromBoxes(boxes, outputsProposition = prop)
      boxes -> utx
    }
  }

  def validUnsignedErgoTransactionGen(prop: ErgoTree*): Gen[(IndexedSeq[ErgoBox], UnsignedErgoTransaction)] =
    validUnsignedErgoTransactionGenTemplate(minAssets = 0, maxAssets = 5, maxInputs = 10, propositionGen = Gen.oneOf(prop))

  lazy val validUnsignedErgoTransactionGen: Gen[(IndexedSeq[ErgoBox], UnsignedErgoTransaction)] =
    validUnsignedErgoTransactionGenTemplate(minAssets = 0, maxAssets = 5, maxInputs = 10, propositionGen = trueLeafGen)

  lazy val validErgoTransactionGen: Gen[(IndexedSeq[ErgoBox], ErgoTransaction)] = validErgoTransactionGenTemplate(minAssets = 0)
  lazy val validErgoTransactionWithAssetsGen: Gen[(IndexedSeq[ErgoBox], ErgoTransaction)] =
    validErgoTransactionGenTemplate(minAssets = 1)

  def boxesHolderGenOfSize(numBoxes:Int): Gen[BoxHolder] = Gen.listOfN(numBoxes, ergoBoxGenForTokens(Seq(), trueLeafGen))
    .map(l => BoxHolder(l))

  lazy val boxesHolderGen: Gen[BoxHolder] = Gen.listOfN(2000, ergoBoxGenForTokens(Seq(), trueLeafGen))
    .map(l => BoxHolder(l))

  def transactionSigningRequestGen(includeInputs: Boolean): Gen[TransactionSigningRequest] = for {
    (secret, pubKey) <- dlogSecretWithPublicImageGen
    (secretDh, _) <- dhtSecretWithPublicImageGen
    (inputBoxes, utx) <- validUnsignedErgoTransactionGen(ErgoTree.fromSigmaBoolean(pubKey))
    inputBoxesEncoded = inputBoxes.map(b => Base16.encode(b.bytes))
    secretSeq = Seq(ExternalSecret(DlogSecretKey(secret)), ExternalSecret(DhtSecretKey(secretDh)))
  } yield TransactionSigningRequest(utx, TransactionHintsBag.empty, secretSeq,
    if (includeInputs) Some(inputBoxesEncoded) else None, None)

  def transactionSigningRequestGen(utxoSet: WrappedUtxoState): Gen[TransactionSigningRequest] = Gen.const {
    val inputBoxes = utxoSet.takeBoxes(3).toIndexedSeq

    val utx = UnsignedErgoTransaction(inputBoxes.map(b => new UnsignedInput(b.id)), inputBoxes)
    val coin = Random.nextBoolean()
    val inputBoxesEncoded = inputBoxes.map(b => Base16.encode(b.bytes))

    TransactionSigningRequest(utx, TransactionHintsBag.empty, Seq.empty, if (coin) Some(inputBoxesEncoded) else None, None)
  }

}
