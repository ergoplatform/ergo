package org.ergoplatform.wallet.transactions

import org.ergoplatform.ErgoBox.TokenId
import org.ergoplatform._
import org.ergoplatform.sdk.SecretString
import org.ergoplatform.sdk.wallet.TokensMap
import org.ergoplatform.sdk.wallet.secrets.ExtendedSecretKey
import org.ergoplatform.wallet.boxes.BoxSelector
import org.ergoplatform.wallet.mnemonic.Mnemonic
import org.ergoplatform.wallet.utils.WalletTestHelpers
import org.scalatest.matchers.should.Matchers
import sigma.ast.{ErgoTree, TrueLeaf}
import sigma.ast.syntax.SigmaPropValue
import sigmastate.eval.Extensions._
import sigmastate.helpers.TestingHelpers._
import sigmastate.utils.Extensions._
import sigmastate.utils.Helpers._
import sigma.Extensions.ArrayOps
import sigma.eval.Extensions.EvalIterableOps

import scala.util.{Success, Try}

class TransactionBuilderSpec extends WalletTestHelpers with Matchers {
  import TransactionBuilder.buildUnsignedTx

  implicit val addressEncoder = new ErgoAddressEncoder(
    ErgoAddressEncoder.TestnetNetworkPrefix
  )

  val seedStr                       = "edge talent poet tortoise trumpet dose"
  val seed: Array[Byte]             = Mnemonic.toSeed(SecretString.create(seedStr))
  val rootSecret: ExtendedSecretKey = ExtendedSecretKey.deriveMasterKey(seed, usePre1627KeyDerivation = false)

  val currentHeight    = 0
  val minBoxValue      = BoxSelector.MinBoxValue
  val minChangeValue   = BoxSelector.MinBoxValue
  val minerRewardDelay = 720

  val TrueProp: SigmaPropValue = TrueLeaf.toSigmaProp
  val TrueTree = ErgoTree.fromProposition(TrueProp)

  val tid1 = stringToId("t1")
  val tid2 = stringToId("t2")

  def box(value: Long) = testBox(value, TrueTree, currentHeight)

  def box(value: Long, tokens: Seq[(TokenId, Long)]) =
    testBox(value, TrueTree, currentHeight, tokens)

  def boxCandidate(value: Long) = new ErgoBoxCandidate(value, TrueTree, currentHeight)

  def boxCandidate(value: Long, tokens: Seq[(TokenId, Long)]) =
    new ErgoBoxCandidate(value, TrueTree, currentHeight, tokens.toColl)

  def transaction(inputBox: ErgoBox,
                  outBox: ErgoBoxCandidate,
                  fee: Option[Long] = Some(minBoxValue),
                  burnTokens: TokensMap = Map.empty): Try[UnsignedErgoLikeTransaction] = {
    val ins = IndexedSeq(inputBox)
    val outs = IndexedSeq(outBox)
    val changeAddress = P2PKAddress(rootSecret.privateInput.publicImage)
    val res = buildUnsignedTx(
      inputs = ins,
      dataInputs = IndexedSeq(),
      outputCandidates = outs,
      currentHeight = currentHeight,
      createFeeOutput = fee,
      changeAddress = changeAddress,
      minChangeValue = minChangeValue,
      minerRewardDelay = minerRewardDelay,
      burnTokens = burnTokens
    )
    res
  }

  // direct call variant allowing custom inputs/outputs and height (to exercise the stateful checks)
  def buildTx(inputs: IndexedSeq[ErgoBox],
              outputs: IndexedSeq[ErgoBoxCandidate],
              height: Int = currentHeight,
              fee: Option[Long] = Some(minBoxValue)): Try[UnsignedErgoLikeTransaction] = {
    val changeAddress = P2PKAddress(rootSecret.privateInput.publicImage)
    buildUnsignedTx(
      inputs = inputs,
      dataInputs = IndexedSeq(),
      outputCandidates = outputs,
      currentHeight = height,
      createFeeOutput = fee,
      changeAddress = changeAddress,
      minChangeValue = minChangeValue,
      minerRewardDelay = minerRewardDelay
    )
  }

  property("token minting") {
    val inputBox = box(minBoxValue * 2)
    val tokenId  = inputBox.id.toTokenId
    val outBox = boxCandidate(minBoxValue, Seq(tokenId -> 100L))
    val res = transaction(inputBox, outBox)

    res shouldBe a[Success[_]]
    val tx = res.get
    //  added miner fee
    tx.outputCandidates.size shouldBe 2
    tx.outputCandidates(0) shouldEqual outBox
  }

  property("token burning") {
    val inputBox = box(minBoxValue * 3, Seq(tid1.toTokenId -> 1000L, tid2.toTokenId -> 2000L))
    val tokenId  = inputBox.id.toTokenId
    val outBox = boxCandidate(minBoxValue, Seq(tokenId -> 100L))
    val res = transaction(inputBox, outBox, burnTokens = Map(tid1 -> 400L, tid2 -> 800L))

    res shouldBe a[Success[_]]
    val tx = res.get
    //  added miner fee
    tx.outputCandidates.size shouldBe 3
    val Seq(out0, out1, out2) = tx.outputCandidates
    out0 shouldEqual outBox
    out1.value shouldBe minBoxValue
    out2.value shouldBe minBoxValue
    val remainingTokens = Map(tid1 -> 600L, tid2 -> 1200L)
    TransactionBuilder.collTokensToMap(out2.additionalTokens) shouldBe remainingTokens
  }

  property("no fees") {
    val inputBox = box(minBoxValue)
    val tokenId  = inputBox.id.toTokenId
    val outBox = boxCandidate(minBoxValue, Seq(tokenId -> 100L))
    val res = transaction(inputBox, outBox, fee = None)

    res shouldBe a[Success[_]]
    val tx = res.get
    tx.outputCandidates.size shouldBe 1
    tx.outputCandidates(0) shouldEqual outBox
  }

  property("change goes to fee, but no outFee box") {
    val inputBox = box(minBoxValue + minBoxValue / 2)
    val tokenId  = inputBox.id.toTokenId
    val outBox = boxCandidate(minBoxValue, Seq(tokenId -> 100L))
    val res = transaction(inputBox, outBox, fee = None)

    assertExceptionThrown(
      res.getOrThrow,
      t => t.getMessage.contains("createFeeOutput should be defined"))
  }

  property("rejects output with creation height in the future (txFuture)") {
    val inputBox = testBox(minBoxValue * 2, TrueTree, 5)
    val outBox   = new ErgoBoxCandidate(minBoxValue, TrueTree, 15)
    val res      = buildTx(IndexedSeq(inputBox), IndexedSeq(outBox), height = 5)

    assertExceptionThrown(res.getOrThrow, t => t.getMessage.contains("txFuture"))
  }

  property("rejects output with negative creation height (txNegHeight)") {
    val inputBox = testBox(minBoxValue * 2, TrueTree, 0)
    val outBox   = new ErgoBoxCandidate(minBoxValue, TrueTree, -1)
    val res      = buildTx(IndexedSeq(inputBox), IndexedSeq(outBox), height = 0)

    assertExceptionThrown(res.getOrThrow, t => t.getMessage.contains("txNegHeight"))
  }

  property("rejects output below max input creation height (txMonotonicHeight)") {
    val inputBox = testBox(minBoxValue * 2, TrueTree, 100)
    val outBox   = new ErgoBoxCandidate(minBoxValue, TrueTree, 50)
    val res      = buildTx(IndexedSeq(inputBox), IndexedSeq(outBox), height = 100)

    assertExceptionThrown(res.getOrThrow, t => t.getMessage.contains("txMonotonicHeight"))
  }

  property("accepts output at max input creation height (txMonotonicHeight)") {
    val inputBox = testBox(minBoxValue * 2, TrueTree, 100)
    val outBox   = new ErgoBoxCandidate(minBoxValue, TrueTree, 100)
    val res      = buildTx(IndexedSeq(inputBox), IndexedSeq(outBox), height = 100)

    res shouldBe a[Success[_]]
  }

  property("rejects non-positive token amount in output (txPositiveAssets)") {
    val inputBox = testBox(minBoxValue * 2, TrueTree, currentHeight, Seq(tid1.toTokenId -> 100L))
    val outBox   = boxCandidate(minBoxValue, Seq(tid1.toTokenId -> 0L))
    val res      = buildTx(IndexedSeq(inputBox), IndexedSeq(outBox))

    assertExceptionThrown(res.getOrThrow, t => t.getMessage.contains("txPositiveAssets"))
  }

  // Note: txAssetsInOneBox's per-box token-count limit (<= 255) is enforced by ErgoBoxCandidate
  // construction itself (the count is an unsigned byte), so a violating output cannot be built to test
  // here; the check in buildUnsignedTx remains as a faithful guard against cross-output sum overflow.

  property("rejects dust output below the min value per byte (txDust)") {
    val inputBox = box(1L)
    val outBox   = boxCandidate(1L)
    val res      = buildTx(IndexedSeq(inputBox), IndexedSeq(outBox), fee = None)

    assertExceptionThrown(res.getOrThrow, t => t.getMessage.contains("txDust"))
  }

}
