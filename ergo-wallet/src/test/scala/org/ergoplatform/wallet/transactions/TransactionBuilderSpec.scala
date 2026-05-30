package org.ergoplatform.wallet.transactions

import org.ergoplatform.ErgoBox.TokenId
import org.ergoplatform._
import org.ergoplatform.sdk.{BlockchainParameters, SecretString}
import org.ergoplatform.sdk.wallet.TokensMap
import org.ergoplatform.sdk.wallet.secrets.ExtendedSecretKey
import org.ergoplatform.wallet.boxes.BoxSelector.{BoxSelectionError, BoxSelectionResult}
import org.ergoplatform.wallet.boxes.{BoxSelector, DefaultBoxSelector, ReemissionData}
import org.ergoplatform.wallet.mnemonic.Mnemonic
import org.ergoplatform.wallet.utils.WalletTestHelpers
import org.scalatest.matchers.should.Matchers
import sigma.ast.{ByteArrayConstant, ErgoTree, TrueLeaf}
import sigma.ast.syntax.SigmaPropValue
import sigma.data.SigmaConstants.MaxBoxSize
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
              fee: Option[Long] = Some(minBoxValue)): Try[UnsignedErgoLikeTransaction] =
    buildTxWith(inputs, outputs, height = height, fee = fee)

  // fuller variant also exposing the box selector, network parameters and reemission script
  def buildTxWith(inputs: IndexedSeq[ErgoBox],
                  outputs: IndexedSeq[ErgoBoxCandidate],
                  height: Int = currentHeight,
                  fee: Option[Long] = Some(minBoxValue),
                  boxSelector: BoxSelector = new DefaultBoxSelector(None),
                  parameters: Option[BlockchainParameters] = None,
                  payToReemissionScript: Option[ErgoTree] = None): Try[UnsignedErgoLikeTransaction] =
    buildUnsignedTx(
      inputs = inputs,
      dataInputs = IndexedSeq(),
      outputCandidates = outputs,
      currentHeight = height,
      createFeeOutput = fee,
      changeAddress = P2PKAddress(rootSecret.privateInput.publicImage),
      minChangeValue = minChangeValue,
      minerRewardDelay = minerRewardDelay,
      boxSelector = boxSelector,
      parameters = parameters,
      payToReemissionScript = payToReemissionScript
    )

  // minimal BlockchainParameters with overridable minValuePerByte / blockVersion
  def params(minValuePerByteV: Int = BoxSelector.MinValuePerByteDefault,
             blockVersionV: Byte = 3): BlockchainParameters =
    new BlockchainParameters {
      override def storageFeeFactor: Int = 1250000
      override def minValuePerByte: Int = minValuePerByteV
      override def maxBlockSize: Int = 524288
      override def tokenAccessCost: Int = 100
      override def inputCost: Int = 2000
      override def dataInputCost: Int = 100
      override def outputCost: Int = 100
      override def maxBlockCost: Int = 1000000
      override def softForkStartingHeight: Option[Int] = None
      override def softForkVotesCollected: Option[Int] = None
      override def blockVersion: Byte = blockVersionV
    }

  // box selector echoing back the given inputs with caller-supplied change / reemission outputs, used to
  // exercise the defensive checks (txErgPreservation, txAssetsPreservation) and the EIP-27 output path
  class StubBoxSelector(changeBoxes: Seq[ErgoBoxAssets],
                        payToReemission: Option[ErgoBoxAssets] = None) extends BoxSelector {
    override def reemissionDataOpt: Option[ReemissionData] = None
    override def select[T <: ErgoBoxAssets](inputBoxes: Iterator[T],
                                            filterFn: T => Boolean,
                                            targetBalance: Long,
                                            targetAssets: TokensMap): Either[BoxSelectionError, BoxSelectionResult[T]] =
      Right(new BoxSelectionResult[T](inputBoxes.toIndexedSeq, changeBoxes, payToReemission))
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

  property("rejects oversized output box (txBoxSize)") {
    // a register larger than MaxBoxSize pushes the whole box over the limit; the value is set high
    // enough to clear the (now larger) dust threshold so the box-size rule is the one that fires
    val oversized = Array.fill(MaxBoxSize.value.toInt)(1.toByte)
    val bigValue  = MaxBoxSize.value.toLong * BoxSelector.MinValuePerByteDefault * 2
    val bigBox = new ErgoBoxCandidate(
      bigValue, TrueTree, currentHeight,
      Array.empty[(TokenId, Long)].toColl,
      Map(ErgoBox.R4 -> ByteArrayConstant(oversized)))
    val inputBox = box(bigValue * 2)
    val res      = buildTxWith(IndexedSeq(inputBox), IndexedSeq(bigBox), fee = None)

    assertExceptionThrown(res.getOrThrow, t => t.getMessage.contains("txBoxSize"))
  }

  // Note: txBoxPropositionSize cannot be triggered independently - a proposition over MaxPropositionBytes
  // also makes the whole box exceed MaxBoxSize, so txBoxSize fires first. Likewise txInputsSum: inputs
  // that overflow Long are already rejected by the `changeAmt >= 0` check before the stateful pass.

  property("rejects transaction where ERGs are not preserved (txErgPreservation)") {
    val inputBox = box(minBoxValue * 2)
    val outBox   = boxCandidate(minBoxValue)
    // selector returns an inflated change box, so outputs sum to more than inputs
    val selector = new StubBoxSelector(changeBoxes = Seq(ErgoBoxAssetsHolder(minBoxValue * 5)))
    val res      = buildTxWith(IndexedSeq(inputBox), IndexedSeq(outBox), fee = None, boxSelector = selector)

    assertExceptionThrown(res.getOrThrow, t => t.getMessage.contains("txErgPreservation"))
  }

  property("rejects transaction where tokens are not preserved (txAssetsPreservation)") {
    val inputBox = box(minBoxValue * 2, Seq(tid1.toTokenId -> 100L))
    val outBox   = boxCandidate(minBoxValue)
    // selector returns a change box claiming more of tid1 than the inputs hold (ERGs stay balanced)
    val selector = new StubBoxSelector(changeBoxes = Seq(ErgoBoxAssetsHolder(minBoxValue, Map(tid1 -> 500L))))
    val res      = buildTxWith(IndexedSeq(inputBox), IndexedSeq(outBox), fee = None, boxSelector = selector)

    assertExceptionThrown(res.getOrThrow, t => t.getMessage.contains("txAssetsPreservation"))
  }

  property("adds the EIP-27 pay-to-reemission output supplied by the selector") {
    val inputBox         = box(minBoxValue * 3)
    val outBox           = boxCandidate(minBoxValue)
    val reemissionScript = ErgoTreePredef.feeProposition(minerRewardDelay) // distinct from out/change scripts
    // selector splits the remainder: one box to reemission, one to change (ERGs stay balanced)
    val selector = new StubBoxSelector(
      changeBoxes     = Seq(ErgoBoxAssetsHolder(minBoxValue)),
      payToReemission = Some(ErgoBoxAssetsHolder(minBoxValue)))
    val res = buildTxWith(
      IndexedSeq(inputBox), IndexedSeq(outBox), fee = None,
      boxSelector = selector, payToReemissionScript = Some(reemissionScript))

    res shouldBe a[Success[_]]
    res.get.outputCandidates.exists(o => o.ergoTree == reemissionScript && o.value == minBoxValue) shouldBe true
  }

  property("fails when a pay-to-reemission output is needed but no script is supplied") {
    val inputBox = box(minBoxValue * 3)
    val outBox   = boxCandidate(minBoxValue)
    val selector = new StubBoxSelector(
      changeBoxes     = Seq(ErgoBoxAssetsHolder(minBoxValue)),
      payToReemission = Some(ErgoBoxAssetsHolder(minBoxValue)))
    val res = buildTxWith(IndexedSeq(inputBox), IndexedSeq(outBox), fee = None, boxSelector = selector)

    assertExceptionThrown(res.getOrThrow, t => t.getMessage.contains("payToReemissionScript"))
  }

  property("uses minValuePerByte from supplied parameters (txDust)") {
    // a box fine at the default per-byte price becomes dust under a much higher price
    val inputBox = box(minBoxValue * 2)
    val outBox   = boxCandidate(minBoxValue)
    val res = buildTxWith(
      IndexedSeq(inputBox), IndexedSeq(outBox), fee = None,
      parameters = Some(params(minValuePerByteV = Int.MaxValue)))

    assertExceptionThrown(res.getOrThrow, t => t.getMessage.contains("txDust"))
  }

  property("relaxes monotonic-height for a pre-hardening block version (txMonotonicHeight)") {
    // same shape as the rejecting test above, but a pre-hardening block version disables the rule
    val inputBox = testBox(minBoxValue * 2, TrueTree, 100)
    val outBox   = new ErgoBoxCandidate(minBoxValue, TrueTree, 50)
    val res = buildTxWith(
      IndexedSeq(inputBox), IndexedSeq(outBox), height = 100, fee = None,
      parameters = Some(params(blockVersionV = 1.toByte)))

    res shouldBe a[Success[_]]
  }

}
