package org.ergoplatform.wallet.transactions

import sigmastate.Values
import sigmastate.Values.SigmaPropValue
import sigmastate.eval._
import sigmastate.eval.Extensions._
import sigmastate.helpers.TestingHelpers._
import org.ergoplatform._
import org.scalatest.Matchers
import org.ergoplatform.ErgoBox.TokenId
import org.ergoplatform.wallet.TokensMap

import scala.util.{Success, Try}
import scorex.crypto.hash.Digest32
import org.ergoplatform.wallet.mnemonic.Mnemonic
import org.ergoplatform.wallet.secrets.ExtendedSecretKey
import org.ergoplatform.wallet.boxes.BoxSelector
import org.ergoplatform.wallet.utils.WalletTestHelpers
import scorex.util.idToBytes

class TransactionBuilderSpec extends WalletTestHelpers with Matchers {
  import TransactionBuilder.buildUnsignedTx

  implicit val addressEncoder = new ErgoAddressEncoder(
    ErgoAddressEncoder.TestnetNetworkPrefix
  )

  val seedStr                       = "edge talent poet tortoise trumpet dose"
  val seed: Array[Byte]             = Mnemonic.toSeed(seedStr)
  val rootSecret: ExtendedSecretKey = ExtendedSecretKey.deriveMasterKey(seed)

  val currentHeight    = 0
  val minBoxValue      = BoxSelector.MinBoxValue
  val minChangeValue   = BoxSelector.MinBoxValue
  val minerRewardDelay = 720

  val TrueProp: SigmaPropValue = Values.TrueLeaf.toSigmaProp

  val tid1 = stringToId("t1")
  val tid2 = stringToId("t2")

  def box(value: Long) = testBox(value, TrueProp, currentHeight)

  def box(value: Long, tokens: Seq[(TokenId, Long)]) =
    testBox(value, TrueProp, currentHeight, tokens)

  def boxCandidate(value: Long) = new ErgoBoxCandidate(value, TrueProp, currentHeight)

  def boxCandidate(value: Long, tokens: Seq[(TokenId, Long)]) =
    new ErgoBoxCandidate(value, TrueProp, currentHeight, tokens.toColl)

  def transaction(inputBox: ErgoBox,
                  outBox: ErgoBoxCandidate,
                  burnTokens: TokensMap = Map.empty): Try[UnsignedErgoLikeTransaction] = {
    val ins = IndexedSeq(inputBox)
    val outs = IndexedSeq(outBox)
    val changeAddress = P2PKAddress(rootSecret.privateInput.publicImage)
    val fee = minBoxValue
    val res = buildUnsignedTx(
      inputs = ins,
      dataInputs = IndexedSeq(),
      outputCandidates = outs,
      currentHeight = currentHeight,
      feeAmount = fee,
      changeAddress = changeAddress,
      minChangeValue = minChangeValue,
      minerRewardDelay = minerRewardDelay,
      burnTokens = burnTokens
    )
    res
  }

  property("token minting") {
    val inputBox = box(minBoxValue * 2)
    val tokenId  = Digest32 @@ inputBox.id
    val outBox = boxCandidate(minBoxValue, Seq(tokenId -> 100L))
    val res = transaction(inputBox, outBox)

    res shouldBe a[Success[_]]
    val tx = res.get
    //  added miner fee
    tx.outputCandidates.size shouldBe 2
    tx.outputCandidates(0) shouldEqual outBox
  }

  property("token burning") {
    val inputBox = box(minBoxValue * 3, Seq(Digest32 @@ idToBytes(tid1) -> 1000L, Digest32 @@ idToBytes(tid2) -> 2000L))
    val tokenId  = Digest32 @@ inputBox.id
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
}
