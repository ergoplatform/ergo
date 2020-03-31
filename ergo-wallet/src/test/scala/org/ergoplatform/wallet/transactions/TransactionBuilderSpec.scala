package org.ergoplatform.wallet.transactions

import sigmastate.Values
import sigmastate.Values.SigmaPropValue
import sigmastate.eval._
import sigmastate.eval.Extensions._
import org.ergoplatform.{ErgoBox, ErgoBoxCandidate}

import org.scalatest.{Matchers, PropSpec}
import org.ergoplatform.ErgoBox.TokenId
import scala.util.Success
import scorex.crypto.hash.Digest32
import org.ergoplatform.P2PKAddress
import org.ergoplatform.wallet.mnemonic.Mnemonic
import org.ergoplatform.wallet.secrets.ExtendedSecretKey
import org.ergoplatform.ErgoAddressEncoder

class TransactionBuilderSpec extends PropSpec with Matchers {
  import TransactionBuilder.buildUnsignedTx

  implicit val addressEncoder = new ErgoAddressEncoder(
    ErgoAddressEncoder.TestnetNetworkPrefix
  )

  val seedStr                       = "edge talent poet tortoise trumpet dose"
  val seed: Array[Byte]             = Mnemonic.toSeed(seedStr)
  val rootSecret: ExtendedSecretKey = ExtendedSecretKey.deriveMasterKey(seed)

  val currentHeight    = 0
  val minFee           = 1000000L
  val minChangeValue   = 1000000L
  val minerRewardDelay = 720

  val TrueProp: SigmaPropValue = Values.TrueLeaf.toSigmaProp

  def box(value: Long) = ErgoBox(value, TrueProp, currentHeight)

  def box(value: Long, tokens: Seq[(TokenId, Long)]) =
    ErgoBox(value, TrueProp, currentHeight, tokens)

  def boxCandidate(value: Long) = new ErgoBoxCandidate(value, TrueProp, currentHeight)

  def boxCandidate(value: Long, tokens: Seq[(TokenId, Long)]) =
    new ErgoBoxCandidate(value, TrueProp, currentHeight, tokens.toColl)

  property("token minting") {
    val inputBox = box(minFee * 2)
    val tokenId  = Digest32 @@ inputBox.id
    val outBox = boxCandidate(minFee, Seq(tokenId -> 100L))

    val ins           = IndexedSeq(inputBox)
    val outs          = IndexedSeq(outBox)
    val changeAddress = P2PKAddress(rootSecret.key.publicImage)
    val fee           = minFee

    val res = buildUnsignedTx(
      inputs           = ins,
      dataInputs       = IndexedSeq(),
      outputCandidates = outs,
      currentHeight    = currentHeight,
      feeAmount        = fee,
      changeAddress    = changeAddress,
      minChangeValue   = minChangeValue,
      minerRewardDelay = minerRewardDelay
    )

    res shouldBe a[Success[_]]
    val tx = res.get
    //  added miner fee
    tx.outputCandidates.size shouldBe 2
    tx.outputCandidates(0) shouldEqual outBox
  }
}
