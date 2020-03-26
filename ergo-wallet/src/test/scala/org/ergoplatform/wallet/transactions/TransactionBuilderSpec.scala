package org.ergoplatform.wallet.transactions

import sigmastate.Values
import sigmastate.Values.SigmaPropValue
import sigmastate.eval._
import sigmastate.eval.Extensions._
import org.ergoplatform.{ErgoBox, ErgoBoxCandidate}

import org.scalatest.{Matchers, PropSpec}
import org.ergoplatform.ErgoBox.TokenId
import scorex.crypto.hash.Blake2b256
import scala.util.Success
import scorex.crypto.hash.`package`.Digest32
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

  val TrueLeaf: SigmaPropValue = Values.TrueLeaf.toSigmaProp

  def box(value: Long) = ErgoBox(value, TrueLeaf, currentHeight)

  def box(value: Long, tokens: Seq[(TokenId, Long)]) =
    ErgoBox(value, TrueLeaf, currentHeight, tokens)

  def boxCandidate(value: Long) = new ErgoBoxCandidate(value, TrueLeaf, currentHeight)

  def boxCandidate(value: Long, tokens: Seq[(TokenId, Long)]) =
    new ErgoBoxCandidate(value, TrueLeaf, currentHeight, tokens.toColl)

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
      feeAmount        = fee,
      changeAddress    = Some(changeAddress),
      currentHeight    = currentHeight,
      minFee           = minFee,
      minChangeValue   = minChangeValue,
      minerRewardDelay = minerRewardDelay
    )

    res shouldBe a[Success[_]]
    val tx = res.get
    tx.outputCandidates.size shouldBe 2
    tx.outputCandidates(0) shouldEqual outBox
  }
}
