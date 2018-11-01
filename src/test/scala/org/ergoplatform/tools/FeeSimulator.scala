package org.ergoplatform.tools

import org.ergoplatform.{ErgoBoxCandidate, Input}
import org.ergoplatform.modifiers.mempool.ErgoTransaction
import scapi.sigma.DLogProtocol.DLogProverInput
import scorex.crypto.authds.ADKey
import scorex.crypto.hash.Digest32
import sigmastate.interpreter.{ContextExtension, ProverResult}

object FeeSimulator extends App {
  import org.ergoplatform.settings.Parameters._
  import org.ergoplatform.settings.Constants._

  //standard output size is about 80 bytes
  private val StdSize = 80

  println(s"K: $K")

  lazy val perOutputFee = StdSize * K  / CoinsInOneErgo.toDouble
  println(s"Storage fee for ordinary output: $perOutputFee")

  val bitcoinUtxos = 60000000
  println(s"Reward per block: ${perOutputFee * (bitcoinUtxos / StoragePeriod)}")

  val minStdDust = MinValuePerByte * StdSize
  println(s"Min dust value of standard-size box: $minStdDust")


  //Now assume that a miner is requiring upfront payment equal to storage fee to move a box
  val k1 = DLogProverInput.random().publicImage
  val k2 = DLogProverInput.random().publicImage

  val input = Input(ADKey @@ Array.fill(32)(0:Byte), ProverResult(Array.fill(65)(0: Byte), ContextExtension(Map())))

  val box1 = new ErgoBoxCandidate(1L, k1, Seq((Digest32 @@ Array.fill(32)(0:Byte)) -> 0L))
  val box2 = new ErgoBoxCandidate(100L, k2)

  val simpleTx = ErgoTransaction(IndexedSeq(input, input), IndexedSeq(box1, box2), None)

  val outputsSize = simpleTx.outputs.map(_.bytes.length).sum

  val minTxFee = K * outputsSize / CoinsInOneErgo.toDouble
  println(s"Min tx fee: $minTxFee")

  println("Four years storage fee: ")
  println(s"Everyday relocation: ${minTxFee * StoragePeriod / BlocksPerDay}")
  println(s"Weekly relocation: ${minTxFee * StoragePeriod / BlocksPerWeek}")
  println(s"Monthly relocation: ${minTxFee * StoragePeriod / BlocksPerMonth}")
  println(s"Yearly relocation: ${minTxFee * StoragePeriod / BlocksPerYear}")

  println("=====================")


  //Assume tx byte in Ergo has the same cost as in Bitcoin (not very realistic, as we take current market price of
  // Ergo token, but it will be much higher probably under the assumption of equality)
  val byteFeeBitcoin = 0.00039128734 * CoinsInOneErgo
  println(s"bytefee Bitcoin: $byteFeeBitcoin")
  val simpleTxSize = simpleTx.bytes.length
  println(s"Simple tx size: $simpleTxSize")
  val txsBlock = MaxBlockSize / simpleTxSize
  println(s"Transactions in a block: $txsBlock")
  val blockFee = MaxBlockSize * byteFeeBitcoin
  println(s"Block fee: ${blockFee / CoinsInOneErgo.toDouble}")

  println("=====================")


  //et's assume that a miner is requiring minimum tx fee proportional to lifetime of its inputs (as a part of
  // StoragePeriod = 4 years). Mean lifetime of a box in Bitcoin = 8182 blocks. Assume average tx which has 2 inputs
  // of mean lifetime and creating 2 outputs, then:

  val LBitcoin = 8182

  val meanMinTxFee = 2 * StdSize * K * LBitcoin / StoragePeriod.toDouble

  println(meanMinTxFee / CoinsInOneErgo)
  println(meanMinTxFee * txsBlock / CoinsInOneErgo)
}
