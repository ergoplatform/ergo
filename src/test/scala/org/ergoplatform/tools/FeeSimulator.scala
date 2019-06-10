package org.ergoplatform.tools

import org.ergoplatform.modifiers.mempool.ErgoTransaction
import org.ergoplatform.{ErgoBoxCandidate, Input}
import scorex.crypto.authds.ADKey
import scorex.crypto.hash.Digest32
import scorex.utils.Random
import sigmastate.basics.DLogProtocol.DLogProverInput
import sigmastate.interpreter.{ProverResult, ContextExtension}
import sigmastate.eval._
import sigmastate.eval.Extensions._

object FeeSimulator extends App {

  import org.ergoplatform.settings.LaunchParameters._
  import org.ergoplatform.settings.Constants._

  //Block size is 500Kb
  private val BlockSize = 500 * 1024

  val k1 = DLogProverInput.random().publicImage
  val k2 = DLogProverInput.random().publicImage

  val input = Input(ADKey @@ Random.randomBytes(32), ProverResult(Random.randomBytes(65), ContextExtension(Map())))
  val creationHeight: Int = 100000

  val box1 = new ErgoBoxCandidate(scala.util.Random.nextLong(), k1, creationHeight, Colls.fromItems((Digest32 @@ Random.randomBytes(32)) -> scala.util.Random.nextLong()))
  val box2 = new ErgoBoxCandidate(scala.util.Random.nextLong(), k2, creationHeight)

  val simpleTx = ErgoTransaction(IndexedSeq(input, input), IndexedSeq(box1, box2))
  val stdSize = simpleTx.outputs.map(_.bytes.length).sum / simpleTx.outputs.length
  val simpleTxSize = simpleTx.size
  val outputsSize = simpleTx.outputs.map(_.bytes.length).sum
  lazy val perOutputFee = stdSize * storageFeeFactor / CoinsInOneErgo.toDouble
  val minStdDust = minValuePerByte * stdSize
  val byteFeeBitcoin = 0.00039128734 * CoinsInOneErgo

  println("=====================")
  println("Global parameters:")
  println(s"Storage fee factor: $storageFeeFactor")
  println(s"Output size: $stdSize B")
  println(s"Simple tx size: $simpleTxSize B")
  println(s"Block size: $BlockSize B")
  println(s"Storage fee for ordinary output: $perOutputFee")
  println(s"Min dust value of standard-size box: $minStdDust")

  println("=====================")
  println(s"Assume that Ergo state have the same number of outputs($bitcoinUtxos), as Bitcoin.")

  def bitcoinUtxos = 60000000

  println(s"Reward per block: ${perOutputFee * (bitcoinUtxos.toDouble / StoragePeriod)} Erg + tx fees")


  println("=====================")
  println(s"Assume that blocks are full and miner is requiring upfront payment equal to storage fee to move a box")

  println(s"Reward per block: ${BlockSize * storageFeeFactor / CoinsInOneErgo.toDouble} Erg")

  val minTxFee = storageFeeFactor * outputsSize / CoinsInOneErgo.toDouble
  println(s"Tx fee: $minTxFee")

  println(s"Everyday relocation: ${minTxFee * StoragePeriod / BlocksPerDay}")
  println(s"Weekly relocation: ${minTxFee * StoragePeriod / BlocksPerWeek}")
  println(s"Monthly relocation: ${minTxFee * StoragePeriod / BlocksPerMonth}")
  println(s"Yearly relocation: ${minTxFee * StoragePeriod / BlocksPerYear}")

  println("=====================")
  println("Assume tx byte in Ergo has the same USD cost as in Bitcoin")

  //(not very realistic, as we take current market price of Ergo token, but it will be much higher probably under the assumption of equality)
  val blockFee = maxBlockSize * byteFeeBitcoin
  println(s"Reward per block: ${blockFee / CoinsInOneErgo.toDouble} Erg")
  println(s"bytefee Bitcoin: ${byteFeeBitcoin / CoinsInOneErgo.toDouble} Erg")


  println("=====================")
  println("Assume that a miner is requiring minimum tx fee proportional to lifetime of its inputs and average" +
    " box lifetime equals to Bitcoin (8182 blocks)")

  // Mean lifetime of a box in Bitcoin = 8182 blocks.
  val LBitcoin = 8182

  val meanMinTxFee = outputsSize * storageFeeFactor * LBitcoin / StoragePeriod.toDouble

  println(s"Reward per block: ${BlockSize * storageFeeFactor * LBitcoin / StoragePeriod.toDouble / CoinsInOneErgo.toDouble} Erg")

  println(s"Tx fee: ${meanMinTxFee / CoinsInOneErgo.toDouble}")

  println("=====================")
  println("Assume that a miner is requiring minimum tx fee proportional to lifetime of its inputs and average" +
    " box lifetime equals to Bitcoin (56,8 days)")

  // Mean lifetime of a box in Ergo = 56,8 days
  val LErgo = LBitcoin * 5

  val meanMinTxFeeE = outputsSize * storageFeeFactor * LErgo / StoragePeriod.toDouble

  println(s"Reward per block: ${BlockSize * storageFeeFactor * LErgo / StoragePeriod.toDouble  / CoinsInOneErgo.toDouble} Erg")

  println(s"Tx fee: ${meanMinTxFeeE / CoinsInOneErgo.toDouble}")

}
