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
  val StdSize = 80

  println("K: " + K )

  lazy val perOutputFee = StdSize * K  / CoinsInOneErgo.toDouble
  println("Storage fee for ordinary output: " + perOutputFee)

  val bitcoinUtxos = 60000000
  println("Reward per block: " + perOutputFee * (bitcoinUtxos / StoragePeriod))

  val minStdDust = MinValuePerByte * StdSize
  println(s"Min dust value of standard-size box: $minStdDust")

  val k1 = DLogProverInput.random().publicImage
  val k2 = DLogProverInput.random().publicImage

  val input = Input(ADKey @@ Array.fill(32)(0:Byte), ProverResult(Array.fill(65)(0: Byte), ContextExtension(Map())))

  val box1 = new ErgoBoxCandidate(1L, k1, Seq((Digest32 @@ Array.fill(32)(0:Byte)) -> 0L))
  val box2 = new ErgoBoxCandidate(100L, k2)

  val simpleTx = ErgoTransaction(IndexedSeq(input), IndexedSeq(box1, box2), None)

  val outputsSize = simpleTx.outputs.map(_.bytes.length).sum

  val minTxFee = K * outputsSize / CoinsInOneErgo.toDouble
  println(s"Min tx fee: $minTxFee")

  println("Four years storage fee: ")
  println(s"Everyday relocation: ${minTxFee * StoragePeriod / BlocksPerDay}")
  println(s"Weekly relocation: ${minTxFee * StoragePeriod / BlocksPerWeek}")
  println(s"Monthly relocation: ${minTxFee * StoragePeriod / BlocksPerMonth}")
  println(s"Yearly relocation: ${minTxFee * StoragePeriod / BlocksPerYear}")
}
