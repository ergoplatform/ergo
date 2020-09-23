package org.ergoplatform.examples

import org.ergoplatform.ErgoBox.{R4, R5}
import org.ergoplatform.{ErgoAddressEncoder, ErgoBoxCandidate, JsonCodecs, UnsignedInput}
import scorex.util.encode.Base16
import sigmastate.Values.{ByteArrayConstant, LongConstant}
import org.ergoplatform.modifiers.mempool.UnsignedErgoTransaction
import scorex.crypto.authds.ADKey
import sigmastate.eval._
import io.circe.syntax._
import org.ergoplatform.http.api.ApiCodecs

object PowNft extends App with ApiCodecs {

  val enc = new ErgoAddressEncoder(ErgoAddressEncoder.MainnetNetworkPrefix)
  val pk = enc.fromString("9iHWcYYSPkgYbnC6aHfZcLZrKrrkpFzM2ETUZ2ikFqFwVAB2CU7").get.script

  val preimage = "kushti2020-32bytes".getBytes("UTF-8")

  val inputBoxId = ADKey @@ Base16.decode("e6095b22bf504cdb2c74385eb6bbd49ab203a8402accde8d886ff5125a5003c0").get
  val input: UnsignedInput = new UnsignedInput(inputBoxId)


  // todo: generate transaction with the box
  def nftGeneratorCandidate(nonce:Long): UnsignedErgoTransaction = {
    val candidate = new ErgoBoxCandidate(1000000L, pk, 300000, Colls.emptyColl, Map(R4 -> ByteArrayConstant(preimage), R5 -> LongConstant(nonce)))
    new UnsignedErgoTransaction(IndexedSeq(input), IndexedSeq.empty, IndexedSeq(candidate))
  }

  def mineNftGeneratorBox(target: BigInt): (UnsignedErgoTransaction, Long) = {
    (0 to Int.MaxValue).foreach{nonce =>
      val utx = nftGeneratorCandidate(nonce)
      val box = utx.outputs.head
      val id = box.id
      println("nonce: " + nonce)
      val numId = BigInt(1, id)
      println(numId)
      if(numId < target) return utx -> nonce
    }
    ???
  }


  val target = BigInt(2).pow(224)
  val (utx, nonce) = mineNftGeneratorBox(target)

  println("Token stamp: " + new String(preimage, "UTF-8"))
  println("Worker stamp: " + nonce)
  println("Minted token id: " + Base16.encode(utx.outputs.head.id))
  println("Transaction: " + utx.asJson)

}
