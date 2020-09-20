package org.ergoplatform.examples

import org.ergoplatform.ErgoBox.{R4, R5}
import org.ergoplatform.{ErgoAddressEncoder, ErgoBox}
import sigmastate.Values.{ByteArrayConstant, LongConstant}

object PowNft extends App {

  val enc = new ErgoAddressEncoder(ErgoAddressEncoder.MainnetNetworkPrefix)
  val pk = enc.fromString("9iHWcYYSPkgYbnC6aHfZcLZrKrrkpFzM2ETUZ2ikFqFwVAB2CU7").get.script
  
  val preimage = "kushti2020".getBytes("UTF-8")

  def nftGeneratorCandidate(nonce:Long): ErgoBox = {
    ErgoBox(1000000L, pk, creationHeight = 300000,
      additionalRegisters = Map(R4 -> ByteArrayConstant(preimage), R5 -> LongConstant(nonce)))
  }

  def mineNftGeneratorBox(target: BigInt): ErgoBox = {
    (0 to Int.MaxValue).foreach{nonce =>
      val b = nftGeneratorCandidate(nonce)
      val id = b.id
      println("nonce: " + nonce)
      val numId = BigInt(1, id)
      println(numId)
      if(numId < target) {
        println("winning nonce: " + nonce)
        return b
      }
    }
    ???
  }


  val target = BigInt(2).pow(232)
  val b = mineNftGeneratorBox(target)

  println(b.id.mkString("-"))
  println(b.id.head)
}
