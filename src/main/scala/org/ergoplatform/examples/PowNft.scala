package org.ergoplatform.examples

import org.ergoplatform.ErgoBox.{R4, R5}
import org.ergoplatform.examples.OneWayStablecoin.enc
import org.ergoplatform.{ErgoAddressEncoder, ErgoBox}
import sigmastate.Values.{ByteArrayConstant, LongConstant}

object PowNft extends App {

  val enc = new ErgoAddressEncoder(ErgoAddressEncoder.MainnetNetworkPrefix)
  val pk = enc.fromString("9iHWcYYSPkgYbnC6aHfZcLZrKrrkpFzM2ETUZ2ikFqFwVAB2CU7").get.script


  val preimage = "kushti2020"

  def nftGeneratorCandidate(nonce:Long): ErgoBox = {
    ErgoBox(1000000L, pk, creationHeight = 300000,
      additionalRegisters = Map(R4 -> ByteArrayConstant(preimage), R5 -> LongConstant(nonce)))
  }

  def mineNftGeneratorBox(target: BigInt): ErgoBox = {
    (0 to Long.MaxValue).foreach{
      val b = nftGeneratorCandidate(nonce)
      val id = b.id
      if(BigInt(1, id) < target) return b
    }
    ???
  }

  mineNftGeneratorBox(BigInt.apply(1, ))
}
