package org.ergoplatform.examples

import org.ergoplatform.ErgoAddressEncoder
import org.ergoplatform.http.api.ApiCodecs
import org.ergoplatform.modifiers.mempool.{ErgoBoxSerializer, UnsignedErgoTransaction}
import scorex.crypto.authds.ADKey
import scorex.util.encode.Base16
import io.circe.syntax._
import scala.collection.IndexedSeq
import org.ergoplatform.DataInput
import org.ergoplatform.ErgoBoxCandidate
import org.ergoplatform.UnsignedInput
import sigmastate.eval.Extensions._
import sigmastate.eval._
import scorex.crypto.hash.Digest32



object OneWayStablecoin extends App with ApiCodecs {
  val enc = new ErgoAddressEncoder(ErgoAddressEncoder.MainnetNetworkPrefix)
  val creationHeight = 215592

  val usdTokenId: Digest32 = Digest32 @@ Base16.decode("6064c4288523cb1facc1ccd5654682f6f385f4fd00e36ca514ae5f3a8c456869").get

  val gfBytes = Base16.decode("a08d0610090400040004000402040005c80104000e206064c4288523cb1facc1ccd5654682f6f385f4fd00e36ca514ae5f3a8c45686908cd03f00473f6e7dc871b879f44c3c215df14e46e2355bd7ba677d1e2a9aeb36a6bd5d1d805d601b2db6308a7730000d602b2a5730100d603b2db63087202730200d604b2a5730300d605b2db6501fe730400ededed90998c7201028c7203029dc172049de4c6720504057305938cb2db63087205730600017307ed938c7201018c72030193c27202c2a793c27204d07308b88e0d016064c4288523cb1facc1ccd5654682f6f385f4fd00e36ca514ae5f3a8c45686988270041738cb4aa9421acf4c649200f0cf04f909f5020bfc33fefc270d5ed079b462a00").get


  // 100000
  val tokenBox = ErgoBoxSerializer.parseBytes(gfBytes)
  val tokenBoxId = tokenBox.id

  // 53866449600 nanoErgs
  val aliceBoxId = Base16.decode("ae2d9083fc458b77cd63f282f96287b2713971658c02ca3fbb3bd749751db8df").get

  val inputs = IndexedSeq(new UnsignedInput(ADKey @@ tokenBoxId), new UnsignedInput(ADKey @@ aliceBoxId))

  // Alice buy 1 USD from the box and pays to Bob
  // 2020-04-26 01:00:12,260 [INFO] Ergo Price in USD: 0.141234271868
  // 2020-04-26 01:00:12,261 [INFO] Nanoergs per USD: 7080434421

  val tokenOut = new ErgoBoxCandidate(100000, tokenBox.ergoTree, creationHeight, Array(usdTokenId -> 4900L).toColl)

  val bobScript = enc.fromString("9iHWcYYSPkgYbnC6aHfZcLZrKrrkpFzM2ETUZ2ikFqFwVAB2CU7").get.script
  val bobOut = new ErgoBoxCandidate(7080434421L, bobScript, creationHeight)

  val aliceScript = enc.fromString("9gmNsqrqdSppLUBqg2UzREmmivgqh1r3jmNcLAc53hk3YCvAGWE").get.script
  val aliceOut = new ErgoBoxCandidate(46785015179L, aliceScript, creationHeight, Array(usdTokenId -> 100L).toColl)

  val feeScript = enc.fromString("2iHkR7CWvD1R4j1yZg5bkeDRQavjAaVPeTDFGGLZduHyfWMuYpmhHocX8GJoaieTx78FntzJbCBVL6rf96ocJoZdmWBL2fci7NqWgAirppPQmZ7fN9V6z13Ay6brPriBKYqLp1bT2Fk4FkFLCfdPpe").get.script
  val feeOut = new ErgoBoxCandidate(1000000, feeScript, creationHeight)

  val rateBoxId = Base16.decode("4f314e472e42eed5236ab6fdf45f827d21ca44d9ca8c8bc8f82d78b66fa1c9a5").get
  val dataInputs = IndexedSeq(DataInput(ADKey @@ rateBoxId))

  val outputs = IndexedSeq(tokenOut, bobOut, aliceOut, feeOut)

  val tx = UnsignedErgoTransaction(inputs, dataInputs, outputs)
  println(tx.asJson)

  val rate = 7080434421L / 100
  println(rate)

  println(7080434421L / rate)
}




