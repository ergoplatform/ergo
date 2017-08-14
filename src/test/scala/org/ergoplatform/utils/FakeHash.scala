package org.ergoplatform.utils

import com.google.common.primitives.Ints
import scorex.crypto.hash.CryptographicHash32

import scala.util.Random


class FakeHash(minZeroBits: Int) extends CryptographicHash32 {
  override def hash(input: Message): Digest = {
    val seedBytesUnsafe = if(input.length <= 4) input else input.take(4)
    val seedBytes = Array.fill(4 - seedBytesUnsafe.length)(0 : Byte) ++ seedBytesUnsafe

    val seed = Ints.fromByteArray(seedBytes)
    val data = BigInt(256 - minZeroBits, new Random(seed)).toByteArray
    val zeroBytesCnt = 32 - data.length
    Array.fill(zeroBytesCnt)(0:Byte) ++ data
  }
}


object FakeHashTester extends App {
  val fh = new FakeHash(64)
  println(fh.hash(Array.fill(4)(Random.nextInt(100).toByte)).mkString("-"))
}
