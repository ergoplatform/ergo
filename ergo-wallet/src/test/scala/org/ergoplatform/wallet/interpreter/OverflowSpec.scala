package org.ergoplatform.wallet.interpreter

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class OverflowSpec extends AnyFlatSpec with Matchers {

  "Integer arithmetic" should "overflow with Int" in {
    val storageFeeFactor: Int = 1250000
    val boxBytesLength: Int = 2239
    val result: Int = storageFeeFactor * boxBytesLength
    result should be (-1496217296) // The overflowed negative value
  }

  it should "not overflow with Long cast" in {
    val storageFeeFactor: Int = 1250000
    val boxBytesLength: Int = 2239
    val result: Long = storageFeeFactor.toLong * boxBytesLength
    result should be (2798750000L)
  }
}
