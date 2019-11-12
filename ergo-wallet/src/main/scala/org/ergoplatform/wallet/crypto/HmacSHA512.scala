package org.ergoplatform.wallet.crypto

import javax.crypto.Mac
import javax.crypto.spec.SecretKeySpec

object HmacSHA512 {

  private val HashAlgo = "HmacSHA512"

  def hash(key: Array[Byte], data: Array[Byte]): Array[Byte] = initialize(key).doFinal(data)

  private def initialize(byteKey: Array[Byte]) = {
    val hmacSha512 = Mac.getInstance(HashAlgo)
    val keySpec = new SecretKeySpec(byteKey, HashAlgo)
    hmacSha512.init(keySpec)
    hmacSha512
  }

}
