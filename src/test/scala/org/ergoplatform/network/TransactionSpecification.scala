package org.ergoplatform.network

import java.nio.ByteBuffer

import org.ergoplatform.utils.ErgoPropertyTest
import scorex.util.encode.Base16

class TransactionSpecification extends ErgoPropertyTest with DecodingUtils {

  property("transaction specification parser") {
    // transaction bytes from CreateTransactionDemo.java example in ergo-wallet
    val txBytesEnc = "02430e80ca31a25400e77dac0ad14c1cd39cb09dc3f7c1c384dce9aef19b604e273827237ab5e42df16a327cb2e79ec0048aaeaf38020f9217f0a3b6fc4bda244170cfb528677c356ed4b07abf286186820c2261ca590049216700d9ba3ed2f55bc61ec90b7ee3949362a9a20fbf8514d2306eb97f14e07d2347973827237ab5e42df16a327cb2e79ec0048aaeaf38020f9217f0a3b6fc4bda244170cfb528677c356ed4b07abf286186820c2261ca590049216700000003c0f0f50b0008cd02699009600c1d6139d935f2101cba074eb02b86ca35205de67ed6c9d1e270ed97db81020000a09c010008cd03a09711da7508740313e1fc2db1fa853c7b7920a3d27d5027fed070241c1e2464db81020000c0843d10010101d17300db81020000"

    val txBytes = Base16.decode(txBytesEnc).get

    val bb = ByteBuffer.wrap(txBytes)

    val inputsCount = getULong(bb) // must be within  0..0xFFFF range
    inputsCount shouldBe 2
  }

}
