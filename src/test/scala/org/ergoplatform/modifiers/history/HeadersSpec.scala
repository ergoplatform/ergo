package org.ergoplatform.modifiers.history

import org.ergoplatform.utils.ErgoPropertyTest
import scorex.crypto.hash.Blake2b256
import scorex.util.ModifierId

class HeadersSpec extends ErgoPropertyTest {

  val chain: HeaderChain = genHeaderChain(50, diffBitsOpt = None, useRealTs = false)
  val genesisId: ModifierId = chain.head.id

  property("Any field change should lead to different id") {
    forAll(defaultHeaderGen) { header =>
      val initialId = header.id
      header.copy(version = (header.version + 1).toByte).id should not equal initialId
      header.copy(parentId = initialId).id should not equal initialId
      header.copy(ADProofsRoot = Blake2b256(header.ADProofsRoot)).id should not equal initialId
      header.copy(transactionsRoot = Blake2b256(header.transactionsRoot)).id should not equal initialId
      header.copy(timestamp = header.timestamp + 1).id should not equal initialId
      header.copy(nBits = header.nBits + 1).id should not equal initialId
      header.copy(height = header.height + 1).id should not equal initialId
      header.copy(extensionRoot = Blake2b256(header.extensionRoot)).id should not equal initialId
      header.copy(powSolution = header.powSolution.copy(d = header.powSolution.d + 1)).id should not equal initialId
    }
  }

}
