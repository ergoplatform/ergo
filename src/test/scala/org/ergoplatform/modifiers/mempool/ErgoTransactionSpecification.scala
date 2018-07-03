package org.ergoplatform.modifiers.mempool

import org.ergoplatform.nodeView.state.ErgoStateContext
import org.ergoplatform.utils.ErgoPropertyTest
import scorex.crypto.authds.ADDigest

class ErgoTransactionSpecification extends ErgoPropertyTest {

  property("a valid transaction is valid") {
    forAll(validErgoTransactionGen){case (from, tx) =>
      val context = ErgoStateContext(0, ADDigest @@ Array.fill(32)(0:Byte))

      tx.statelessValidity.isSuccess shouldBe true
      val res = tx.statefulValidity(from, context)
      println(res)
      res.isSuccess shouldBe true
    }
  }

  property("ergo preservation law holds") {

  }

  property("impossible to create a negative-value output") {

  }


  property("impossible to overflow ergo tokens") {

  }

  property("assets preservation law holds") {

  }

  property("impossible to create a negative-value asset") {

  }

  property("impossible to overflow an asset value") {

  }

  property("too costly transaction is rejected") {

  }

  property("output contains too many assets") {

  }
}