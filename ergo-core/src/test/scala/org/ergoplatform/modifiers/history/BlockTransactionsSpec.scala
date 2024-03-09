package org.ergoplatform.modifiers.history

import org.ergoplatform.utils.ErgoCorePropertyTest

class BlockTransactionsSpec extends ErgoCorePropertyTest {
  import org.ergoplatform.utils.generators.CoreObjectGenerators._
  import org.ergoplatform.utils.generators.ErgoCoreTransactionGenerators._

  property("Correct Merkle proofs are generated") {
    forAll(invalidBlockTransactionsGen, modifierIdGen){ case (bt, absentTx) =>
      // for all the transactions presented in a BlockTransactions instance valid proofs should be generated
      bt.transactions.forall{t => BlockTransactions.proofValid(bt.digest, bt.proofFor(t.id).get)} shouldBe true

      // no proof should be generated for a transaction which is not there
      bt.proofFor(absentTx).isDefined shouldBe false
    }
  }

}
