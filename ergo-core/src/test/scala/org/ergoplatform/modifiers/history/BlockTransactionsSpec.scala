package org.ergoplatform.modifiers.history

import org.ergoplatform.modifiers.history.header.Header
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

  // The miner uses BlockTransactions.transactionsRoot (static) to fill header.transactionsRoot
  // before a BlockTransactions instance exists; later the instance is constructed and its
  // `digest` (= merkleTree.rootHash) must equal that value, otherwise blocks fail validation.
  property("transactionsRoot (static) matches BlockTransactions.digest for v1 (no witnesses)") {
    forAll(invalidBlockTransactionsGen) { bt =>
      BlockTransactions.transactionsRoot(bt.txs, Header.InitialVersion) shouldBe bt.digest
    }
  }

  property("transactionsRoot (static) matches BlockTransactions.digest for v2+ (with witnesses)") {
    forAll(invalidBlockTransactionsGen) { bt =>
      val withWitnesses = bt.copy(blockVersion = Header.HardeningVersion)
      BlockTransactions.transactionsRoot(withWitnesses.txs, Header.HardeningVersion) shouldBe withWitnesses.digest
    }
  }

}
