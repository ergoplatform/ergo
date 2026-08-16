package org.ergoplatform.modifiers.history

import org.ergoplatform.modifiers.history.header.Header
import org.ergoplatform.modifiers.mempool.ErgoTransaction
import org.ergoplatform.settings.Constants.TrueTree
import org.ergoplatform.utils.ErgoCorePropertyTest
import org.ergoplatform.{ErgoBoxCandidate, Input}
import scorex.crypto.authds.ADKey
import scorex.util.bytesToId

class BlockTransactionsSpec extends ErgoCorePropertyTest {
  import org.ergoplatform.utils.ErgoCoreTestConstants.emptyProverResult
  import org.ergoplatform.utils.generators.CoreObjectGenerators._
  import org.ergoplatform.utils.generators.ErgoCoreTransactionGenerators._

  private val fixtureTransactions = (1 to 5).map { seed =>
    ErgoTransaction(
      IndexedSeq(Input(ADKey @@ Array.fill[Byte](32)(seed.toByte), emptyProverResult)),
      IndexedSeq(new ErgoBoxCandidate(1000000L + seed, TrueTree, creationHeight = 0))
    )
  }

  private val headerId = bytesToId(Array.fill[Byte](32)(0.toByte))
  private val absentTxId = bytesToId(Array.fill[Byte](32)(Byte.MaxValue))

  property("Correct Merkle proofs are generated") {
    forAll(invalidBlockTransactionsGen, modifierIdGen){ case (bt, absentTx) =>
      // for all the transactions presented in a BlockTransactions instance valid proofs should be generated
      bt.transactions.forall{t => BlockTransactions.proofValid(bt.digest, bt.proofFor(t.id).get)} shouldBe true

      // no proof should be generated for a transaction which is not there
      bt.proofFor(absentTx).isDefined shouldBe false
    }
  }

  property("Merkle proofs bind requested transaction ids across tree sizes") {
    fixtureTransactions.map(_.id).distinct should have size 5
    fixtureTransactions.map(_.witnessSerializedId.toSeq).distinct should have size 1

    Seq(Header.InitialVersion, Header.HardeningVersion).foreach { blockVersion =>
      (1 to fixtureTransactions.size).foreach { transactionCount =>
        val txs = fixtureTransactions.take(transactionCount)
        val blockTransactions = BlockTransactions(headerId, blockVersion, txs)

        withClue(s"version=$blockVersion, transactionCount=$transactionCount: ") {
          blockTransactions.digest.sameElements(
            BlockTransactions.transactionsRoot(txs, blockVersion)
          ) shouldBe true

          blockTransactions.proofFor(absentTxId) shouldBe None

          txs.foreach { tx =>
            val proof = blockTransactions.proofFor(tx.id).get
            proof.leafData.sameElements(tx.serializedId) shouldBe true
            BlockTransactions.proofValid(blockTransactions.digest, proof) shouldBe true
          }
        }
      }
    }
  }
  property("Merkle proof returns the requested transaction when witness ids repeat") {
    val txs = fixtureTransactions.take(3)
    txs.map(_.id).distinct should have size 3
    txs.map(_.witnessSerializedId.toSeq).distinct should have size 1

    val blockTransactions = BlockTransactions(headerId, Header.HardeningVersion, txs)
    val requestedTx = txs.head
    val proof = blockTransactions.proofFor(requestedTx.id).get

    BlockTransactions.proofValid(blockTransactions.digest, proof) shouldBe true
    proof.leafData.sameElements(requestedTx.serializedId) shouldBe true
  }

}
