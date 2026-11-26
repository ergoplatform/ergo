package org.ergoplatform.modifiers.history

import org.ergoplatform.{ErgoBox, ErgoBoxCandidate, Input}
import org.ergoplatform.modifiers.history.header.Header
import org.ergoplatform.modifiers.mempool.ErgoTransaction
import org.ergoplatform.settings.Constants
import org.ergoplatform.utils.ErgoCorePropertyTest
import sigma.{Colls, VersionContext}
import sigma.ast.HeaderConstant
import sigma.serialization.SerializerException

import scala.collection.mutable.ArrayBuffer

class BlockTransactionsSpec extends ErgoCorePropertyTest {
  import org.ergoplatform.utils.generators.CoreObjectGenerators._
  import org.ergoplatform.utils.generators.ErgoCoreGenerators.defaultHeaderGen
  import org.ergoplatform.utils.generators.ErgoCoreTransactionGenerators._

  property("Correct Merkle proofs are generated") {
    forAll(invalidBlockTransactionsGen, modifierIdGen){ case (bt, absentTx) =>
      // for all the transactions presented in a BlockTransactions instance valid proofs should be generated
      bt.transactions.forall{t => BlockTransactions.proofValid(bt.digest, bt.proofFor(t.id).get)} shouldBe true

      // no proof should be generated for a transaction which is not there
      bt.proofFor(absentTx).isDefined shouldBe false
    }
  }

  property("BlockTransactionsSerializer.serialize matches parse VersionContext for v5.0 and v6.0 blocks") {
    val baseTx = invalidErgoTransactionGen.sample.get
    val header = defaultHeaderGen.sample.get.copy(version = Header.Interpreter60Version)
    val observed = ArrayBuffer.empty[VersionContext]
    val observingInputs: scala.collection.immutable.IndexedSeq[Input] =
      new scala.collection.immutable.IndexedSeq[Input] {
        override def length: Int = baseTx.inputs.length
        override def apply(idx: Int): Input = {
          observed += VersionContext.current
          baseTx.inputs(idx)
        }
      }
    val observingTx = ErgoTransaction(observingInputs, baseTx.dataInputs, baseTx.outputCandidates)

    // v5.0 block (blockVersion = 3 < Header.Interpreter60Version = 4): must keep outer default VersionContext
    val defaultCtx = VersionContext.current
    observed.clear()
    val v5Bt = BlockTransactions(header.id, Header.Interpreter50Version, Seq(observingTx))
    BlockTransactionsSerializer.toBytes(v5Bt) should not be empty
    observed should not be empty
    observed.distinct shouldBe Seq(defaultCtx)

    // v6.0 block (blockVersion = 4 == Header.Interpreter60Version): must use script/tree version (3, 3), not (4, 4)
    val expectedV6Ctx = Header.scriptAndTreeFromBlockVersions(Header.Interpreter60Version)
    observed.clear()
    val v6Bt = BlockTransactions(header.id, Header.Interpreter60Version, Seq(observingTx))
    BlockTransactionsSerializer.toBytes(v6Bt) should not be empty
    observed should not be empty
    observed.distinct shouldBe Seq(expectedV6Ctx)

    // v6-only SHeader constant in output register R4: rejected on v5.0 block (matching parse), serialized on v6.0
    // (parseBytes rejects SHeader in registers under CheckV6Type even on v6.0)
    val v6Out = new ErgoBoxCandidate(
      value = 1000000L,
      ergoTree = Constants.TrueTree,
      creationHeight = 1,
      additionalTokens = Colls.emptyColl,
      additionalRegisters = Map(ErgoBox.R4 -> HeaderConstant(Header.toSigma(header)))
    )
    val txWithHeaderReg = VersionContext.withVersions(expectedV6Ctx.activatedVersion, expectedV6Ctx.ergoTreeVersion) {
      ErgoTransaction(baseTx.inputs, baseTx.dataInputs, IndexedSeq(v6Out))
    }

    val v5HeaderBt = BlockTransactions(header.id, Header.Interpreter50Version, Seq(txWithHeaderReg))
    a[SerializerException] should be thrownBy BlockTransactionsSerializer.toBytes(v5HeaderBt)

    val v6HeaderBt = BlockTransactions(header.id, Header.Interpreter60Version, Seq(txWithHeaderReg))
    val v6HeaderBytes = BlockTransactionsSerializer.toBytes(v6HeaderBt)
    v6HeaderBytes should not be empty
    BlockTransactionsSerializer.parseBytesTry(v6HeaderBytes).isFailure shouldBe true

    // Valid v6.0 BlockTransactions round-trips cleanly under VersionContext(3, 3)
    val v6NormalBt = BlockTransactions(header.id, Header.Interpreter60Version, Seq(baseTx))
    val v6NormalBytes = BlockTransactionsSerializer.toBytes(v6NormalBt)
    val v6NormalParsed = BlockTransactionsSerializer.parseBytes(v6NormalBytes)
    v6NormalParsed.blockVersion shouldBe Header.Interpreter60Version
    v6NormalParsed.bytes shouldEqual v6NormalBytes
  }

}
