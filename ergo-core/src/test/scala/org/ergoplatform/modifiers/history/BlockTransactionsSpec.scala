package org.ergoplatform.modifiers.history

import org.ergoplatform.utils.ErgoCorePropertyTest
import org.ergoplatform.{ErgoBox, ErgoBoxCandidate, Input}
import org.ergoplatform.modifiers.history.header.Header
import org.ergoplatform.modifiers.mempool.{ErgoTransaction, ErgoTransactionSerializer}
import scorex.util.{ByteArrayBuilder, idToBytes}
import scorex.util.serialization.VLQByteBufferWriter
import sigma.VersionContext
import sigma.ast.{Constant, ErgoTree, SInt, SOption}
import sigma.data.TrivialProp.TrueProp
import sigma.interpreter.ProverResult

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

  // Original section writer, retained as a byte-level oracle for the factoring used by mining size accounting.
  private def originalBytes(bt: BlockTransactions): Array[Byte] = {
    val w = new VLQByteBufferWriter(new ByteArrayBuilder())
    w.putBytes(idToBytes(bt.headerId))
    val blockVersion = bt.blockVersion
    if (blockVersion > 1) {
      w.putUInt(10000000L + bt.blockVersion)
    }
    w.putUInt(bt.txs.size.toLong)
    bt.txs.foreach { tx =>
      if (blockVersion >= VersionContext.V6SoftForkVersion) {
        VersionContext.withVersions(blockVersion, blockVersion) {
          ErgoTransactionSerializer.serialize(tx, w)
        }
      } else {
        ErgoTransactionSerializer.serialize(tx, w)
      }
    }
    w.result().toBytes
  }

  private def transaction(index: Int, versionedRegister: Boolean, treeVersion: Byte = 0): ErgoTransaction = {
    val registers: ErgoBox.AdditionalRegisters = if (versionedRegister) {
      Map(ErgoBox.R4 -> Constant[SOption[SInt.type]](Some(index), SOption(SInt)))
    } else {
      Map(ErgoBox.R4 -> Constant[SInt.type](index, SInt))
    }
    val tree = ErgoTree.fromSigmaBoolean(ErgoTree.defaultHeaderWithVersion(treeVersion), TrueProp)
    val box = new ErgoBoxCandidate(10000000L, tree, index)
      .toBox(Header.GenesisParentId, 0.toShort)
    ErgoTransaction(IndexedSeq(Input(box.id, ProverResult.empty)), IndexedSeq.empty,
      IndexedSeq(new ErgoBoxCandidate(box.value, box.ergoTree, index, additionalRegisters = registers)))
  }

  property("section sizing preserves original bytes across versions and count framing") {
    for (version <- Seq[Byte](1, 2, 3, 4); count <- Seq(1, 127, 128)) {
      val txs = (0 until count).map(i => transaction(i, versionedRegister = false))
      val section = BlockTransactions(Header.GenesisParentId, version, txs)
      val original = originalBytes(section)
      section.bytes shouldBe original
      val payloadSize = txs.map(BlockTransactionsSerializer.transactionSize(_, version).toLong).sum
      BlockTransactionsSerializer.sectionSize(version, count, payloadSize) shouldBe original.length.toLong
      BlockTransactions.sizeOf(txs, version) shouldBe original.length
      val parsed = BlockTransactionsSerializer.parseBytesTry(original).get
      parsed.blockVersion shouldBe version
      parsed.txs.map(_.id) shouldBe txs.map(_.id)
      parsed.bytes shouldBe original
    }
  }

  property("section sizing uses the embedded version context for register values") {
    val tx = VersionContext.withVersions(3, 3) {
      transaction(1, versionedRegister = true)
    }
    for (version <- Seq[Byte](3, 4)) {
      val section = BlockTransactions(Header.GenesisParentId, version, Seq(tx))
      val original = originalBytes(section)
      section.bytes shouldBe original
      val payloadSize = BlockTransactionsSerializer.transactionSize(tx, version)
      BlockTransactionsSerializer.sectionSize(version, 1, payloadSize) shouldBe original.length.toLong
      BlockTransactions.sizeOf(Seq(tx), version) shouldBe original.length
    }
  }

  property("version four tree and register values round trip through the activated parser") {
    val tx = VersionContext.withVersions(3, 3) { transaction(1, versionedRegister = false, treeVersion = 3) }
    val section = BlockTransactions(Header.GenesisParentId, 4.toByte, Seq(tx))
    val parsed = BlockTransactionsSerializer.parseBytesTry(section.bytes).get
    parsed.blockVersion shouldBe 4.toByte
    parsed.txs.head.outputCandidates.head.additionalRegisters shouldBe tx.outputCandidates.head.additionalRegisters
    parsed.bytes shouldBe section.bytes
  }

  property("version four still rejects V6-only types in registers") {
    val tx = VersionContext.withVersions(3, 3) { transaction(1, versionedRegister = true, treeVersion = 3) }
    val section = BlockTransactions(Header.GenesisParentId, 4.toByte, Seq(tx))
    BlockTransactionsSerializer.parseBytesTry(section.bytes).isFailure shouldBe true
  }

  property("section sizing does not force an uncached standalone size in the default version context") {
    val tx = VersionContext.withVersions(3, 3) { transaction(2, versionedRegister = true) }
    scala.util.Try(tx.size).isFailure shouldBe true
    BlockTransactions.sizeOf(Seq(tx), 4.toByte) shouldBe originalBytes(
      BlockTransactions(Header.GenesisParentId, 4.toByte, Seq(tx))).length
  }

}
