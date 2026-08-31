package org.ergoplatform.serialization

import org.ergoplatform.modifiers.mempool.{ErgoTransaction, ErgoTransactionSerializer}
import org.ergoplatform.settings.Constants.TrueTree
import org.ergoplatform.utils.ErgoCorePropertyTest
import org.ergoplatform.{ErgoBoxCandidate, Input}
import scorex.crypto.authds.ADKey
import scorex.util.encode.Base16
import sigma.ast.IntConstant
import sigma.interpreter.{ContextExtension, ProverResult}

import scala.util.Try

/**
  * Pins the sigma-state 6.0.6 change "reject negative-id vars in ContextExtension
  * deserializer" and documents its consensus impact.
  *
  * The wire format itself did not change: var ids are written as one raw byte via
  * signed `put(id)` (verified against the published sources of both 6.0.3 and 6.0.6).
  * What changed is the reader only: 6.0.6 adds an explicit
  * "Negative id of context extension variable" guard, so transactions carrying such
  * extensions now fail at parse time with a `SerializerException`.
  *
  * Before 6.0.6 the same transaction could still be serialized/parsed successfully;
  * negative ids were rejected later, at proving/verification time, with different
  * exceptions (e.g., `NegativeArraySizeException`, `ArrayIndexOutOfBoundsException`).
  * Those paths are covered by `ErgoTransactionSpec`. This spec adds the missing
  * serializer-level and transaction-parser-level coverage that directly answers the
  * review's confirm-item.
  *
  * Consensus impact: a hand-crafted transaction with a negative extension id could,
  * in principle, be mined by an un-upgraded node and rejected by upgraded nodes.
  * All known wallets/SDKs use small non-negative var ids, so no such transactions
  * are known; the change is a soft-fork-style tightening that is safe once the
  * majority of hashrate is upgraded.
  */
class ContextExtensionSpec extends ErgoCorePropertyTest {

  private val serializer = ContextExtension.serializer

  property("ContextExtension deserialization rejects negative var id") {
    // wire layout: [values count][id][serialized value]
    val bytes = serializer.toBytes(ContextExtension(Map(56.toByte -> IntConstant(0))))
    bytes(1) = 0xC8.toByte // id 56 -> -56 as signed byte
    val parsed = Try(serializer.fromBytes(bytes))
    parsed.isFailure shouldBe true
    parsed.failed.get.getMessage.contains("Negative id") shouldBe true
  }

  property("ContextExtension serialization writes var id byte raw") {
    // pins that the rejected wire bytes are producible by ordinary node code;
    // the writer does not range-check the id
    val bytes = serializer.toBytes(ContextExtension(Map((-56).toByte -> IntConstant(0))))
    bytes(1) shouldBe 0xC8.toByte
  }

  property("ContextExtension valid ids round-trip") {
    Seq(0.toByte, 127.toByte).foreach { id =>
      val ce = ContextExtension(Map(id -> IntConstant(1)))
      serializer.fromBytes(serializer.toBytes(ce)) shouldBe ce
    }
  }

  property("ErgoTransaction parsing rejects negative context extension var id") {
    val boxId = ADKey @@ Base16.decode("c95c2ccf55e03cac6659f71ca4df832d28e2375569cec178dcb17f3e2e5f7742").get
    val input = Input(
      boxId,
      ProverResult(Array.emptyByteArray, ContextExtension(Map((-10).toByte -> IntConstant(0))))
    )
    val output = new ErgoBoxCandidate(1000000000L, TrueTree, 0)
    val tx = ErgoTransaction(IndexedSeq(input), IndexedSeq.empty, IndexedSeq(output))

    val bytes = ErgoTransactionSerializer.toBytes(tx)
    val parsed = Try(ErgoTransactionSerializer.parseBytes(bytes))
    parsed.isFailure shouldBe true
    parsed.failed.get.getMessage.contains("Negative id") shouldBe true
  }

}
