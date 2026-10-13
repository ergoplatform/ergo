package org.ergoplatform.network

import org.ergoplatform.mining.InputBlockFields
import org.ergoplatform.modifiers.mempool.ErgoTransaction
import org.ergoplatform.network.message.inputblocks.{
  InputBlockMessageSpec,
  InputBlockTransactionIdsData,
  InputBlockTransactionIdsMessageSpec,
  InputBlockTransactionsData,
  InputBlockTransactionsMessageSpec,
  InputBlockTransactionsRequest,
  InputBlockTransactionsRequestMessageSpec
}
import org.ergoplatform.settings.Constants
import org.ergoplatform.subblocks.InputBlockAnnouncement
import org.ergoplatform.utils.{ErgoCorePropertyTest, SerializationTests}
import org.scalacheck.Gen
import scorex.crypto.authds.merkle.BatchMerkleProof
import scorex.crypto.hash.Blake2b256
import scorex.util.{ByteArrayBuilder, ModifierId, idToBytes}
import scorex.util.serialization.VLQByteBufferWriter

import scala.util.{Failure, Try}

class InputBlockMessageSpecsSpec extends ErgoCorePropertyTest with SerializationTests {
  import org.ergoplatform.utils.generators.CoreObjectGenerators._
  import org.ergoplatform.utils.generators.ErgoCoreGenerators._
  import org.ergoplatform.utils.generators.ErgoCoreTransactionGenerators._

  private val inputBlockMessageSpec = InputBlockMessageSpec
  private val inputBlockTransactionIdsMessageSpec = InputBlockTransactionIdsMessageSpec
  private val inputBlockTransactionsMessageSpec = InputBlockTransactionsMessageSpec
  private val inputBlockTransactionsRequestMessageSpec = InputBlockTransactionsRequestMessageSpec

  private def inputBlockInfoGen: Gen[InputBlockAnnouncement] = for {
    header <- defaultHeaderGen
    prevInputBlockId <- Gen.option(genBytes(Constants.ModifierIdSize))
    transactionsDigest <- digest32Gen
    prevTransactionsDigest <- digest32Gen
    weakTxIds <- Gen.option(Gen.listOf(genBytes(ErgoTransaction.WeakIdLength)).map(_.take(5)))
  } yield {
    val merkleProof = BatchMerkleProof(Seq.empty, Seq.empty)(Blake2b256)
    val inputBlockFields = new InputBlockFields(prevInputBlockId, transactionsDigest, prevTransactionsDigest, merkleProof)
    InputBlockAnnouncement(InputBlockAnnouncement.initialMessageVersion, header, inputBlockFields, weakTxIds)
  }

  private def inputBlockTransactionIdsDataGen: Gen[InputBlockTransactionIdsData] = for {
    inputBlockId <- modifierIdGen
    transactionIds <- Gen.listOf(genBytes(ErgoTransaction.WeakIdLength)).map(_.take(5))
  } yield InputBlockTransactionIdsData(inputBlockId, transactionIds)

  private def inputBlockTransactionsDataGen: Gen[InputBlockTransactionsData] = for {
    inputBlockId <- modifierIdGen
    transactions <- Gen.listOf(invalidErgoTransactionGen).map(_.take(3))
  } yield InputBlockTransactionsData(inputBlockId, transactions)

  private def inputBlockTransactionsRequestGen: Gen[InputBlockTransactionsRequest] = for {
    inputBlockId <- modifierIdGen
    txIds <- Gen.listOf(genBytes(ErgoTransaction.WeakIdLength)).map(_.take(5))
  } yield InputBlockTransactionsRequest(inputBlockId, txIds)

  /**
    * Builds a raw message payload with an arbitrary declared count, so that counts which do not
    * match the actual payload can be fed to the parsers.
    */
  private def payloadWithDeclaredCount(inputBlockId: ModifierId,
                                       declaredCount: Long,
                                       payload: Array[Byte]): Array[Byte] = {
    val w = new VLQByteBufferWriter(new ByteArrayBuilder())
    w.putBytes(idToBytes(inputBlockId))
    w.putUInt(declaredCount)
    w.putBytes(payload)
    w.result().toBytes
  }

  private def weakIds(n: Int): Array[Byte] =
    Array.tabulate(n * ErgoTransaction.WeakIdLength)(i => (i % 256).toByte)

  /**
    * True iff parsing was rejected by the structural count bound (an `IllegalArgumentException`
    * thrown by `require` before any allocation or per-element reading is done).
    */
  private def rejectedByCountBound(res: Try[_]): Boolean = res match {
    case Failure(e: IllegalArgumentException) =>
      Option(e.getMessage).exists(_.contains("Too many transaction"))
    case _ =>
      false
  }

  property("InputBlockAnnouncement serialization roundtrip") {
    forAll(inputBlockInfoGen) { info =>
      val bytes = inputBlockMessageSpec.toBytes(info)
      val recovered = inputBlockMessageSpec.parseBytes(bytes)

      recovered.version shouldEqual info.version
      recovered.header shouldEqual info.header
      recovered.prevInputBlockId.map(_.toSeq) shouldEqual info.prevInputBlockId.map(_.toSeq)
      recovered.transactionsDigest.toSeq shouldEqual info.transactionsDigest.toSeq
      recovered.weakTxIds.map(_.map(_.toSeq)) shouldEqual info.weakTxIds.map(_.map(_.toSeq))
    }
  }

  property("InputBlockTransactionIdsData serialization roundtrip") {
    forAll(inputBlockTransactionIdsDataGen) { data =>
      val bytes = inputBlockTransactionIdsMessageSpec.toBytes(data)
      val recovered = inputBlockTransactionIdsMessageSpec.parseBytes(bytes)

      recovered.inputBlockId shouldEqual data.inputBlockId
      recovered.transactionIds.map(_.toSeq) shouldEqual data.transactionIds.map(_.toSeq)
    }
  }

  property("InputBlockTransactionIdsData serialization with empty transaction ids") {
    forAll(modifierIdGen) { inputBlockId =>
      val emptyData = InputBlockTransactionIdsData(inputBlockId, Seq.empty)
      val bytes = inputBlockTransactionIdsMessageSpec.toBytes(emptyData)
      val recovered = inputBlockTransactionIdsMessageSpec.parseBytes(bytes)

      recovered.inputBlockId shouldEqual emptyData.inputBlockId
      recovered.transactionIds shouldEqual emptyData.transactionIds
    }
  }

  property("InputBlockTransactionsData serialization roundtrip") {
    forAll(inputBlockTransactionsDataGen) { data =>
      val bytes = inputBlockTransactionsMessageSpec.toBytes(data)
      val recovered = inputBlockTransactionsMessageSpec.parseBytes(bytes)

      recovered.inputBlockId shouldEqual data.inputBlockId
      recovered.transactions shouldEqual data.transactions
    }
  }

  property("InputBlockTransactionsData serialization with empty transactions") {
    forAll(modifierIdGen) { inputBlockId =>
      val emptyData = InputBlockTransactionsData(inputBlockId, Seq.empty)
      val bytes = inputBlockTransactionsMessageSpec.toBytes(emptyData)
      val recovered = inputBlockTransactionsMessageSpec.parseBytes(bytes)

      recovered.inputBlockId shouldEqual emptyData.inputBlockId
      recovered.transactions shouldEqual emptyData.transactions
    }
  }

  property("InputBlockTransactionsRequest serialization roundtrip") {
    forAll(inputBlockTransactionsRequestGen) { request =>
      val bytes = inputBlockTransactionsRequestMessageSpec.toBytes(request)
      val recovered = inputBlockTransactionsRequestMessageSpec.parseBytes(bytes)

      recovered.inputBlockId shouldEqual request.inputBlockId
      recovered.txIds.map(_.toSeq) shouldEqual request.txIds.map(_.toSeq)
    }
  }

  property("InputBlockTransactionsRequest serialization with empty tx ids") {
    forAll(modifierIdGen) { inputBlockId =>
      val emptyRequest = InputBlockTransactionsRequest(inputBlockId, Seq.empty)
      val bytes = inputBlockTransactionsRequestMessageSpec.toBytes(emptyRequest)
      val recovered = inputBlockTransactionsRequestMessageSpec.parseBytes(bytes)

      recovered.inputBlockId shouldEqual emptyRequest.inputBlockId
      recovered.txIds shouldEqual emptyRequest.txIds
    }
  }

  property("InputBlock hardcoded test vectors") {
    // Test InputBlockTransactionIdsData with various scenarios
    val blockId = modifierIdGen.sample.get
    
    // Empty transaction IDs
    val emptyTxIdsData = InputBlockTransactionIdsData(blockId, Seq.empty)
    val emptyTxIdsBytes = inputBlockTransactionIdsMessageSpec.toBytes(emptyTxIdsData)
    val emptyTxIdsRecovered = inputBlockTransactionIdsMessageSpec.parseBytes(emptyTxIdsBytes)
    
    emptyTxIdsRecovered.inputBlockId shouldEqual emptyTxIdsData.inputBlockId
    emptyTxIdsRecovered.transactionIds shouldBe empty

    // Single transaction ID
    val singleTxId = Array.fill(ErgoTransaction.WeakIdLength)(1.toByte)
    val singleTxIdsData = InputBlockTransactionIdsData(blockId, Seq(singleTxId))
    val singleTxIdsBytes = inputBlockTransactionIdsMessageSpec.toBytes(singleTxIdsData)
    val singleTxIdsRecovered = inputBlockTransactionIdsMessageSpec.parseBytes(singleTxIdsBytes)
    
    singleTxIdsRecovered.inputBlockId shouldEqual singleTxIdsData.inputBlockId
    singleTxIdsRecovered.transactionIds.map(_.toSeq) shouldEqual singleTxIdsData.transactionIds.map(_.toSeq)

    // Multiple transaction IDs
    val multipleTxIds = Seq(
      Array.fill(ErgoTransaction.WeakIdLength)(1.toByte),
      Array.fill(ErgoTransaction.WeakIdLength)(2.toByte),
      Array.fill(ErgoTransaction.WeakIdLength)(3.toByte)
    )
    val multipleTxIdsData = InputBlockTransactionIdsData(blockId, multipleTxIds)
    val multipleTxIdsBytes = inputBlockTransactionIdsMessageSpec.toBytes(multipleTxIdsData)
    val multipleTxIdsRecovered = inputBlockTransactionIdsMessageSpec.parseBytes(multipleTxIdsBytes)
    
    multipleTxIdsRecovered.inputBlockId shouldEqual multipleTxIdsData.inputBlockId
    multipleTxIdsRecovered.transactionIds.map(_.toSeq) shouldEqual multipleTxIdsData.transactionIds.map(_.toSeq)

    // Test InputBlockTransactionsRequest scenarios
    // Empty request
    val emptyRequest = InputBlockTransactionsRequest(blockId, Seq.empty)
    val emptyRequestBytes = inputBlockTransactionsRequestMessageSpec.toBytes(emptyRequest)
    val emptyRequestRecovered = inputBlockTransactionsRequestMessageSpec.parseBytes(emptyRequestBytes)
    
    emptyRequestRecovered.inputBlockId shouldEqual emptyRequest.inputBlockId
    emptyRequestRecovered.txIds shouldBe empty

    // Single transaction ID request
    val singleRequest = InputBlockTransactionsRequest(blockId, Seq(singleTxId))
    val singleRequestBytes = inputBlockTransactionsRequestMessageSpec.toBytes(singleRequest)
    val singleRequestRecovered = inputBlockTransactionsRequestMessageSpec.parseBytes(singleRequestBytes)
    
    singleRequestRecovered.inputBlockId shouldEqual singleRequest.inputBlockId
    singleRequestRecovered.txIds.map(_.toSeq) shouldEqual singleRequest.txIds.map(_.toSeq)

    // Multiple transaction IDs request
    val multipleRequest = InputBlockTransactionsRequest(blockId, multipleTxIds)
    val multipleRequestBytes = inputBlockTransactionsRequestMessageSpec.toBytes(multipleRequest)
    val multipleRequestRecovered = inputBlockTransactionsRequestMessageSpec.parseBytes(multipleRequestBytes)
    
    multipleRequestRecovered.inputBlockId shouldEqual multipleRequest.inputBlockId
    multipleRequestRecovered.txIds.map(_.toSeq) shouldEqual multipleRequest.txIds.map(_.toSeq)

    // Test InputBlockTransactionsData scenarios
    val transaction = invalidErgoTransactionGen.sample.get
    
    // Empty transactions
    val emptyTransactionsData = InputBlockTransactionsData(blockId, Seq.empty)
    val emptyTransactionsBytes = inputBlockTransactionsMessageSpec.toBytes(emptyTransactionsData)
    val emptyTransactionsRecovered = inputBlockTransactionsMessageSpec.parseBytes(emptyTransactionsBytes)
    
    emptyTransactionsRecovered.inputBlockId shouldEqual emptyTransactionsData.inputBlockId
    emptyTransactionsRecovered.transactions shouldBe empty

    // Single transaction
    val singleTransactionData = InputBlockTransactionsData(blockId, Seq(transaction))
    val singleTransactionBytes = inputBlockTransactionsMessageSpec.toBytes(singleTransactionData)
    val singleTransactionRecovered = inputBlockTransactionsMessageSpec.parseBytes(singleTransactionBytes)
    
    singleTransactionRecovered.inputBlockId shouldEqual singleTransactionData.inputBlockId
    singleTransactionRecovered.transactions shouldEqual singleTransactionData.transactions

    // Verify serialized bytes have expected structure and size relationships
    emptyTxIdsBytes should not be empty
    singleTxIdsBytes.length should be > emptyTxIdsBytes.length
    multipleTxIdsBytes.length should be > singleTxIdsBytes.length
    
    emptyRequestBytes should not be empty
    singleRequestBytes.length should be > emptyRequestBytes.length
    multipleRequestBytes.length should be > singleRequestBytes.length
    
    emptyTransactionsBytes should not be empty
    singleTransactionBytes.length should be > emptyTransactionsBytes.length

    // Test roundtrip consistency
    val emptyTxIdsBytes2 = inputBlockTransactionIdsMessageSpec.toBytes(emptyTxIdsData)
    emptyTxIdsBytes shouldEqual emptyTxIdsBytes2
    
    val emptyRequestBytes2 = inputBlockTransactionsRequestMessageSpec.toBytes(emptyRequest)
    emptyRequestBytes shouldEqual emptyRequestBytes2

    // Test edge case: maximum allowed transaction IDs (within reasonable limits)
    val maxTxIds = Seq.fill(10)(Array.fill(ErgoTransaction.WeakIdLength)(255.toByte))
    val maxTxIdsData = InputBlockTransactionIdsData(blockId, maxTxIds)
    val maxTxIdsBytes = inputBlockTransactionIdsMessageSpec.toBytes(maxTxIdsData)
    val maxTxIdsRecovered = inputBlockTransactionIdsMessageSpec.parseBytes(maxTxIdsBytes)
    
    maxTxIdsRecovered.inputBlockId shouldEqual maxTxIdsData.inputBlockId
    maxTxIdsRecovered.transactionIds.map(_.toSeq) shouldEqual maxTxIdsData.transactionIds.map(_.toSeq)

    // Test edge case: transaction IDs with all zeros
    val zeroTxId = Array.fill(ErgoTransaction.WeakIdLength)(0.toByte)
    val zeroTxIdsData = InputBlockTransactionIdsData(blockId, Seq(zeroTxId))
    val zeroTxIdsBytes = inputBlockTransactionIdsMessageSpec.toBytes(zeroTxIdsData)
    val zeroTxIdsRecovered = inputBlockTransactionIdsMessageSpec.parseBytes(zeroTxIdsBytes)
    
    zeroTxIdsRecovered.inputBlockId shouldEqual zeroTxIdsData.inputBlockId
    zeroTxIdsRecovered.transactionIds.map(_.toSeq) shouldEqual zeroTxIdsData.transactionIds.map(_.toSeq)
  }

  //
  // Structural bounds on untrusted counts (messages 102, 104, 105)
  //

  property("InputBlockTransactionsData parsing rejects tx count exceeding remaining bytes") {
    forAll(modifierIdGen) { blockId =>
      // a 39 byte payload (about 52 bytes once framed as a P2P message), but a million
      // transactions declared
      val bytes = payloadWithDeclaredCount(blockId, 1000000L, Array.fill(4)(0.toByte))
      bytes.length should be < 64

      val res = inputBlockTransactionsMessageSpec.parseBytesTry(bytes)
      rejectedByCountBound(res) shouldBe true
    }
  }

  property("InputBlockTransactionsData parsing rejects Int.MaxValue tx count without allocating") {
    forAll(modifierIdGen) { blockId =>
      val bytes = payloadWithDeclaredCount(blockId, Int.MaxValue.toLong, Array.fill(4)(0.toByte))

      // An Array[ErgoTransaction] of Int.MaxValue elements needs gigabytes of heap, so had the
      // allocation been attempted this would have died with a (fatal, uncatchable by Try)
      // OutOfMemoryError instead of returning a Failure. Getting the require message back proves
      // the parser bailed out before `new Array[ErgoTransaction](txsCount)`.
      val res = inputBlockTransactionsMessageSpec.parseBytesTry(bytes)
      rejectedByCountBound(res) shouldBe true
    }
  }

  property("InputBlockTransactionsData parsing accepts tx count equal to remaining bytes, rejects one above") {
    forAll(modifierIdGen) { blockId =>
      val payload = Array.fill(8)(0.toByte)

      // One byte per transaction is a deliberately conservative lower bound, not a tight one:
      // ErgoTransactionSerializer delegates to ErgoLikeTransactionSerializer, which writes four
      // collection counts even for an empty transaction, so four bytes is the real minimum. The
      // guard only has to be sound, so at count == remaining it must not fire - whatever the
      // transaction parser then makes of the bytes
      val atBound = inputBlockTransactionsMessageSpec.parseBytesTry(
        payloadWithDeclaredCount(blockId, payload.length.toLong, payload))
      rejectedByCountBound(atBound) shouldBe false

      val aboveBound = inputBlockTransactionsMessageSpec.parseBytesTry(
        payloadWithDeclaredCount(blockId, payload.length.toLong + 1, payload))
      rejectedByCountBound(aboveBound) shouldBe true
    }
  }

  property("InputBlockTransactionsData with realistic tx count still roundtrips") {
    forAll(modifierIdGen, Gen.listOfN(3, invalidErgoTransactionGen)) { (blockId, txs) =>
      val data = InputBlockTransactionsData(blockId, txs)
      val recovered = inputBlockTransactionsMessageSpec.parseBytes(inputBlockTransactionsMessageSpec.toBytes(data))

      recovered.inputBlockId shouldEqual blockId
      recovered.transactions shouldEqual txs
    }
  }

  property("InputBlockTransactionsData with zero tx count parses to empty") {
    forAll(modifierIdGen) { blockId =>
      val recovered = inputBlockTransactionsMessageSpec.parseBytes(
        payloadWithDeclaredCount(blockId, 0L, Array.emptyByteArray))

      recovered.inputBlockId shouldEqual blockId
      recovered.transactions shouldBe empty
    }
  }

  property("InputBlockTransactionsRequest parsing rejects id count exceeding remaining bytes") {
    forAll(modifierIdGen) { blockId =>
      val bytes = payloadWithDeclaredCount(blockId, 1000000L, weakIds(2))
      bytes.length should be < 64

      val res = inputBlockTransactionsRequestMessageSpec.parseBytesTry(bytes)
      rejectedByCountBound(res) shouldBe true
    }
  }

  property("InputBlockTransactionsRequest parsing rejects Int.MaxValue id count safely") {
    forAll(modifierIdGen) { blockId =>
      val res = inputBlockTransactionsRequestMessageSpec.parseBytesTry(
        payloadWithDeclaredCount(blockId, Int.MaxValue.toLong, weakIds(2)))

      // Int.MaxValue * WeakIdLength overflows Int arithmetic, the check is done in Long
      rejectedByCountBound(res) shouldBe true
    }
  }

  property("InputBlockTransactionsRequest parsing accepts id count whose ids exactly fill the remaining bytes, rejects one above") {
    forAll(modifierIdGen) { blockId =>
      val payload = weakIds(5)

      val recovered = inputBlockTransactionsRequestMessageSpec.parseBytes(
        payloadWithDeclaredCount(blockId, 5L, payload))
      recovered.inputBlockId shouldEqual blockId
      recovered.txIds.length shouldBe 5

      val aboveBound = inputBlockTransactionsRequestMessageSpec.parseBytesTry(
        payloadWithDeclaredCount(blockId, 6L, payload))
      rejectedByCountBound(aboveBound) shouldBe true
    }
  }

  property("InputBlockTransactionsRequest with realistic id count still roundtrips") {
    forAll(modifierIdGen) { blockId =>
      val txIds = Seq.tabulate(5)(i => Array.fill(ErgoTransaction.WeakIdLength)(i.toByte))
      val request = InputBlockTransactionsRequest(blockId, txIds)
      val recovered = inputBlockTransactionsRequestMessageSpec.parseBytes(
        inputBlockTransactionsRequestMessageSpec.toBytes(request))

      recovered.inputBlockId shouldEqual blockId
      recovered.txIds.map(_.toSeq) shouldEqual txIds.map(_.toSeq)
    }
  }

  property("InputBlockTransactionsRequest with zero id count parses to empty") {
    forAll(modifierIdGen) { blockId =>
      val recovered = inputBlockTransactionsRequestMessageSpec.parseBytes(
        payloadWithDeclaredCount(blockId, 0L, Array.emptyByteArray))

      recovered.inputBlockId shouldEqual blockId
      recovered.txIds shouldBe empty
    }
  }

  property("InputBlockTransactionIdsData parsing rejects id count exceeding remaining bytes") {
    forAll(modifierIdGen) { blockId =>
      val bytes = payloadWithDeclaredCount(blockId, 1000000L, weakIds(2))
      bytes.length should be < 64

      val res = inputBlockTransactionIdsMessageSpec.parseBytesTry(bytes)
      rejectedByCountBound(res) shouldBe true
    }
  }

  property("InputBlockTransactionIdsData parsing rejects Int.MaxValue id count safely") {
    forAll(modifierIdGen) { blockId =>
      val res = inputBlockTransactionIdsMessageSpec.parseBytesTry(
        payloadWithDeclaredCount(blockId, Int.MaxValue.toLong, weakIds(2)))

      // Int.MaxValue * WeakIdLength overflows Int arithmetic, the check is done in Long
      rejectedByCountBound(res) shouldBe true
    }
  }

  property("InputBlockTransactionIdsData parsing accepts id count whose ids exactly fill the remaining bytes, rejects one above") {
    forAll(modifierIdGen) { blockId =>
      val payload = weakIds(5)

      val recovered = inputBlockTransactionIdsMessageSpec.parseBytes(
        payloadWithDeclaredCount(blockId, 5L, payload))
      recovered.inputBlockId shouldEqual blockId
      recovered.transactionIds.length shouldBe 5

      val aboveBound = inputBlockTransactionIdsMessageSpec.parseBytesTry(
        payloadWithDeclaredCount(blockId, 6L, payload))
      rejectedByCountBound(aboveBound) shouldBe true
    }
  }

  property("InputBlockTransactionIdsData with realistic id count still roundtrips") {
    forAll(modifierIdGen) { blockId =>
      val txIds = Seq.tabulate(5)(i => Array.fill(ErgoTransaction.WeakIdLength)(i.toByte))
      val data = InputBlockTransactionIdsData(blockId, txIds)
      val recovered = inputBlockTransactionIdsMessageSpec.parseBytes(
        inputBlockTransactionIdsMessageSpec.toBytes(data))

      recovered.inputBlockId shouldEqual blockId
      recovered.transactionIds.map(_.toSeq) shouldEqual txIds.map(_.toSeq)
    }
  }

  property("InputBlockTransactionIdsData with zero id count parses to empty") {
    forAll(modifierIdGen) { blockId =>
      val recovered = inputBlockTransactionIdsMessageSpec.parseBytes(
        payloadWithDeclaredCount(blockId, 0L, Array.emptyByteArray))

      recovered.inputBlockId shouldEqual blockId
      recovered.transactionIds shouldBe empty
    }
  }
}
