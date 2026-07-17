package org.ergoplatform.network

import org.ergoplatform.mining.InputBlockFields
import org.ergoplatform.modifiers.mempool.ErgoTransaction
import org.ergoplatform.network.message.inputblocks.{
  InputBlockMessageSpec,
  InputBlockMessageLimits,
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
import scorex.util.ModifierId
import scorex.util.serialization.{VLQByteBufferReader, VLQByteBufferWriter}

import java.nio.ByteBuffer

class InputBlockMessageSpecsSpec extends ErgoCorePropertyTest with SerializationTests {
  import org.ergoplatform.utils.generators.CoreObjectGenerators._
  import org.ergoplatform.utils.generators.ErgoCoreGenerators._
  import org.ergoplatform.utils.generators.ErgoCoreTransactionGenerators._

  private val inputBlockMessageSpec = InputBlockMessageSpec
  private val inputBlockTransactionIdsMessageSpec = InputBlockTransactionIdsMessageSpec
  private val inputBlockTransactionsMessageSpec = InputBlockTransactionsMessageSpec
  private val inputBlockTransactionsRequestMessageSpec = InputBlockTransactionsRequestMessageSpec

  private def bytesWithInputBlockIdAndCount(inputBlockId: ModifierId, count: Long): Array[Byte] = {
    val writer = new VLQByteBufferWriter(new scorex.util.ByteArrayBuilder())
    writer.putBytes(scorex.util.idToBytes(inputBlockId))
    writer.putUInt(count)
    writer.toBytes
  }

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

  property("InputBlockTransactionIdsData rejects excessive transaction id count") {
    val inputBlockId = modifierIdGen.sample.get
    val bytes = bytesWithInputBlockIdAndCount(inputBlockId, InputBlockMessageLimits.MaxArraySize + 1)
    val reader = new VLQByteBufferReader(ByteBuffer.wrap(bytes))

    val ex = the[Exception] thrownBy inputBlockTransactionIdsMessageSpec.parse(reader)
    ex.getMessage should include ("Input-block transaction IDs count too large")
  }

  property("InputBlockTransactionsData rejects excessive transaction count") {
    val inputBlockId = modifierIdGen.sample.get
    val bytes = bytesWithInputBlockIdAndCount(inputBlockId, InputBlockMessageLimits.MaxArraySize + 1)
    val reader = new VLQByteBufferReader(ByteBuffer.wrap(bytes))

    val ex = the[Exception] thrownBy inputBlockTransactionsMessageSpec.parse(reader)
    ex.getMessage should include ("Input-block transactions count too large")
  }

  property("InputBlockTransactionsRequest rejects excessive transaction id count") {
    val inputBlockId = modifierIdGen.sample.get
    val bytes = bytesWithInputBlockIdAndCount(inputBlockId, InputBlockMessageLimits.MaxArraySize + 1)
    val reader = new VLQByteBufferReader(ByteBuffer.wrap(bytes))

    val ex = the[Exception] thrownBy inputBlockTransactionsRequestMessageSpec.parse(reader)
    ex.getMessage should include ("Input-block transaction request IDs count too large")
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
}
