package org.ergoplatform.nodeView.state

import org.ergoplatform.modifiers.history.extension.{Extension, ExtensionCandidate,
  ExtensionSerializer}
import org.ergoplatform.settings.Constants
import org.ergoplatform.utils.ErgoCorePropertyTest
import org.ergoplatform.utils.ErgoCoreTestConstants.{defaultMinerSecretNumber,
  emptyStateContext, powScheme}
import org.ergoplatform.utils.ErgoNodeTestConstants.settings
import org.ergoplatform.utils.generators.ChainGenerator.genChain
import org.ergoplatform.utils.generators.ValidBlocksGenerators.{createUtxoState,
  validFullBlock}

import scala.util.Try

class ParsedExtensionSizeBoundarySpec extends ErgoCorePropertyTest {

  property("parsed extension preserves the serialized size for block validation") {
    val chain = genChain(2)
    val context = emptyStateContext.appendFullBlock(chain.head).get
    val block = chain.last
    context.appendFullBlock(block).isSuccess shouldBe true
    val extraFields: Seq[(Array[Byte], Array[Byte])] = (0 until 500).map { i =>
      val key = Array(if (i < 256) 0x05.toByte else 0x06.toByte, (i % 256).toByte)
      key -> Array.fill[Byte](Extension.FieldValueMaxSize)(0)
    }
    val extensionCandidate = ExtensionCandidate(extraFields ++ block.extension.fields)
    val committed = powScheme.proveBlock(
      Some(chain.head.header), block.header.version, block.header.nBits,
      block.header.stateRoot, block.adProofs.get.proofBytes, block.transactions,
      block.header.timestamp, extensionCandidate, block.header.votes,
      defaultMinerSecretNumber
    ).get
    val direct = committed.extension
    val encoded = ExtensionSerializer.toBytes(direct)
    encoded.length should be > Constants.MaxExtensionSize
    encoded.length should be < Constants.MaxExtensionSizeMax

    val parsed = ExtensionSerializer.parseBytes(encoded)
    parsed.fields.length shouldBe direct.fields.length
    ExtensionSerializer.toBytes(parsed).toSeq shouldBe encoded.toSeq
    parsed.id shouldBe direct.id
    parsed.id shouldBe committed.header.extensionId

    val directResult = context.appendFullBlock(committed.copy(extension = direct))
    val parsedResult = context.appendFullBlock(committed.copy(extension = parsed))
    val evidence = s"wire=${encoded.length}, direct.size=${direct.size}, " +
      s"parsed.size=${parsed.size}, direct=$directResult, parsed=$parsedResult"
    withClue(evidence) {
      directResult.isFailure shouldBe true
      parsedResult.isFailure shouldBe true
      directResult.failed.get.getMessage should include("Size of extension section should not exceed")
      parsedResult.failed.get.getMessage should include("Size of extension section should not exceed")
      parsed.size shouldBe encoded.length
    }
  }

  property("parser fails rather than truncating a declared extension past its parse cap") {
    val fields: Seq[(Array[Byte], Array[Byte])] = (0 until 16000).map { i =>
      Array((i / 256).toByte, (i % 256).toByte) ->
        Array.fill[Byte](Extension.FieldValueMaxSize)(0)
    }
    val extension = Extension(genChain(1).head.id, fields)
    val encoded = ExtensionSerializer.toBytes(extension)
    encoded.length should be > Constants.MaxExtensionSizeMax
    Try(ExtensionSerializer.parseBytes(encoded)).isFailure shouldBe true
  }

  property("a parsed over-limit extension must not reach persistent UTXO application") {
    val (state, boxHolder) = createUtxoState(settings)
    try {
      val original = validFullBlock(None, state, boxHolder)
      val extraFields: Seq[(Array[Byte], Array[Byte])] = (0 until 500).map { i =>
        Array(if (i < 256) 0x05.toByte else 0x06.toByte, (i % 256).toByte) ->
          Array.fill[Byte](Extension.FieldValueMaxSize)(0)
      }
      val extension = ExtensionCandidate(extraFields ++ original.extension.fields)
      val committed = powScheme.proveBlock(
        None, original.header.version, original.header.nBits,
        original.header.stateRoot, original.adProofs.get.proofBytes,
        original.transactions, original.header.timestamp, extension,
        original.header.votes, defaultMinerSecretNumber
      ).get
      val direct = committed.extension
      val parsed = ExtensionSerializer.parseBytes(ExtensionSerializer.toBytes(direct))
      parsed.id shouldBe committed.header.extensionId
      val directResult = state.applyModifier(committed, None)(_ => ())
      val parsedResult = state.applyModifier(committed.copy(extension = parsed), None)(_ => ())
      val evidence = s"wire=${direct.bytes.length}, parsed.size=${parsed.size}, " +
        s"direct=$directResult, parsed=$parsedResult"
      withClue(evidence) {
        directResult.isFailure shouldBe true
        parsedResult.isFailure shouldBe true
        directResult.failed.get.getMessage should include("Size of extension section should not exceed")
        parsedResult.failed.get.getMessage should include("Size of extension section should not exceed")
      }
    } finally {
      state.closeStorage()
    }
  }
}
