package org.ergoplatform.serialization

import java.nio.ByteBuffer

import org.ergoplatform.modifiers.history.HistoryModifierSerializer
import org.ergoplatform.modifiers.history.header.{Header, HeaderSerializer}
import org.ergoplatform.utils.ErgoCorePropertyTest
import org.ergoplatform.utils.generators.ErgoCoreGenerators.defaultHeaderGen
import scorex.util.serialization.VLQByteBufferReader

class HeaderSizeSerializationSpec extends ErgoCorePropertyTest {
  private def checkHeader(original: Header, parsed: Header): Unit = {
    val encoded = HeaderSerializer.toBytes(original)
    parsed.sizeOpt shouldBe Some(encoded.length)
    parsed.size shouldBe encoded.length
    parsed.bytes.toSeq shouldBe encoded.toSeq
    parsed.id shouldBe original.id
  }

  private def variants(header: Header): Seq[Header] = Seq[Byte](1, 2, 5).map { version =>
    header.copy(version = version, sizeOpt = None,
      unparsedBytes = if (version == 5) Array[Byte](7, 8, 9) else Array.emptyByteArray)
  }

  property("standalone headers retain their serialized size, bytes and identity") {
    forAll(defaultHeaderGen) { generated =>
      variants(generated).foreach { header =>
        checkHeader(header, HeaderSerializer.parseBytes(HeaderSerializer.toBytes(header)))
      }
    }
  }

  property("history-wrapped headers exclude the modifier type from their size") {
    forAll(defaultHeaderGen) { generated =>
      variants(generated).foreach { header =>
        val wrapped = HistoryModifierSerializer.toBytes(header)
        val parsed = HistoryModifierSerializer.parseBytes(wrapped).asInstanceOf[Header]
        wrapped.length shouldBe header.bytes.length + 1
        checkHeader(header, parsed)
        HistoryModifierSerializer.toBytes(parsed).toSeq shouldBe wrapped.toSeq
      }
    }
  }

  property("consecutive headers count only their own bytes after a nonzero reader entry") {
    forAll(defaultHeaderGen) { generated =>
      variants(generated).foreach { header =>
        val encoded = HeaderSerializer.toBytes(header)
        val prefix = Array[Byte](1, 2, 3, 4, 5, 6, 7)
        val suffix: Byte = 99
        val reader = new VLQByteBufferReader(ByteBuffer.wrap(prefix ++ encoded ++ encoded ++ Array(suffix)))
        reader.getBytes(prefix.length).toSeq shouldBe prefix.toSeq
        reader.consumed shouldBe prefix.length
        checkHeader(header, HeaderSerializer.parse(reader))
        reader.consumed shouldBe prefix.length + encoded.length
        checkHeader(header, HeaderSerializer.parse(reader))
        reader.consumed shouldBe prefix.length + 2 * encoded.length
        reader.getByte() shouldBe suffix
      }
    }
  }
}
