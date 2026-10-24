package org.ergoplatform.serialization

import org.scalatest.matchers.should.Matchers
import org.scalatest.propspec.AnyPropSpec

class SubtreeSerializerDepthSpecification extends AnyPropSpec with Matchers {
  private val internal = Array[Byte](0, 0) ++
    Array.fill[Byte](32)(1) ++ Array.fill[Byte](32)(2) ++ Array.fill[Byte](32)(3)
  private val leaf = Array[Byte](1) ++ Array.fill[Byte](32)(4) ++
    Array[Byte](0, 0, 0, 1, 5) ++ Array.fill[Byte](32)(6)

  private def encoded(depth: Int): Array[Byte] =
    Array.concat(Array.fill(depth)(internal).flatten, leaf, Array.fill(depth)(leaf).flatten)

  property("rejects excessive subtree nesting without exhausting the parser stack") {
    SubtreeSerializer.parseBytesTry(encoded(1)).isSuccess shouldBe true
    SubtreeSerializer.parseBytesTry(encoded(127)).isSuccess shouldBe true
    SubtreeSerializer.parseBytesTry(encoded(128)).isFailure shouldBe true

    val outcome = try {
      if (SubtreeSerializer.parseBytesTry(encoded(20000)).isFailure) "rejected" else "accepted"
    } catch {
      case _: StackOverflowError => "stack-overflow"
    }
    outcome shouldBe "rejected"
  }
}
