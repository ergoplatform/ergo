package org.ergoplatform.serialization

import io.circe.parser.parse
import org.scalatest.flatspec.AnyFlatSpec

class JsonParserDepthSpec extends AnyFlatSpec {

  "Circe JSON parser" should "reject deeply nested input" in {
    // Jawn 1.7.0 limits nesting to 4096 levels by default; Jawn 1.6.0 has no limit
    val depth  = 100000
    val nested = ("[" * depth) + "0" + ("]" * depth)

    parse(nested) match {
      case Left(_)  => succeed
      case Right(_) => fail(s"JSON parser accepted $depth nested arrays")
    }
  }
}
