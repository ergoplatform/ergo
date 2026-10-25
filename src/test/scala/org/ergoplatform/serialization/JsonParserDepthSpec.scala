package org.ergoplatform.serialization

import io.circe.parser.parse
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.typelevel.jawn.{MaxDepthExceededException, Parser}

class JsonParserDepthSpec extends AnyFlatSpec with Matchers {

  "Circe JSON parser" should "accept ordinary JSON" in {
    parse("""{"values":[1,2,3]}""").isRight shouldBe true
  }

  it should "reject input beyond Jawn's default nesting limit" in {
    Parser.DefaultMaxDepth shouldBe 4096
    val nested = ("[" * 4097) + "0" + ("]" * 4097)

    parse(nested) match {
      case Left(failure) =>
        failure.underlying.isInstanceOf[MaxDepthExceededException] shouldBe true
      case Right(_) =>
        fail("JSON parser accepted nesting beyond its maximum depth")
    }
  }
}
