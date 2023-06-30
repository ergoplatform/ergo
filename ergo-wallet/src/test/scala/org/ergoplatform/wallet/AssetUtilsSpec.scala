package org.ergoplatform.wallet

import org.ergoplatform.sdk.wallet.{AssetUtils, TokensMap}
import org.ergoplatform.wallet.utils.WalletTestHelpers
import org.scalatest.matchers.should.Matchers
import org.scalatest.prop.TableDrivenPropertyChecks

class AssetUtilsSpec extends WalletTestHelpers with Matchers with TableDrivenPropertyChecks {
  import org.ergoplatform.sdk.wallet.AssetUtils._

  val tid1 = stringToId("t1")
  val tid2 = stringToId("t2")

  val examples = Table[TokensMap, TokensMap, TokensMap]  (("m1", "m2", "merged"),
    (Map(), Map(), Map()),
    (Map(tid1 -> 1), Map(), Map(tid1 -> 1)),
    (Map(tid1 -> 1, tid2 -> 1), Map(), Map(tid1 -> 1, tid2 -> 1)),
    (Map(), Map(tid1 -> 1), Map(tid1 -> 1)),
    (Map(), Map(tid1 -> 1, tid2 -> 1), Map(tid1 -> 1, tid2 -> 1)),
    (Map(tid1 -> 1), Map(tid1 -> 1), Map(tid1 -> 2)),
    (Map(tid1 -> 1, tid2 -> 1), Map(tid1 -> 1), Map(tid2 -> 1, tid1 -> 2)), // NOTE, order is not important
    (Map(tid1 -> 1), Map(tid1 -> 1, tid2 -> 1), Map(tid2 -> 1, tid1 -> 2))
  )

  property("mergeAssets") {
    forAll(examples) { (m1, m2, merged) =>
      mergeAssets(m1, m2) shouldBe merged
    }

    // commutativity
    forAll(examples) { (m1, m2, _) =>
      mergeAssets(m1, m2) shouldBe mergeAssets(m2, m1)
    }
  }

  property("subtractAssets") {
    forAll(examples) { (m1, m2, merged) =>
      subtractAssets(merged, m2) shouldBe m1
      subtractAssets(merged, m1) shouldBe m2
    }

    val failExamples = Table[TokensMap, TokensMap](("m1", "m2"),
      (Map(), Map(tid1 -> 1)),
      (Map(tid2 -> 1), Map(tid2 -> 1, tid1 -> 1)),
      (Map(tid1 -> 1), Map(tid1 -> 2)),
      (Map(tid1 -> 1, tid2 -> 1), Map(tid2 -> 1, tid1 -> 2)),
      (Map(tid1 -> 1), Map(tid1 -> -1))
    )

    forAll(failExamples) { (small, big) =>
      assertExceptionThrown({
        AssetUtils.subtractAssets(small, big)
      },
      { // expected one of the following exceptions
        case e: IllegalArgumentException
          if Seq("Cannot subtract", s"of token $tid1").forall(e.getMessage.contains(_)) => true
        case e: IllegalArgumentException
          if Seq(s"Not enough amount of token $tid1").forall(e.getMessage.contains(_)) => true
        case e: IllegalArgumentException
          if Seq(s"Cannot subtract negative amount from token $tid1").forall(e.getMessage.contains(_)) => true
        case _ => false
      })
    }
  }
}
