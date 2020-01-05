package org.ergoplatform.nodeView.wallet.persistence

import org.ergoplatform.utils.WalletTestOps
import org.ergoplatform.utils.generators.WalletGenerators
import org.scalacheck.Gen
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalatest.{FlatSpec, Matchers}

class OffChainRegistrySpec
  extends FlatSpec
    with Matchers
    with GeneratorDrivenPropertyChecks
    with WalletGenerators
    with WalletTestOps {

  it should "calculate indexes correctly" in {
    forAll(Gen.listOf(trackedBoxGen)) { boxes =>
      var registry = OffChainRegistry.empty
      registry = registry.updated(boxes, Seq.empty)
      val balance = balanceAmount(boxes.map(_.box))
      val assetsBalance = assetAmount(boxes.map(_.box))
      registry.digest.walletBalance shouldEqual balance
      registry.digest.walletAssetBalances shouldEqual assetsBalance
    }
  }

}
