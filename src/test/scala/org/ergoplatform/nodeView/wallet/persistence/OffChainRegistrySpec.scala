package org.ergoplatform.nodeView.wallet.persistence

import org.ergoplatform.ErgoBox
import org.ergoplatform.nodeView.wallet.IdUtils.{EncodedBoxId, encodedBoxId}
import org.ergoplatform.nodeView.wallet.scanning.{EqualsScanningPredicate, Scan, ScanWalletInteraction}
import org.ergoplatform.utils.WalletTestOps
import org.ergoplatform.utils.generators.WalletGenerators
import org.ergoplatform.wallet.Constants
import org.scalacheck.Gen
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import sigmastate.Values.ByteArrayConstant

import scala.collection.immutable.TreeSet
import scala.util.Random


class OffChainRegistrySpec
  extends AnyFlatSpec
    with Matchers
    with ScalaCheckPropertyChecks
    with WalletGenerators
    with WalletTestOps {

  implicit override val generatorDrivenConfig: PropertyCheckConfiguration = PropertyCheckConfiguration(minSuccessful = 5, sizeRange = 10)

  //registry.updateOnTransaction is called when offchain transaction comes
  it should "calculate indexes correctly on offchain transaction" in {
    forAll(Gen.listOf(trackedBoxGen)) { boxes =>
      //apply transaction outputs to empty offchain registry
      var registry = OffChainRegistry.empty.updateOnTransaction(boxes, Seq.empty, Seq.empty)
      val balance = balanceAmount(boxes.map(_.box))
      val assetsBalance = assetAmount(boxes.map(_.box))
      registry.digest.walletBalance shouldEqual balance
      registry.digest.walletAssetBalances.toMap shouldEqual assetsBalance.toMap

      //spend all the outputs
      registry = registry.updateOnTransaction(Seq.empty, boxes.map(EncodedBoxId @@ _.boxId), Seq.empty)
      registry.digest.walletBalance shouldEqual 0
      registry.digest.walletAssetBalances shouldEqual Seq.empty


      //check remove-offchain flag
      boxes.filter(_.scans.size > 1).flatMap(_.scans).find(_ != Constants.PaymentsScanId).map { scanId =>
        val trueProp = org.ergoplatform.settings.Constants.TrueLeaf
        val p = EqualsScanningPredicate(ErgoBox.R1, ByteArrayConstant(trueProp.bytes))
        val scan = Scan(scanId, "_", p, ScanWalletInteraction.Off, removeOffchain = false)
        val filtered = boxes.filter(tb => tb.scans.contains(scanId))

        val fbalance = balanceAmount(filtered.map(_.box))
        val fassetsBalance = assetAmount(filtered.map(_.box))

        registry = registry.updateOnTransaction(filtered, Seq.empty, Seq.empty)
        registry.digest.walletBalance shouldEqual fbalance
        registry.digest.walletAssetBalances.toMap shouldEqual fassetsBalance.toMap

        registry = registry.updateOnTransaction(Seq.empty, filtered.map(EncodedBoxId @@ _.boxId), Seq(scan))
        registry.digest.walletBalance shouldEqual fbalance
        registry.digest.walletAssetBalances.toMap shouldEqual fassetsBalance.toMap

        val scan2 = Scan(scanId, "_", p, ScanWalletInteraction.Off, removeOffchain = true)
        registry = registry.updateOnTransaction(Seq.empty, filtered.map(EncodedBoxId @@ _.boxId), Seq(scan2))
        registry.digest.walletBalance shouldEqual 0
        registry.digest.walletAssetBalances shouldEqual Seq.empty
      }
    }
  }

  //registry.updateOnTransaction is called when a block comes
  it should "calculate indexes correctly on a block" in {
    forAll(Gen.listOf(trackedBoxGen)) { boxes =>
      val height = Random.nextInt(500) + 1

      //apply block to empty registry
      val registry = OffChainRegistry.empty.updateOnBlock(height, boxes.toArray, boxes.map(tb => encodedBoxId(tb.box.id)).to[TreeSet])
      val balance = balanceAmount(boxes.map(_.box))
      val assetsBalance = assetAmount(boxes.map(_.box))
      registry.height shouldEqual height
      registry.digest.walletBalance shouldEqual balance
      registry.digest.walletAssetBalances.toMap shouldEqual assetsBalance.toMap

      //a block coming is not making any offchain box on-chain
      val registry2 = OffChainRegistry.empty.updateOnBlock(height, boxes.toArray, TreeSet.empty)
      registry2.height shouldEqual height
      registry2.digest.walletBalance shouldEqual balance
      registry2.digest.walletAssetBalances.toMap shouldEqual assetsBalance.toMap
    }
  }


}
