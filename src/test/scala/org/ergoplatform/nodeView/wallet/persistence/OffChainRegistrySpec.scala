package org.ergoplatform.nodeView.wallet.persistence

import org.ergoplatform.{ErgoBox, ErgoLikeTransaction}
import org.ergoplatform.nodeView.wallet.IdUtils.{EncodedBoxId, encodedBoxId, encodedTokenId}
import org.ergoplatform.nodeView.wallet.scanning.{EqualsScanningPredicate, Scan, ScanWalletInteraction}
import org.ergoplatform.settings.Constants.TrueTree
import org.ergoplatform.utils.WalletTestOps
import org.ergoplatform.wallet.Constants
import org.ergoplatform.wallet.Constants.{PaymentsScanId, ScanId}
import org.ergoplatform.wallet.boxes.TrackedBox
import org.scalacheck.Gen
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import sigma.ast.ByteArrayConstant
import sigmastate.eval.Extensions.ArrayByteOps
import sigmastate.helpers.TestingHelpers.testBox

import scala.collection.immutable.TreeSet
import scala.util.Random


class OffChainRegistrySpec
  extends AnyFlatSpec
    with Matchers
    with ScalaCheckPropertyChecks
    with WalletTestOps {
  import org.ergoplatform.utils.generators.ErgoNodeWalletGenerators._

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
      registry = registry.updateOnTransaction(Seq.empty, boxes.map(EncodedBoxId @@@ _.boxId), Seq.empty)
      registry.digest.walletBalance shouldEqual 0
      registry.digest.walletAssetBalances shouldEqual Seq.empty


      //check remove-offchain flag
      // Only test scanIds > PaymentsScanId because single-scan boxes with scanId <= PaymentsScanId
      // are always removed by regardless of removeOffchain flag
      boxes.filter(_.scans.size > 1).flatMap(_.scans).find(id => id != Constants.PaymentsScanId && id > Constants.PaymentsScanId).map { scanId =>
        val p = EqualsScanningPredicate(ErgoBox.R1, ByteArrayConstant(TrueTree.bytes))
        val scan = Scan(scanId, "_", p, ScanWalletInteraction.Off, removeOffchain = false)
        val filtered = boxes.filter(tb => tb.scans.contains(scanId))

        val fbalance = balanceAmount(filtered.map(_.box))
        val fassetsBalance = assetAmount(filtered.map(_.box))

        registry = registry.updateOnTransaction(filtered, Seq.empty, Seq.empty)
        registry.digest.walletBalance shouldEqual fbalance
        registry.digest.walletAssetBalances.toMap shouldEqual fassetsBalance.toMap

        registry = registry.updateOnTransaction(Seq.empty, filtered.map(EncodedBoxId @@@ _.boxId), Seq(scan))
        registry.digest.walletBalance shouldEqual fbalance
        registry.digest.walletAssetBalances.toMap shouldEqual fassetsBalance.toMap

        val scan2 = Scan(scanId, "_", p, ScanWalletInteraction.Off, removeOffchain = true)
        registry = registry.updateOnTransaction(Seq.empty, filtered.map(EncodedBoxId @@@ _.boxId), Seq(scan2))
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
      val registry = OffChainRegistry.empty.updateOnBlock(height, boxes, boxes.map(tb => encodedBoxId(tb.box.id)).to[TreeSet])
      val balance = balanceAmount(boxes.map(_.box))
      val assetsBalance = assetAmount(boxes.map(_.box))
      registry.height shouldEqual height
      registry.digest.walletBalance shouldEqual balance
      registry.digest.walletAssetBalances.toMap shouldEqual assetsBalance.toMap

      //a block coming is not making any offchain box on-chain
      val registry2 = OffChainRegistry.empty.updateOnBlock(height, boxes, TreeSet.empty)
      registry2.height shouldEqual height
      registry2.digest.walletBalance shouldEqual balance
      registry2.digest.walletAssetBalances.toMap shouldEqual assetsBalance.toMap
    }
  }

  it should "count only boxes tracked by the payments scan in the off-chain balance" in {
    val customScanId = ScanId @@ 42.toShort
    val emptyTx = ErgoLikeTransaction(IndexedSeq(), IndexedSeq())
    val customToken = Array.fill(32)(7: Byte).toTokenId
    val sharedToken = Array.fill(32)(8: Byte).toTokenId
    val walletBox =
      TrackedBox(emptyTx, creationOutIndex = 0, None, testBox(1000000000L, TrueTree, 0), Set(PaymentsScanId))
    val customScanBox = TrackedBox(emptyTx, creationOutIndex = 1, None,
      testBox(2000000000L, TrueTree, 0, Seq(customToken -> 5L), boxIndex = 1), Set(customScanId))
    val sharedBox = TrackedBox(emptyTx, creationOutIndex = 2, None,
      testBox(3000000000L, TrueTree, 0, Seq(sharedToken -> 3L), boxIndex = 2), Set(PaymentsScanId, customScanId))

    val registry = OffChainRegistry.empty.updateOnTransaction(Seq(walletBox, customScanBox, sharedBox), Seq.empty, Seq.empty)

    // /wallet/balances/withUnconfirmed: the wallet box and the shared box with its token,
    // not the external scan's box or its token
    registry.digest.walletBalance shouldBe 4000000000L
    registry.digest.walletAssetBalances shouldBe Seq(encodedTokenId(sharedToken) -> 3L)
  }


}
