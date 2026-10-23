package org.ergoplatform.nodeView.wallet

import org.ergoplatform.nodeView.history.storage.modifierprocessors.{
  UtxoSnapshotScanSource,
  UtxoSnapshotScanSourceReader
}
import org.ergoplatform.utils.ErgoCorePropertyTest
import scorex.crypto.authds.avltree.batch.Constants.DigestType
import scorex.crypto.authds.avltree.batch.serialization.BatchAVLProverSubtree
import scorex.util.ModifierId

import java.lang.reflect.Modifier
import scala.util.{Failure, Try}

class WalletApiCompatibilitySpec extends ErgoCorePropertyTest {

  property("ErgoWalletService retains a concrete off-chain reconciliation default") {
    val method = classOf[ErgoWalletService].getDeclaredMethods.find { candidate =>
      candidate.getName == "reconcileOffChainRegistry" && candidate.getParameterCount == 2
    }.get

    Modifier.isAbstract(method.getModifiers) shouldBe false
    method.isDefault shouldBe true
  }

  property("legacy ErgoWalletService implementations inherit fail-closed snapshot defaults") {
    val methods = classOf[ErgoWalletService].getDeclaredMethods
    val reset = methods.find { candidate =>
      candidate.getName == "recreateRegistryForUtxoSnapshotRecovery" &&
        candidate.getParameterCount == 2
    }.get
    val scan = methods.find { candidate =>
      candidate.getName == "scanUtxoSnapshotChunk" && candidate.getParameterCount == 8
    }.get

    Seq(reset, scan).foreach { method =>
      Modifier.isAbstract(method.getModifiers) shouldBe false
      method.isDefault shouldBe true
    }
  }

  property("legacy snapshot source readers inherit a fail-closed no-argument read") {
    val legacyReader = new UtxoSnapshotScanSourceReader {
      override def readUtxoSnapshotScanSource(
        expectedBlockId: ModifierId
      ): Try[UtxoSnapshotScanSource] =
        Failure(new UnsupportedOperationException(expectedBlockId))

      override def readUtxoSnapshotScanPart(
        source: UtxoSnapshotScanSource,
        index: Int
      ): Try[BatchAVLProverSubtree[DigestType]] =
        Failure(new UnsupportedOperationException(index.toString))
    }

    val result = legacyReader.readUtxoSnapshotScanSource()
    result.isFailure shouldBe true
    result.failed.get shouldBe a[UnsupportedOperationException]
  }
}
