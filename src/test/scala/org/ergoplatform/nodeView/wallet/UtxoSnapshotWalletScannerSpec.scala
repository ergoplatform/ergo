package org.ergoplatform.nodeView.wallet

import org.ergoplatform.utils.ErgoCorePropertyTest
import scorex.crypto.authds.avltree.batch.Constants.{DigestType, hashFn}
import scorex.crypto.authds.avltree.batch.ProverLeaf
import scorex.crypto.authds.avltree.batch.serialization.BatchAVLProverSubtree
import scorex.crypto.authds.{ADKey, ADValue}

class UtxoSnapshotWalletScannerSpec extends ErgoCorePropertyTest {

  property("UTXO snapshot collectBoxes fails on malformed box bytes") {
    val leaf = new ProverLeaf[DigestType](
      ADKey @@ Array.fill(32)(1: Byte),
      ADValue @@ Array[Byte](1, 2, 3),
      ADKey @@ Array.fill(32)(2: Byte)
    )(hashFn)
    val subtree = new BatchAVLProverSubtree[DigestType](leaf)

    UtxoSnapshotWalletScanner.collectBoxes(subtree).isFailure shouldBe true
  }
}
