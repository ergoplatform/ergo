package org.ergoplatform.nodeView.wallet.scanning

import org.ergoplatform.ErgoBox
import scorex.crypto.hash.Digest32

object ScanningPredicateTestVectors {
  val complexOr = OrScanningPredicate(
    ContainsScanningPredicate(ErgoBox.R1, Array.fill(32)(1: Byte)),
    EqualsScanningPredicate(ErgoBox.R4, Array.fill(32)(0: Byte)),
    ContainsAssetPredicate(Digest32 @@ Array.fill(32)(0: Byte))
  )

  val complexAnd = AndScanningPredicate(
    ContainsScanningPredicate(ErgoBox.R1, Array.fill(32)(1: Byte)),
    EqualsScanningPredicate(ErgoBox.R4, Array.fill(32)(1: Byte)),
    ContainsAssetPredicate(Digest32 @@ Array.fill(32)(1: Byte))
  )
}
