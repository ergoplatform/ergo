package org.ergoplatform.nodeView.history.storage

import io.iohk.iodb.ByteArrayWrapper
import org.ergoplatform.modifiers.history.Header
import org.ergoplatform.settings.Algos
import org.ergoplatform.settings.Constants.HashLength

object StorageKeys {

  val BestHeaderKey: ByteArrayWrapper =
    ByteArrayWrapper(Array.fill(HashLength)(Header.TypeId))

  val BestFullBlockKey: ByteArrayWrapper =
    ByteArrayWrapper(Array.fill(HashLength)(-1))

  // PoPow
  val LastProofIdKey: ByteArrayWrapper =
    ByteArrayWrapper(Algos.hash("last_proof"))

  val BestProofIdKey: ByteArrayWrapper =
    ByteArrayWrapper(Algos.hash("best_checked"))

  val ProofsCheckedKey: ByteArrayWrapper =
    ByteArrayWrapper(Algos.hash("proofs_checked"))
}
