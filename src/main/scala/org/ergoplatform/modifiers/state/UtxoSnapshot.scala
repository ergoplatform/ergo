package org.ergoplatform.modifiers.state

import org.ergoplatform.settings.Algos
import scorex.crypto.hash.Digest32
import scorex.util.{ModifierId, bytesToId}

object UtxoSnapshot {

  def rootHashToSerializedId(rootHash: Digest32): Array[Byte] = Algos.hash(rootHash)

  def rootHashToId(rootHash: Digest32): ModifierId = bytesToId(rootHashToSerializedId(rootHash))

}
