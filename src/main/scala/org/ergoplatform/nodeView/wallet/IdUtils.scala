package org.ergoplatform.nodeView.wallet

import org.ergoplatform.settings.Algos

object IdUtils {

  type EncodedId = String

  type EncodedBoxId = EncodedId

  type EncodedTokenId = EncodedId

  def encodedId(id: Array[Byte]): EncodedId = Algos.encode(id)

  def decodedId(id: EncodedId): Array[Byte] = Algos.decode(id)
    .getOrElse(throw new Error("Failed to decode id"))

}
