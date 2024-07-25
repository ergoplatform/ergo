package org.ergoplatform.nodeView.wallet

import org.ergoplatform.ErgoBox.{BoxId, TokenId}
import org.ergoplatform.settings.Algos
import scorex.crypto.authds.ADKey
import scorex.util.ModifierId
import sigmastate.eval.Digest32Coll
import sigmastate.eval.Extensions.ArrayOps
import supertagged.TaggedType

object IdUtils {

  object EncodedBoxId extends TaggedType[String]

  type EncodedBoxId = EncodedBoxId.Type

  type EncodedTokenId = ModifierId

  def encodedBoxId(id: BoxId): EncodedBoxId = EncodedBoxId @@ Algos.encode(id)

  def decodedBoxId(id: EncodedBoxId): BoxId = ADKey @@ Algos.decode(id)
    .getOrElse(throw new Error("Failed to decode box id"))

  def encodedTokenId(id: TokenId): EncodedTokenId = ModifierId @@ Algos.encode(id)

  def decodedTokenId(id: EncodedTokenId): TokenId =
    Digest32Coll @@ (Algos.decode(id).getOrElse(throw new Error("Failed to decode token id"))).toColl

}
