package org.ergoplatform.nodeView.wallet

import org.ergoplatform.ErgoBox.{BoxId, TokenId}
import org.ergoplatform.modifiers.ModifierId
import org.ergoplatform.settings.Algos
import scorex.crypto.authds.ADKey
import sigma.data.Digest32Coll
import supertagged.TaggedType
import sigma.Extensions.ArrayOps

object IdUtils {

  object EncodedBoxId extends TaggedType[String]

  type EncodedBoxId = EncodedBoxId.Type

  type EncodedTokenId = ModifierId

  def encodedBoxId(id: BoxId): EncodedBoxId = EncodedBoxId @@ Algos.encode(id)

  def decodedBoxId(id: EncodedBoxId): BoxId = ADKey @@ Algos.decode(id)
    .getOrElse(throw new Error("Failed to decode box id"))

  def encodedTokenId(id: TokenId): EncodedTokenId = ModifierId.fromHex(Algos.encode(id))

  def decodedTokenId(id: EncodedTokenId): TokenId =
    Digest32Coll @@ (Algos.decode(id.toHexString).getOrElse(throw new Error("Failed to decode token id"))).toColl

}
