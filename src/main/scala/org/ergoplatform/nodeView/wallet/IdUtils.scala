package org.ergoplatform.nodeView.wallet

import org.ergoplatform.ErgoException
import org.ergoplatform.ErgoBox.{BoxId, TokenId}
import org.ergoplatform.settings.Algos
import scorex.crypto.authds.ADKey
import scorex.util.ModifierId
import sigma.data.Digest32Coll
import supertagged.TaggedType
import sigma.Extensions.ArrayOps

object IdUtils {

  object EncodedBoxId extends TaggedType[String]

  type EncodedBoxId = EncodedBoxId.Type

  type EncodedTokenId = ModifierId

  def encodedBoxId(id: BoxId): EncodedBoxId = EncodedBoxId @@ Algos.encode(id)

  def decodedBoxId(id: EncodedBoxId): BoxId = ADKey @@ Algos.decode(id)
    .fold(
      e => throw new ErgoException(ErgoException.WalletError, "Failed to decode box id", Some(e)),
      identity)

  def encodedTokenId(id: TokenId): EncodedTokenId = ModifierId @@ Algos.encode(id)

  def decodedTokenId(id: EncodedTokenId): TokenId =
    Digest32Coll @@ (Algos.decode(id).fold(
      e => throw new ErgoException(ErgoException.WalletError, "Failed to decode token id", Some(e)),
      identity)).toColl

}
