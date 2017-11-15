package org.ergoplatform.modifiers

import io.circe.Json
import io.circe.syntax._
import org.ergoplatform.modifiers.history.{ADProofs, BlockTransactions, Header}
import org.ergoplatform.modifiers.mempool.AnyoneCanSpendTransaction
import org.ergoplatform.modifiers.mempool.proposition.AnyoneCanSpendProposition
import org.ergoplatform.settings.Algos
import scorex.core.{ModifierId, ModifierTypeId, TransactionsCarryingPersistentNodeViewModifier}
import scorex.core.serialization.Serializer

//TODO we need it to be ErgoPersistentModifier just to put it to ProgressInfo
case class ErgoFullBlock(header: Header,
                         blockTransactions: BlockTransactions,
                         aDProofs: Option[ADProofs])
  extends ErgoPersistentModifier
    with TransactionsCarryingPersistentNodeViewModifier[AnyoneCanSpendProposition.type, AnyoneCanSpendTransaction] {

  lazy val toSeq: Seq[ErgoPersistentModifier] = Seq(header, blockTransactions) ++ aDProofs.toSeq

  override val modifierTypeId: ModifierTypeId = ErgoFullBlock.modifierTypeId

  override val parentId: ModifierId = header.parentId

  //TODO is it ok to exclude ADProofs here? Otherwise we have problems in UTXOState when we can't rollback to block with proofs if we applied block withour proofs
  override lazy val id: ModifierId = ModifierId @@ Algos.hash(header.id ++ blockTransactions.id)

  override lazy val json: Json = Map(
    "header" -> header.json,
    "blockTransactions" -> blockTransactions.json,
    "adPoofs" -> aDProofs.map(_.json).getOrElse(Map.empty[String, String].asJson)
  ).asJson

  override type M = ErgoFullBlock

  override lazy val serializer: Serializer[ErgoFullBlock] = ???

  override lazy val transactions: Seq[AnyoneCanSpendTransaction] = blockTransactions.txs
}

object ErgoFullBlock {
  val modifierTypeId: ModifierTypeId = ModifierTypeId @@ (-127: Byte)
}
