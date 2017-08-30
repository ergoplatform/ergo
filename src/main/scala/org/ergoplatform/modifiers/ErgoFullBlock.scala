package org.ergoplatform.modifiers

import io.circe.Json
import org.ergoplatform.modifiers.history.{ADProof, BlockTransactions, Header}
import org.ergoplatform.modifiers.mempool.AnyoneCanSpendTransaction
import org.ergoplatform.modifiers.mempool.proposition.AnyoneCanSpendProposition
import org.ergoplatform.settings.Algos
import scorex.core.NodeViewModifier.{ModifierId, ModifierTypeId}
import scorex.core.TransactionsCarryingPersistentNodeViewModifier
import scorex.core.serialization.Serializer

//TODO we need it to be ErgoPersistentModifier just to put it to ProgressInfo
case class ErgoFullBlock(header: Header,
                         blockTransactions: BlockTransactions,
                         aDProofs: Option[ADProof])
  extends ErgoPersistentModifier
    with TransactionsCarryingPersistentNodeViewModifier[AnyoneCanSpendProposition.type, AnyoneCanSpendTransaction] {

  override val modifierTypeId: ModifierTypeId = ErgoFullBlock.ModifierTypeId

  override val parentId = header.parentId

  override lazy val id: ModifierId = Algos.hash(header.id ++ blockTransactions.id ++
    aDProofs.map(_.id).getOrElse(Array()))

  override lazy val json: Json = ???

  override type M = ErgoFullBlock

  override lazy val serializer: Serializer[ErgoFullBlock] = ???

  override lazy val transactions: Seq[AnyoneCanSpendTransaction] = blockTransactions.txs
}

object ErgoFullBlock {
  val ModifierTypeId: ModifierTypeId = (-127).toByte
}
