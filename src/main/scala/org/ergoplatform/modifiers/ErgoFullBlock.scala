package org.ergoplatform.modifiers

import io.circe.Encoder
import io.circe.syntax._
import org.ergoplatform.modifiers.history.{ADProofs, BlockTransactions, Header}
import org.ergoplatform.modifiers.mempool.AnyoneCanSpendTransaction
import org.ergoplatform.modifiers.mempool.proposition.AnyoneCanSpendProposition
import scorex.core.serialization.Serializer
import scorex.core.{ModifierId, ModifierTypeId, TransactionsCarryingPersistentNodeViewModifier}

//TODO we need it to be ErgoPersistentModifier just to put it to ProgressInfo
case class ErgoFullBlock(header: Header,
                         blockTransactions: BlockTransactions,
                         aDProofs: Option[ADProofs])
  extends ErgoPersistentModifier
    with TransactionsCarryingPersistentNodeViewModifier[AnyoneCanSpendProposition.type, AnyoneCanSpendTransaction] {

  lazy val toSeq: Seq[ErgoPersistentModifier] = Seq(header, blockTransactions) ++ aDProofs.toSeq

  override val modifierTypeId: ModifierTypeId = ErgoFullBlock.modifierTypeId

  override val parentId: ModifierId = header.parentId

  override lazy val id: ModifierId = header.id

  override type M = ErgoFullBlock

  override lazy val serializer: Serializer[ErgoFullBlock] = ???

  override lazy val transactions: Seq[AnyoneCanSpendTransaction] = blockTransactions.txs
}

object ErgoFullBlock {
  val modifierTypeId: ModifierTypeId = ModifierTypeId @@ (-127: Byte)

  implicit val jsonEncoder: Encoder[ErgoFullBlock] = (b: ErgoFullBlock) =>
    Map(
      "header" -> b.header.asJson,
      "blockTransactions" -> b.blockTransactions.asJson,
      "adProofs" -> b.aDProofs.map(_.asJson).getOrElse(Map.empty[String, String].asJson)
    ).asJson
}
