package org.ergoplatform.modifiers

import io.circe.Json
import io.iohk.iodb.ByteArrayWrapper
import org.ergoplatform.mining.Miner
import org.ergoplatform.modifiers.history.{ADProof, BlockTransactions, Header}
import org.ergoplatform.modifiers.mempool.AnyoneCanSpendTransaction
import org.ergoplatform.modifiers.mempool.proposition.AnyoneCanSpendProposition
import org.ergoplatform.nodeView.state.ErgoState.Digest
import org.ergoplatform.nodeView.state.{ErgoState, UtxoState}
import org.ergoplatform.settings.{Algos, Constants}
import org.ergoplatform.settings.Constants._
import scorex.core.NodeViewModifier.{ModifierId, ModifierTypeId}
import scorex.core.block.Block.Timestamp
import scorex.core.serialization.Serializer

//TODO we need it to be ErgoPersistentModifier just to put it to ProcessInfo
case class ErgoFullBlock(header: Header,
                         blockTransactions: BlockTransactions,
                         aDProofs: Option[ADProof]) extends ErgoPersistentModifier {
  override val modifierTypeId: ModifierTypeId = ErgoFullBlock.modifierTypeId

  override val parentId = header.parentId

  override lazy val id: ModifierId = Algos.hash(header.id ++ blockTransactions.id ++
    aDProofs.map(_.id).getOrElse(Array()))

  override lazy val json: Json = ???

  override type M = ErgoFullBlock

  override lazy val serializer: Serializer[ErgoFullBlock] = ???
}

object ErgoFullBlock {
  val modifierTypeId: ModifierTypeId = (-127).toByte
}
