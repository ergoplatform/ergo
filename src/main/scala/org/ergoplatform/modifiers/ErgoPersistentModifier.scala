package org.ergoplatform.modifiers

import org.ergoplatform.modifiers.mempool.AnyoneCanSpendTransaction
import org.ergoplatform.modifiers.mempool.proposition.AnyoneCanSpendProposition
import scorex.core.NodeViewModifier._
import scorex.core.PersistentNodeViewModifier

trait ErgoPersistentModifier extends PersistentNodeViewModifier[AnyoneCanSpendProposition.type, AnyoneCanSpendTransaction] {

  //TODO do we need version field for all modifiers?
  //val version: Version

  //TODO Remove from base class
  def parentId: ModifierId = null

  //TODO Remove from base class
  def transactions: Option[Seq[AnyoneCanSpendTransaction]] = None

}
