package org.ergoplatform.nodeView.wallet.persistence

import org.ergoplatform.modifiers.mempool.ErgoTransaction
import org.ergoplatform.wallet.boxes.TrackedBox
import scorex.core.VersionTag
import scorex.util.ModifierId

import scala.util.Try

trait StorageBackend {

  def putBox(box: TrackedBox): Unit

  def getBox(id: ModifierId): Option[TrackedBox]

  def removeBox(id: ModifierId): Unit

  def getAllBoxes: Seq[TrackedBox]

  def putTransaction(tx: ErgoTransaction): Unit

  def getTransaction(id: ModifierId): Option[ErgoTransaction]

  def removeTransaction(id: ModifierId): Unit

  def rollback(to: VersionTag): Try[Unit]

}
