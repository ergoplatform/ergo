package org.ergoplatform.nodeView.wallet.persistence

import cats.free.Free
import org.ergoplatform.ErgoBox.BoxId
import org.ergoplatform.modifiers.mempool.ErgoTransaction
import org.ergoplatform.wallet.boxes.TrackedBox
import scorex.util.ModifierId

/**
  * ADT defining all primitive operations over wallet registry.
  */
sealed trait RegistryOpA[A]

object RegistryOpA {

  type RegistryOp[A] = Free[RegistryOpA, A]

  final case class PutBox(box: TrackedBox) extends RegistryOpA[Unit]

  final case class GetBox(id: BoxId) extends RegistryOpA[Option[TrackedBox]]

  final case class GetBoxes(ids: Seq[BoxId]) extends RegistryOpA[Seq[Option[TrackedBox]]]

  case object GetAllBoxes extends RegistryOpA[Seq[TrackedBox]]

  final case class RemoveBoxes(ids: Seq[BoxId]) extends RegistryOpA[Unit]

  final case class PutTx(tx: ErgoTransaction) extends RegistryOpA[Unit]

  case object GetAllTxs extends RegistryOpA[Seq[ErgoTransaction]]

  final case class RemoveTxs(ids: Seq[ModifierId]) extends RegistryOpA[Unit]

  final case class PutIndex(index: RegistryIndex) extends RegistryOpA[Unit]

  case object GetIndex extends RegistryOpA[RegistryIndex]

}
