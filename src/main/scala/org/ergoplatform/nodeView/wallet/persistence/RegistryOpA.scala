package org.ergoplatform.nodeView.wallet.persistence

import cats.free.Free
import org.ergoplatform.ErgoBox.BoxId
import org.ergoplatform.wallet.boxes.TrackedBox

sealed trait RegistryOpA[A]

object RegistryOpA {

  type RegistryOp[A] = Free[RegistryOpA, A]

  final case class PutBox(box: TrackedBox) extends RegistryOpA[Unit]

  final case class GetBox(id: BoxId) extends RegistryOpA[Option[TrackedBox]]

  final case class GetBoxes(ids: Seq[BoxId]) extends RegistryOpA[Seq[Option[TrackedBox]]]

  final case class RemoveBox(id: BoxId) extends RegistryOpA[Unit]

  final case class PutIndex(index: RegistryIndex) extends RegistryOpA[Unit]

  case object GetIndex extends RegistryOpA[Option[RegistryIndex]]

}
