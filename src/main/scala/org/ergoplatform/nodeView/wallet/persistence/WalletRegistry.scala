package org.ergoplatform.nodeView.wallet.persistence

import cats.data.State
import cats.free.Free
import cats.free.Free.liftF
import cats.~>
import io.iohk.iodb.Store
import org.ergoplatform.ErgoBox.BoxId
import org.ergoplatform.wallet.boxes.{TrackedBox, TrackedBoxSerializer}

import scala.util.Try

/**
  * Stores version-sensitive indexes (outputs, balances).
  */
final class WalletRegistry(store: Store) {

  import RegistryOpA._
  import WalletRegistry._

  type RegistryUpdate = (Seq[(Array[Byte], Array[Byte])], Seq[Array[Byte]])

  type RegistryOpState[A] = State[RegistryUpdate, A]

  def putBox(box: TrackedBox): RegistryOp[Unit] =
    liftF[RegistryOpA, Unit](PutBox(box))

  def putBoxes(boxes: Seq[TrackedBox]): RegistryOp[Unit] =
    boxes.foldLeft(Free.pure[RegistryOpA, Unit](())) { case (acc, bx) =>
      acc.flatMap(_ => putBox(bx))
    }

  def getBox(id: BoxId): RegistryOp[Option[TrackedBox]] =
    liftF[RegistryOpA, Option[TrackedBox]](GetBox(id))

  def getBoxes(ids: Seq[BoxId]): RegistryOp[Seq[Option[TrackedBox]]] =
    liftF[RegistryOpA, Seq[Option[TrackedBox]]](GetBoxes(ids))

  def removeBox(id: BoxId): RegistryOp[Unit] =
    liftF[RegistryOpA, Unit](RemoveBox(id))

  def updateBox(id: BoxId, updateF: TrackedBox => TrackedBox): RegistryOp[Unit] =
    getBox(id).flatMap { _
      .map(v => putBox(updateF(v)))
      .getOrElse(Free.pure(()))
    }

  def updateBoxes(ids: Seq[BoxId], updateF: TrackedBox => TrackedBox): RegistryOp[Unit] =
    getBoxes(ids).map { _
      .map { _
        .map(v => putBox(updateF(v)))
        .getOrElse(Free.pure(()))
      }
    }

  def putIndex(index: RegistryIndex): RegistryOp[Unit] =
    liftF[RegistryOpA, Unit](PutIndex(index))

  def getIndex: RegistryOp[Option[RegistryIndex]] =
    liftF[RegistryOpA, Option[RegistryIndex]](GetIndex)

  def updateIndex(updateF: RegistryIndex => RegistryIndex): RegistryOp[Unit] =
    getIndex.map { _
      .map(v => putIndex(updateF(v)))
      .getOrElse(Free.pure(()))
    }

  private val interpreter: RegistryOpA ~> RegistryOpState = new (RegistryOpA ~> RegistryOpState) {
    override def apply[A](fa: RegistryOpA[A]): RegistryOpState[A] = fa match {
      case PutBox(box) =>
        State.modify { case (toInsert, toRemove) =>
          val boxKey = key(box)
          val boxBytes = TrackedBoxSerializer.toBytes(box)
          (toInsert :+ (boxKey, boxBytes), toRemove)
        }
      case _ => ???
    }
  }

  def rollback(version: String): Try[Unit] = ???

}

object WalletRegistry {

  def key(box: TrackedBox): Array[Byte] = box.box.id

}
