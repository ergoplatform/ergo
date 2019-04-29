package org.ergoplatform.nodeView.wallet.persistence

import cats.data.State
import cats.free.Free
import cats.free.Free.liftF
import cats.~>
import io.iohk.iodb.{ByteArrayWrapper, Store}
import org.ergoplatform.ErgoBox.BoxId
import org.ergoplatform.wallet.boxes.{TrackedBox, TrackedBoxSerializer}
import org.ergoplatform.wallet.settings.Constants

import scala.util.Try

/**
  * Stores version-sensitive indexes (outputs, balances).
  */
final class WalletRegistry(private[persistence] val store: Store) {

  import RegistryOpA._
  import WalletRegistry._

  type UpdateBatch = (Seq[(Array[Byte], Array[Byte])], Seq[Array[Byte]])

  type RegistryOpState[A] = State[UpdateBatch, A]

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

  private[persistence] val interpreter: RegistryOpA ~> RegistryOpState =
    new (RegistryOpA ~> RegistryOpState) {
      override def apply[A](fa: RegistryOpA[A]): RegistryOpState[A] = fa match {
        case PutBox(box) =>
          State.modify { case (toInsert, toRemove) =>
            val boxBytes = TrackedBoxSerializer.toBytes(box)
            (toInsert :+ (key(box), boxBytes), toRemove)
          }
        case GetBox(id) =>
          State.inspect { _ =>
            store.get(ByteArrayWrapper(id))
              .flatMap(r => TrackedBoxSerializer.parseBytesTry(r.data).toOption)
              .asInstanceOf[A]
          }
        case GetBoxes(ids) =>
          State.inspect { _ =>
            ids
              .map { id => store.get(ByteArrayWrapper(id))
                .flatMap { x =>
                  TrackedBoxSerializer.parseBytesTry(x.data).toOption
                }
              }
              .asInstanceOf[A]
          }
        case RemoveBox(id) =>
          State.modify { case (toInsert, toRemove) =>
            (toInsert, toRemove :+ key(id))
          }
        case PutIndex(index) =>
          State.modify { case (toInsert, toRemove) =>
            val registryBytes = RegistryIndexSerializer.toBytes(index)
            (toInsert :+ (RegistryIndexKey, registryBytes), toRemove)
          }
        case GetIndex =>
          State.inspect { _ =>
            store.get(ByteArrayWrapper(RegistryIndexKey))
              .flatMap(r => RegistryIndexSerializer.parseBytesTry(r.data).toOption)
              .asInstanceOf[A]
          }
      }
    }

  def rollback(version: String): Try[Unit] = ???

}

object WalletRegistry {

  val RegistryIndexKey: Array[Byte] = "reg_index".getBytes(Constants.Encoding)

  def key(trackedBox: TrackedBox): Array[Byte] = trackedBox.box.id

  def key(id: BoxId): Array[Byte] = id

}
