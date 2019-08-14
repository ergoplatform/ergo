package org.ergoplatform.nodeView.wallet.persistence

import cats.data.State
import cats.free.Free
import cats.free.Free.liftF
import cats.~>
import org.ergoplatform.ErgoBox.BoxId
import org.ergoplatform.db.VersionedLDBKVStore
import org.ergoplatform.nodeView.wallet.persistence.RegistryOpA._
import org.ergoplatform.nodeView.wallet.{WalletTransaction, WalletTransactionSerializer}
import org.ergoplatform.settings.Algos
import org.ergoplatform.wallet.boxes.{TrackedBox, TrackedBoxSerializer}
import scorex.util.{ModifierId, idToBytes}

import scala.language.implicitConversions

object RegistryOps {

  type UpdateBatch = (Seq[(Array[Byte], Array[Byte])], Seq[Array[Byte]])

  type RegistryOpState[A] = State[UpdateBatch, A]

  implicit class TransactionOp[A](ma: RegistryOp[A]) {

    /**
      * Applies non-versioned transaction to a given `store`.
      */
    def transact(store: VersionedLDBKVStore): A = transact(store, None)

    /**
      * Applies versioned transaction to a given `store`.
      */
    def transact(store: VersionedLDBKVStore, version: Array[Byte]): A = transact(store, Some(version))

    private def transact(store: VersionedLDBKVStore, versionOpt: Option[Array[Byte]]): A =
      ma.foldMap(interpreter(store)).run((Seq.empty, Seq.empty)).value match {
        case ((toInsert, toRemove), out: A @unchecked)
          if toInsert.nonEmpty || toRemove.nonEmpty =>
          store.update(
            toInsert,
            toRemove
          )(versionOpt.getOrElse(scorex.utils.Random.randomBytes()))
          out
        case (_, out: A @unchecked) =>
          out
      }

  }

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

  def getAllBoxes: RegistryOp[Seq[TrackedBox]] =
    liftF[RegistryOpA, Seq[TrackedBox]](GetAllBoxes)

  def removeBoxes(ids: Seq[BoxId]): RegistryOp[Unit] =
    liftF[RegistryOpA, Unit](RemoveBoxes(ids))

  def updateBox(id: BoxId)(updateF: TrackedBox => TrackedBox): RegistryOp[Unit] =
    getBox(id).flatMap { _
      .map(v => putBox(updateF(v)))
      .getOrElse(Free.pure(()))
    }

  def updateBoxes(ids: Seq[BoxId])(updateF: TrackedBox => TrackedBox): RegistryOp[Unit] =
    ids.foldLeft(Free.pure[RegistryOpA, Unit](())) { case (acc, id) =>
      acc.flatMap { _ =>
        getBox(id).flatMap { _
          .map(v => putBox(updateF(v)))
          .getOrElse(Free.pure(()))
        }
      }
    }

  def putTx(tx: WalletTransaction): RegistryOp[Unit] =
    liftF[RegistryOpA, Unit](PutTx(tx))

  def putTxs(txs: Seq[WalletTransaction]): RegistryOp[Unit] =
    txs.foldLeft(Free.pure[RegistryOpA, Unit](())) { case (acc, tx) =>
      acc.flatMap(_ => putTx(tx))
    }

  def getTx(id: ModifierId): RegistryOp[Option[WalletTransaction]] =
    liftF[RegistryOpA, Option[WalletTransaction]](GetTx(id))

  def getAllTxs: RegistryOp[Seq[WalletTransaction]] =
    liftF[RegistryOpA, Seq[WalletTransaction]](GetAllTxs)

  def removeTxs(ids: Seq[ModifierId]): RegistryOp[Unit] =
    liftF[RegistryOpA, Unit](RemoveTxs(ids))

  def putIndex(index: RegistryIndex): RegistryOp[Unit] =
    liftF[RegistryOpA, Unit](PutIndex(index))

  def getIndex: RegistryOp[RegistryIndex] =
    liftF[RegistryOpA, RegistryIndex](GetIndex)

  def updateIndex(updateF: RegistryIndex => RegistryIndex): RegistryOp[Unit] =
    getIndex.flatMap(v => putIndex(updateF(v)))

  private def interpreter(store: VersionedLDBKVStore): RegistryOpA ~> RegistryOpState =
    new (RegistryOpA ~> RegistryOpState) {
      override def apply[A](fa: RegistryOpA[A]): RegistryOpState[A] = fa match {
        case PutBox(box) =>
          State.modify { case (toInsert, toRemove) =>
            val boxBytes = TrackedBoxSerializer.toBytes(box)
            (toInsert :+ (key(box), BoxPrefix +: boxBytes), toRemove)
          }
        case GetBox(id) =>
          State.inspect { _ =>
            store.get(id)
              .flatMap(r => TrackedBoxSerializer.parseBytesTry(r.tail).toOption)
              .asInstanceOf[A]
          }
        case GetBoxes(ids) =>
          State.inspect { _ =>
            ids
              .map { id => store.get(id)
                .flatMap { x =>
                  TrackedBoxSerializer.parseBytesTry(x.tail).toOption
                }
              }
              .asInstanceOf[A]
          }
        case GetAllBoxes =>
          State.inspect { _ =>
            store.getAll((_, v) => v.head == BoxPrefix)
              .flatMap { case (_, boxBytes) =>
                TrackedBoxSerializer.parseBytesTry(boxBytes.tail).toOption
              }
              .asInstanceOf[A]
          }
        case RemoveBoxes(ids) =>
          State.modify { case (toInsert, toRemove) =>
            (toInsert, toRemove ++ ids.map(key))
          }
        case PutTx(wtx) =>
          State.modify { case (toInsert, toRemove) =>
            val txBytes = WalletTransactionSerializer.toBytes(wtx)
            (toInsert :+ (key(wtx.id), TxPrefix +: txBytes), toRemove)
          }
        case GetTx(id) =>
          State.inspect { _ =>
            store.get(key(id))
              .flatMap(r => WalletTransactionSerializer.parseBytesTry(r.tail).toOption)
              .asInstanceOf[A]
          }
        case GetAllTxs =>
          State.inspect { _ =>
            store.getAll((_, v) => v.head == TxPrefix)
              .flatMap { case (_, txBytes) =>
                WalletTransactionSerializer.parseBytesTry(txBytes.tail).toOption
              }
              .asInstanceOf[A]
          }
        case RemoveTxs(ids) =>
          State.modify { case (toInsert, toRemove) =>
            (toInsert, toRemove ++ ids.map(key))
          }
        case PutIndex(index) =>
          State.modify { case (toInsert, toRemove) =>
            val registryBytes = RegistryIndexSerializer.toBytes(index)
            (toInsert :+ (RegistryIndexKey, registryBytes), toRemove)
          }
        case GetIndex =>
          State.inspect { _ =>
            store.get(RegistryIndexKey)
              .flatMap(r => RegistryIndexSerializer.parseBytesTry(r).toOption)
              .getOrElse(RegistryIndex.empty)
              .asInstanceOf[A]
          }
      }
    }

  private val RegistryIndexKey = Algos.hash("reg_index")

  private val BoxPrefix: Byte = 0x00

  private val TxPrefix: Byte = 0x01

  private def key(trackedBox: TrackedBox): Array[Byte] = trackedBox.box.id

  private def key(id: BoxId): Array[Byte] = id

  private def key(id: ModifierId): Array[Byte] = idToBytes(id)

}
