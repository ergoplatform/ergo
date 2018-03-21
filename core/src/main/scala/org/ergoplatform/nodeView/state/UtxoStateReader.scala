package org.ergoplatform.nodeView.state

import io.iohk.iodb.Store
import org.ergoplatform.modifiers.mempool.AnyoneCanSpendTransaction
import org.ergoplatform.modifiers.mempool.proposition.{AnyoneCanSpendNoncedBox, AnyoneCanSpendNoncedBoxSerializer, AnyoneCanSpendProposition}
import scorex.core.transaction.state.TransactionValidation
import scorex.core.utils.ScorexLogging
import scorex.crypto.authds.ADKey
import scorex.crypto.authds.avltree.batch.{BatchAVLProver, NodeParameters, PersistentBatchAVLProver, VersionedIODBAVLStorage}
import scorex.crypto.hash.{Blake2b256Unsafe, Digest32}

import scala.util.{Failure, Success, Try}

trait UtxoStateReader extends ErgoStateReader with ScorexLogging with TransactionValidation[AnyoneCanSpendProposition.type, AnyoneCanSpendTransaction] {

  implicit val hf = new Blake2b256Unsafe

  val store: Store
  private lazy val np = NodeParameters(keySize = 32, valueSize = Some(ErgoState.BoxSize), labelSize = 32)
  protected lazy val storage = new VersionedIODBAVLStorage(store, np)

  protected lazy val persistentProver: PersistentBatchAVLProver[Digest32, Blake2b256Unsafe] = {
    val bp = new BatchAVLProver[Digest32, Blake2b256Unsafe](keyLength = 32, valueLengthOpt = Some(ErgoState.BoxSize))
    PersistentBatchAVLProver.create(bp, storage).get
  }

  override def validate(tx: AnyoneCanSpendTransaction): Try[Unit] = if (tx.boxIdsToOpen.forall { k =>
    persistentProver.unauthenticatedLookup(k).isDefined
  }) {
    Success()
  } else {
    Failure(new Exception(s"Not all boxes of the transaction $tx are in the state"))
  }

  /**
    * @return boxes, that miner (or any user) can take to himself when he creates a new block
    */
  def anyoneCanSpendBoxesAtHeight(height: Int): IndexedSeq[AnyoneCanSpendNoncedBox] = {
    //TODO fix
    randomBox().toIndexedSeq
  }

  def boxById(id: ADKey): Option[AnyoneCanSpendNoncedBox] =
    persistentProver
      .unauthenticatedLookup(id)
      .map(AnyoneCanSpendNoncedBoxSerializer.parseBytes)
      .flatMap(_.toOption)

  def randomBox(): Option[AnyoneCanSpendNoncedBox] =
    persistentProver.avlProver.randomWalk().map(_._1).flatMap(boxById)

}
