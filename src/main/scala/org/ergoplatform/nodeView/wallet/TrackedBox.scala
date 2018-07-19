package org.ergoplatform.nodeView.wallet

import java.util.NoSuchElementException

import io.iohk.iodb.ByteArrayWrapper
import org.ergoplatform.ErgoBox
import org.ergoplatform.modifiers.mempool.ErgoTransaction
import org.ergoplatform.nodeView.history.ErgoHistory.Height
import org.ergoplatform.settings.Constants.ModifierIdSize
import scorex.core.ModifierId
import scorex.core.serialization.Serializer
import sigmastate.SBox
import sigmastate.serialization.DataSerializer

import scala.util.{Failure, Success, Try}

trait TrackedBox {
  val box: ErgoBox
  val heightOpt: Option[Height]

  lazy val onchain: Boolean = heightOpt.isDefined

  lazy val boxId = ByteArrayWrapper(box.id)
}


case class UncertainBox(tx: ErgoTransaction,
                        outIndex: Short,
                        heightOpt: Option[Height],
                        spendingTxOpt: Option[ErgoTransaction],
                        heightSpentOpt: Option[Height]) extends TrackedBox {

  lazy val box: ErgoBox = tx.outputCandidates.apply(outIndex).toBox(tx.id, outIndex)
}


trait CertainBox extends TrackedBox

case class BoxUnspent(tx: ErgoTransaction,
                      outIndex: Short,
                      heightOpt: Option[Height],
                      ergoValue: Long,
                      assets: Map[ByteArrayWrapper, Long]) extends CertainBox {
  override lazy val box: ErgoBox = tx.outputCandidates.apply(outIndex).toBox(tx.id, outIndex)

  def toSpent(spendingTx: ErgoTransaction, heightSpentOpt: Option[Height]): BoxSpent =
    BoxSpent(box, tx, spendingTx, heightOpt, heightSpentOpt, ergoValue, assets)
}


class BoxUnspentSerializer(transactionLookup: ModifierId => Option[ErgoTransaction]) extends Serializer[BoxUnspent] {

  override def toBytes(box: BoxUnspent): Array[Byte] = {
    import box._
    val writer = sigmastate.serialization.Serializer.startWriter()
      .putBytes(tx.id)
      .putShort(outIndex)
      .putOption(heightOpt)(_.putInt(_))
      .putLong(ergoValue)
      .putInt(assets.size)
    assets.foreach {
      case (id, value) =>
        writer
        .putBytes(id.data)
        .putLong(value)
    }
    writer.toBytes
  }

  override def parseBytes(bytes: Array[Byte]): Try[BoxUnspent] = Try {
    val reader = sigmastate.serialization.Serializer.startReader(bytes, 0)
    val txId = ModifierId @@ reader.getBytes(ModifierIdSize)
    val outIndex = reader.getShort
    val heightOpt = reader.getOption(reader.getInt)
    val ergoValue = reader.getLong
    val assetSize = reader.getInt
    val assets = (1 to assetSize).map { _ =>
      ByteArrayWrapper(reader.getBytes(ModifierIdSize)) -> reader.getLong
    }
    transactionLookup(txId) match {
      case Some(tx) => Success(BoxUnspent(tx, outIndex, heightOpt, ergoValue, assets.toMap))
      case None => Failure(new NoSuchElementException(s"Transaction not found $txId"))
    }
  }.flatten

}

case class BoxSpent(override val box: ErgoBox,
                    parentTx: ErgoTransaction,
                    spendingTx: ErgoTransaction,
                    heightOpt: Option[Height],
                    heightSpentOpt: Option[Height],
                    ergoValue: Long,
                    assets: Map[ByteArrayWrapper, Long]) extends CertainBox {
  override lazy val onchain = heightSpentOpt.isDefined
}

class BoxSpentSerializer(transactionLookup: ModifierId => Option[ErgoTransaction]) extends Serializer[BoxSpent] {

  override def toBytes(boxSpent: BoxSpent): Array[Byte] = {
    import boxSpent._
    val writer = sigmastate.serialization.Serializer.startWriter()
    DataSerializer.serialize[SBox.type](box, SBox, writer)
    writer
      .putBytes(parentTx.id)
      .putBytes(spendingTx.id)
      .putOption(heightOpt)(_.putInt(_))
      .putOption(heightSpentOpt)(_.putInt(_))
      .putLong(ergoValue)
      .putInt(assets.size)
    assets.foreach {
      case (id, value) =>
        writer
          .putBytes(id.data)
          .putLong(value)
    }
    writer.toBytes
  }

  override def parseBytes(bytes: Array[Byte]): Try[BoxSpent] = Try {
    val reader = sigmastate.serialization.Serializer.startReader(bytes, 0)
    val ergoBox = DataSerializer.deserialize(SBox, reader)
    val parentTxId = ModifierId @@ reader.getBytes(ModifierIdSize)
    val spendingTxId = ModifierId @@ reader.getBytes(ModifierIdSize)
    val heightOpt = reader.getOption(reader.getInt)
    val heightSpentOpt = reader.getOption(reader.getInt)
    val ergoValue = reader.getLong
    val assetSize = reader.getInt
    val assets = (1 to assetSize).map { _ =>
      ByteArrayWrapper(reader.getBytes(ModifierIdSize)) -> reader.getLong
    }
    (transactionLookup(parentTxId), transactionLookup(spendingTxId)) match {
      case (Some(parentTx), Some(spendingTx)) =>
        Success(BoxSpent(ergoBox, parentTx, spendingTx, heightOpt, heightSpentOpt, ergoValue, assets.toMap))
      case (None, _) =>
        Failure(new NoSuchElementException(s"Parent transaction not found $parentTxId"))
      case (_, None) =>
        Failure(new NoSuchElementException(s"Spending transaction not found $spendingTxId"))
    }
  }.flatten

}

case class BalancesSnapshot(height: Height, balance: Long, assetBalances: Map[ByteArrayWrapper, Long])
