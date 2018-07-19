package org.ergoplatform.nodeView.wallet

import io.iohk.iodb.ByteArrayWrapper
import org.ergoplatform.ErgoBox
import org.ergoplatform.modifiers.mempool.ErgoTransaction
import org.ergoplatform.nodeView.history.ErgoHistory.Height
import org.ergoplatform.settings.Constants.ModifierIdSize
import scorex.core.ModifierId
import scorex.core.serialization.Serializer
import sigmastate.utils.{ByteArrayBuilder, ByteArrayWriter}

import scala.util.Try

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


class BoxUnspentSerializer(transactionLookup: ModifierId => Try[ErgoTransaction]) extends Serializer[BoxUnspent] {

  override def toBytes(box: BoxUnspent): Array[Byte] = {
    import box._
    val buf = new ByteArrayWriter(new ByteArrayBuilder)
      .putBytes(tx.id)
      .putShort(outIndex)
      .putOption(heightOpt)(_.putInt(_))
      .putLong(ergoValue)
      .putInt(assets.size)
    assets.foreach {
      case (id, value) =>
        buf
        .putBytes(id.data)
        .putLong(value)
    }
    buf.toBytes
  }

  override def parseBytes(bytes: Array[Byte]): Try[BoxUnspent] = Try {
    val buf = sigmastate.serialization.Serializer.startReader(bytes, 0)
    val txId = ModifierId @@ buf.getBytes(ModifierIdSize)
    val outIndex = buf.getShort
    val heightOpt = buf.getOption(buf.getInt)
    val ergoValue = buf.getLong
    val assetSize = buf.getInt
    val assets = (1 to assetSize).map { _ =>
      ByteArrayWrapper(buf.getBytes(ModifierIdSize)) -> buf.getLong
    }
    transactionLookup(txId) map {
      BoxUnspent(_, outIndex, heightOpt, ergoValue, assets.toMap)
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

class BoxSpentSerializer(transactionLookup: ModifierId => Try[ErgoTransaction]) extends Serializer[BoxSpent] {

  override def toBytes(boxSpent: BoxSpent): Array[Byte] = {
    import boxSpent._
    val boxBytes = ErgoBox.serializer.toBytes(box)
    val buf = new ByteArrayWriter(new ByteArrayBuilder)
      .putBytes(boxBytes)
      .putBytes(parentTx.id)
      .putBytes(spendingTx.id)
      .putOption(heightOpt)(_.putInt(_))
      .putOption(heightSpentOpt)(_.putInt(_))
      .putLong(ergoValue)
      .putInt(assets.size)
    assets.foreach {
      case (id, value) =>
        buf
          .putBytes(id.data)
          .putLong(value)
    }
    buf.toBytes
  }

  override def parseBytes(bytes: Array[Byte]): Try[BoxSpent] = Try {
    val buf = sigmastate.serialization.Serializer.startReader(bytes, 0)
    val (ergoBox, ergoBoxBytesLength) = ErgoBox.serializer.parseBody(bytes, 0)
    buf.position = buf.position + ergoBoxBytesLength
    val parentTxId = ModifierId @@ buf.getBytes(ModifierIdSize)
    val spendingTxId = ModifierId @@ buf.getBytes(ModifierIdSize)
    val heightOpt = buf.getOption(buf.getInt)
    val heightSpentOpt = buf.getOption(buf.getInt)
    val ergoValue = buf.getLong
    val assetSize = buf.getInt
    val assets = (1 to assetSize).map { _ =>
      ByteArrayWrapper(buf.getBytes(ModifierIdSize)) -> buf.getLong
    }
    transactionLookup(parentTxId).flatMap { parentTx =>
      transactionLookup(spendingTxId).map { spendingTx =>
        BoxSpent(ergoBox, parentTx, spendingTx, heightOpt, heightSpentOpt, ergoValue, assets.toMap)
      }
    }
  }.flatten

}

case class BalancesSnapshot(height: Height, balance: Long, assetBalances: Map[ByteArrayWrapper, Long])
