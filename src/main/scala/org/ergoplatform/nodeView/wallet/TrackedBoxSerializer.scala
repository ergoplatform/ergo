package org.ergoplatform.nodeView.wallet

import org.ergoplatform.ErgoBox
import org.ergoplatform.modifiers.mempool.{ErgoBoxSerializer, ErgoTransaction}
import org.ergoplatform.nodeView.wallet.BoxCertainty.{Certain, Uncertain}
import org.ergoplatform.nodeView.wallet.ChainStatus.{Offchain, Onchain}
import org.ergoplatform.nodeView.wallet.SpendingStatus.{Spent, Unspent}
import org.ergoplatform.nodeView.wallet.TrackedBoxSerializer.TransactionLookup
import org.ergoplatform.settings.Constants.ModifierIdSize
import scorex.core.serialization.ScorexSerializer
import scorex.core.utils.ScorexEncoding
import scorex.core.validation.ModifierValidator
import scorex.util.serialization.{Reader, Writer}
import scorex.util.{ModifierId, bytesToId, idToBytes}
import scorex.util.Extensions._

import scala.util.{Failure, Success, Try}

class TrackedBoxSerializer(txLookup: TransactionLookup)
  extends ScorexSerializer[TrackedBox]
    with ModifierValidator
    with ScorexEncoding {


  override def serialize(trackedBox: TrackedBox, w: Writer): Unit = {
    w.putBits(headerBits(trackedBox))
      .putBytes(idToBytes(trackedBox.creationTxId))
      .putShort(trackedBox.creationOutIndex)
      .putOption(trackedBox.inclusionHeight)(_.putInt(_))
      .putOption(trackedBox.spendingTxId)((r, id) => r.putBytes(idToBytes(id)))
      .putOption(trackedBox.spendingHeight)(_.putInt(_))
    ErgoBoxSerializer.serialize(trackedBox.box, w)
  }

  override def parse(r: Reader): TrackedBox = {
    readHeader(r) { certainty =>
      readTx(r, txLookup) { creationTx =>
        val creationOutIndex = r.getShort()
        val creationHeight = r.getOption(r.getInt())
        readTxOpt(r, txLookup) { spendingTx =>
          val spendingHeight = r.getOption(r.getInt())
          readErgoBox(r) { box =>
            TrackedBox(creationTx, creationOutIndex, creationHeight, spendingTx, spendingHeight, box, certainty)
          }
        }
      }
    } match {
      case Success(value) => value
      case Failure(exception) => throw exception
    }
  }

  protected def headerBits(trackedBox: TrackedBox): Array[Boolean] = {
    Array(trackedBox.spendingStatus.spent, trackedBox.chainStatus.onchain, trackedBox.certainty.certain)
  }

  protected def readHeaderBits(r: Reader): (SpendingStatus, ChainStatus, BoxCertainty) = {
    val bits = r.getBits(size = 3)
    (readSpendingStatus(bits(0)), readChainStatus(bits(1)), readCertainty(bits(2)))
  }

  protected def readHeader(r: Reader)(parser: BoxCertainty => Try[TrackedBox]): Try[TrackedBox] = {
    val (spendingStatus, chainStatus, certainty) = readHeaderBits(r)
    parser(certainty) flatMap { trackedBox =>
      accumulateErrors
        .demand(trackedBox.spendingStatus == spendingStatus, s"$trackedBox corrupted: should be $spendingStatus")
        .demand(trackedBox.chainStatus == chainStatus, s"$trackedBox corrupted: should be $chainStatus")
        .result(trackedBox)
        .toTry
    }
  }

  private def readCertainty(bit: Boolean): BoxCertainty =
    Seq(Certain, Uncertain).find(_.certain == bit).getOrElse(Uncertain)

  private def readChainStatus(bit: Boolean): ChainStatus =
    Seq(Onchain, Offchain).find(_.onchain == bit).getOrElse(Offchain)

  private def readSpendingStatus(bit: Boolean): SpendingStatus =
    Seq(Spent, Unspent).find(_.spent == bit).getOrElse(Unspent)

  protected def readTxOpt(r: Reader, txLookup: TransactionLookup)
                         (parser: Option[ErgoTransaction] => Try[TrackedBox]): Try[TrackedBox] = {
    r.getOption(readTx(r, txLookup)(tx => parser(Option(tx)))).getOrElse(parser(None))
  }

  protected def readTx(r: Reader, txLookup: TransactionLookup)
                      (parser: ErgoTransaction => Try[TrackedBox]): Try[TrackedBox] = {
    val txId = bytesToId(r.getBytes(ModifierIdSize))
    txLookup(txId) match {
      case Some(tx) => parser(tx)
      case None => Failure(new NoSuchElementException(s"Transaction not found $txId"))
    }
  }

  protected def readErgoBox(r: Reader)(parser: ErgoBox => TrackedBox): Try[TrackedBox] = {
    ErgoBoxSerializer.parseTry(r).map { box => parser(box) }
  }

}

object TrackedBoxSerializer {
  type TransactionLookup = ModifierId => Option[ErgoTransaction]
}
