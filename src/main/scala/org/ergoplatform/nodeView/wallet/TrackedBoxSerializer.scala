package org.ergoplatform.nodeView.wallet

import org.ergoplatform.ErgoBox
import org.ergoplatform.modifiers.mempool.{ErgoBoxSerializer, ErgoTransaction}
import org.ergoplatform.nodeView.wallet.BoxCertainty.{Certain, Uncertain}
import org.ergoplatform.nodeView.wallet.ChainStatus.{Offchain, Onchain}
import org.ergoplatform.nodeView.wallet.SpendingStatus.{Spent, Unspent}
import org.ergoplatform.nodeView.wallet.TrackedBoxSerializer.TransactionLookup
import org.ergoplatform.settings.Constants.ModifierIdSize
import scorex.core.serialization.Serializer
import scorex.core.utils.ScorexEncoding
import scorex.core.validation.ModifierValidator
import scorex.util.{ModifierId, bytesToId, idToBytes}
import sigmastate.utils.{ByteReader, ByteWriter, SigmaByteReader, SigmaByteWriter}

import scala.util.{Failure, Try}

class TrackedBoxSerializer(txLookup: TransactionLookup)
  extends Serializer[TrackedBox]
    with ModifierValidator
    with ScorexEncoding {

  def toBytes(trackedBox: TrackedBox): Array[Byte] = makeBytes(write(trackedBox, _))

  def parseBytes(bytes: Array[Byte]): Try[TrackedBox] = read(startReader(bytes))

  def write(trackedBox: TrackedBox, w: SigmaByteWriter): Unit = {
    w.putBits(headerBits(trackedBox))
      .putBytes(idToBytes(trackedBox.creationTxId))
      .putShort(trackedBox.creationOutIndex)
      .putOption(trackedBox.creationHeight)(_.putInt(_))
      .putOption(trackedBox.spendingTxId)((r, id) => r.putBytes(idToBytes(id)))
      .putOption(trackedBox.spendingHeight)(_.putInt(_))
    ErgoBoxSerializer.write(trackedBox.box, w)
  }

  def read(r: SigmaByteReader): Try[TrackedBox] = {
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
    }
  }

  protected def startWriter(): SigmaByteWriter = sigmastate.serialization.Serializer.startWriter()

  protected def startReader(bytes: Array[Byte]): SigmaByteReader =
    sigmastate.serialization.Serializer.startReader(bytes, 0)

  protected def makeBytes(encoder: SigmaByteWriter => Unit): Array[Byte] = {
    val w = startWriter()
    encoder(w)
    w.toBytes
  }

  protected def headerBits(trackedBox: TrackedBox): Array[Boolean] = {
    Array(trackedBox.spendingStatus.spent, trackedBox.chainStatus.onchain, trackedBox.certainty.certain)
  }

  protected def readHeaderBits(r: ByteReader): (SpendingStatus, ChainStatus, BoxCertainty) = {
    val bits = r.getBits(size = 3)
    (readSpendingStatus(bits(0)), readChainStatus(bits(1)), readCertainty(bits(2)))
  }

  protected def readHeader(r: ByteReader)(parser: BoxCertainty => Try[TrackedBox]): Try[TrackedBox] = {
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

  protected def readTxOpt(r: ByteReader, txLookup: TransactionLookup)
                         (parser: Option[ErgoTransaction] => Try[TrackedBox]): Try[TrackedBox] = {
    r.getOption(readTx(r, txLookup)(tx => parser(Option(tx)))).getOrElse(parser(None))
  }

  protected def readTx(r: ByteReader, txLookup: TransactionLookup)
                      (parser: ErgoTransaction => Try[TrackedBox]): Try[TrackedBox] = {
    val txId = bytesToId(r.getBytes(ModifierIdSize))
    txLookup(txId) match {
      case Some(tx) => parser(tx)
      case None => Failure(new NoSuchElementException(s"Transaction not found $txId"))
    }
  }

  protected def readErgoBox(r: SigmaByteReader)(parser: ErgoBox => TrackedBox): Try[TrackedBox] = {
    ErgoBoxSerializer.read(r) map { box => parser(box) }
  }

}

object TrackedBoxSerializer {
  type TransactionLookup = ModifierId => Option[ErgoTransaction]
}
