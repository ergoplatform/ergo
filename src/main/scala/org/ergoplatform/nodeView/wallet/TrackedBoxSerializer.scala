package org.ergoplatform.nodeView.wallet

import org.ergoplatform.ErgoBox
import org.ergoplatform.modifiers.mempool.{ErgoBoxSerializer, ErgoTransaction}
import org.ergoplatform.nodeView.wallet.BoxCertainty.{Certain, Uncertain}
import org.ergoplatform.nodeView.wallet.OnchainStatus.{Offchain, Onchain}
import org.ergoplatform.nodeView.wallet.SpendingStatus.{Spent, Unspent}
import org.ergoplatform.nodeView.wallet.TrackedBoxSerializer.TransactionLookup
import org.ergoplatform.settings.Constants.ModifierIdSize
import scorex.core.serialization.Serializer
import scorex.core.utils.ScorexEncoding
import scorex.core.validation.ModifierValidator
import scorex.core.{ModifierId, bytesToId, idToBytes}
import sigmastate.utils.{ByteReader, ByteWriter}

import scala.util.{Failure, Try}

class TrackedBoxSerializer(txLookup: TransactionLookup) extends TypedBoxSerializer[TrackedBox] {

  def write(trackedBox: TrackedBox, w: ByteWriter): Unit = {
    trackedBox match {
      case b: UnspentOffchainBox => new UnspentOffchainBoxSerializer(txLookup).write(b, w)
      case b: UnspentOnchainBox => new UnspentOnchainBoxSerializer(txLookup).write(b, w)
      case b: SpentOffchainBox => new SpentOffchainBoxSerializer(txLookup).write(b, w)
      case b: SpentOnchainBox => new SpentOnchainBoxSerializer(txLookup).write(b, w)
    }
  }

  override def read(r: ByteReader): Try[TrackedBox] = {
    val pos = r.position
    val (_, spendingStatus, onchainStatus) = readHeaderBits(r)
    r.position = pos

    (spendingStatus, onchainStatus) match {
      case (Unspent, Offchain) => new UnspentOffchainBoxSerializer(txLookup).read(r)
      case (Unspent, Onchain) => new UnspentOnchainBoxSerializer(txLookup).read(r)
      case (Spent, Offchain) => new SpentOffchainBoxSerializer(txLookup).read(r)
      case (Spent, Onchain) => new SpentOnchainBoxSerializer(txLookup).read(r)
    }
  }

}

object TrackedBoxSerializer {
  type TransactionLookup = ModifierId => Option[ErgoTransaction]
}

class UnspentOffchainBoxSerializer(txLookup: TransactionLookup) extends TypedBoxSerializer[UnspentOffchainBox] {

  def write(unspentOffchainBox: UnspentOffchainBox, w: ByteWriter): Unit = {
    import unspentOffchainBox._
    w.putBits(headerBits(unspentOffchainBox))
      .putBytes(idToBytes(creationTx.id))
      .putShort(creationOutIndex)
    ErgoBoxSerializer.write(box, w)
  }

  def read(r: ByteReader): Try[UnspentOffchainBox] = {
    readHeader(r, Unspent, Offchain, "UnspentOffchainBox"){ certainty =>
      readTx(r, txLookup) { tx =>
        val outIndex = r.getShort()
        readErgoBox(r) { box =>
          UnspentOffchainBox(tx, outIndex, box, certainty)
        }
      }
    }
  }
}

class UnspentOnchainBoxSerializer(txLookup: TransactionLookup) extends TypedBoxSerializer[UnspentOnchainBox] {

  def write(unspentOnchainBox: UnspentOnchainBox, w: ByteWriter): Unit = {
    import unspentOnchainBox._
    w.putBits(headerBits(unspentOnchainBox))
      .putBytes(idToBytes(creationTx.id))
      .putShort(creationOutIndex)
      .putInt(creationHeight)
    ErgoBoxSerializer.write(box, w)
  }

  def read(r: ByteReader): Try[UnspentOnchainBox] = {
    readHeader(r, Unspent, Onchain, "UnspentOnchainBox"){ certainty =>
      readTx(r, txLookup) { tx =>
        val outIndex = r.getShort()
        val height = r.getInt()
        readErgoBox(r) { box =>
          UnspentOnchainBox(tx, outIndex, height, box, certainty)
        }
      }
    }
  }

}

class SpentOffchainBoxSerializer(txLookup: TransactionLookup) extends TypedBoxSerializer[SpentOffchainBox] {

  def write(spentOffchainBox: SpentOffchainBox, w: ByteWriter): Unit = {
    import spentOffchainBox._
    w.putBits(headerBits(spentOffchainBox))
      .putBytes(idToBytes(creationTx.id))
      .putShort(creationOutIndex)
      .putOption(creationHeight)(_.putInt(_))
      .putBytes(idToBytes(spendingTx.id))
    ErgoBoxSerializer.write(box, w)
  }

  def read(r: ByteReader): Try[SpentOffchainBox] = {
    readHeader(r, Spent, Offchain, "SpentOffchainBox"){ certainty =>
      readTx(r, txLookup) { creationTx =>
        val outIndex = r.getShort()
        val creationHeight = r.getOption(r.getInt)
        readTx(r, txLookup) { spendingTx =>
          readErgoBox(r) { box =>
            SpentOffchainBox(creationTx, outIndex, creationHeight, spendingTx, box, certainty)
          }
        }
      }
    }
  }

}

class SpentOnchainBoxSerializer(txLookup: TransactionLookup) extends TypedBoxSerializer[SpentOnchainBox] {

  def write(spentOffchainBox: SpentOnchainBox, w: ByteWriter): Unit = {
    import spentOffchainBox._
    w.putBits(headerBits(spentOffchainBox))
      .putBytes(idToBytes(creationTx.id))
      .putShort(creationOutIndex)
      .putInt(creationHeight)
      .putBytes(idToBytes(spendingTx.id))
      .putInt(spendingHeight)
    ErgoBoxSerializer.write(box, w)
  }

  def read(r: ByteReader): Try[SpentOnchainBox] = {
    readHeader(r, Spent, Onchain, "SpentOnchainBox"){ certainty =>
      readTx(r, txLookup) { creationTx =>
        val outIndex = r.getShort()
        val creationHeight = r.getInt
        readTx(r, txLookup) { spendingTx =>
          val spendingHeight = r.getInt()
          readErgoBox(r) { box =>
            SpentOnchainBox(creationTx, outIndex, creationHeight, spendingTx, spendingHeight, box, certainty)
          }
        }
      }
    }
  }

}

trait TypedBoxSerializer[T <: TrackedBox] extends Serializer[T] with ModifierValidator with ScorexEncoding {


  def toBytes(trackedBox: T): Array[Byte] = makeBytes(write(trackedBox, _))
  def parseBytes(bytes: Array[Byte]): Try[T] = read(startReader(bytes))

  def write(unspentOnchainBox: T, w: ByteWriter): Unit
  def read(r: ByteReader): Try[T]


  protected def startWriter(): ByteWriter = sigmastate.serialization.Serializer.startWriter()
  protected def startReader(bytes: Array[Byte]): ByteReader = sigmastate.serialization.Serializer.startReader(bytes, 0)

  protected def makeBytes(encoder: ByteWriter => Unit): Array[Byte] = {
    val w = startWriter()
    encoder(w)
    w.toBytes
  }

  protected def headerBits(trackedBox: TrackedBox): Array[Boolean] = {
    Array(trackedBox.certain, trackedBox.onchain, trackedBox.spent)
  }

  protected def readHeaderBits(r: ByteReader): (BoxCertainty, SpendingStatus, OnchainStatus) = {
    val bits = r.getBits(size = 3)
    (readCertainty(bits(0)), readSpendingStatus(bits(2)), readOnchainStatus(bits(1)))
  }

  protected def readHeader(r: ByteReader,
                           expectedSpendingStatus: SpendingStatus,
                           expectedOnchainStatus: OnchainStatus,
                           boxTypeName: String)
                          (parser: BoxCertainty => Try[T]): Try[T] = {
    val (certainty, spendingStatus, onchainStatus) = readHeaderBits(r)
    accumulateErrors
      .demand(onchainStatus == expectedOnchainStatus, s"$boxTypeName should be $expectedOnchainStatus")
      .demand(spendingStatus == expectedSpendingStatus, s"$boxTypeName should be $expectedSpendingStatus")
      .result(Try(parser(certainty)).flatten)
      .toTry.flatten
  }

  private def readCertainty(bit: Boolean): BoxCertainty =
    Seq(Certain, Uncertain).find(_.certain == bit).getOrElse(Uncertain)

  private def readOnchainStatus(bit: Boolean): OnchainStatus =
    Seq(Onchain, Offchain).find(_.onchain == bit).getOrElse(Offchain)

  private def readSpendingStatus(bit: Boolean): SpendingStatus =
    Seq(Spent, Unspent).find(_.spent == bit).getOrElse(Unspent)

  protected def readTx(r: ByteReader, txLookup: TransactionLookup)(parser: ErgoTransaction => Try[T]): Try[T] = {
    val txId = bytesToId(r.getBytes(ModifierIdSize))
    txLookup(txId) match {
      case Some(tx) => parser(tx)
      case None => Failure(new NoSuchElementException(s"Transaction not found $txId"))
    }
  }

  protected def readErgoBox(r: ByteReader)(parser: ErgoBox => T): Try[T] = {
    ErgoBoxSerializer.read(r) map { box => parser(box) }
  }

}
