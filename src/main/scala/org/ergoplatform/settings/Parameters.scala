package org.ergoplatform.settings

import com.google.common.primitives.Ints
import org.ergoplatform.modifiers.history.{Extension, ExtensionCandidate}
import org.ergoplatform.nodeView.history.ErgoHistory.Height
import scorex.core.serialization.Serializer

import scala.util.Try


/**
  * System parameters which could be readjusted via collective miners decision.
  */
abstract class Parameters {

  def height: Height

  def parametersTable: Map[Byte, Int]

  // Max size of transactions section of a block.
  lazy val MaxBlockSize: Int = parametersTable(MaxBlockSizeIncrease)

  // Max total computation cost of a block.
  lazy val MaxBlockCost: Long = parametersTable(MaxBlockCostIncrease)

  val Kdefault = 1250000
  val Kmax = 5000000
  val Kmin = 0
  val Kstep = 25000

  /** Cost of storing 1 byte per Constants.StoragePeriod blocks, in nanoErgs
    */
  lazy val K: Int = parametersTable(KIncrease)

  /** To prevent creation of dust which is not profitable to charge storage fee from, we have this min-value per-byte
    * parameter.
    */
  val MinValuePerByteDefault: Int = 30 * 12
  val MinValueStep = 10
  val MinValueMin = 0
  val MinValueMax = 100000000 //0.1 Erg

  lazy val MinValuePerByte: Int = parametersTable(MinValuePerByteIncrease)

  //Parameter identifiers
  val KIncrease = 1: Byte
  val KDecrease = -KIncrease

  val MinValuePerByteIncrease = 2: Byte
  val MinValuePerByteDecrease = -MinValuePerByteIncrease

  val MaxBlockSizeIncrease = 3: Byte
  val MaxBlockSizeDecrease = -MaxBlockSizeIncrease

  val MaxBlockCostIncrease = 4: Byte
  val MaxBlockCostDecrease = -MaxBlockCostIncrease

  def update(newHeight: Height, votes: Seq[(Byte, Int)], votingEpochLength: Int): Parameters = {
    val paramsTable = votes.foldLeft(parametersTable){case (table, (paramId, count)) =>
      paramId match {
        case b: Byte if b == KIncrease =>
          if(count > votingEpochLength / 2) {
            val newK = if (K < Kmax) K + Kstep else K
            table.updated(KIncrease, newK)
          } else {
            table
          }
        case _ => table
      }
    }
    Parameters(newHeight, paramsTable)
  }

  def toExtensionCandidate(optionalFields: Seq[(Array[Byte], Array[Byte])] = Seq()): ExtensionCandidate = {
    val mandatoryFields = parametersTable.toSeq.map{case (k,v) => Array(k) -> Ints.toByteArray(v)}
    ExtensionCandidate(mandatoryFields, optionalFields)
  }

  override def toString: String = s"Parameters(height: $height; ${parametersTable.mkString("; ")})"
}

object Parameters {

  //A vote for nothing
  val NoParameter = 0: Byte

  val ParametersCount = 2

  def apply(h: Height, paramsTable: Map[Byte, Int]): Parameters = new Parameters {
    override val height: Height = h
    override val parametersTable: Map[Byte, Int] = paramsTable
  }

  def parseExtension(h: Height, extension: Extension): Try[Parameters] = Try {
    val paramsTable = extension.mandatoryFields.map { case (k, v) =>
      require(k.length == 1)
      require(v.length == 4)

      k.head -> Ints.fromByteArray(v)
    }.toMap
    Parameters(h, paramsTable)
  }
}

object ParametersSerializer extends Serializer[Parameters] {
  override def toBytes(params: Parameters): Array[Byte] = {
    require(params.parametersTable.nonEmpty, s"$params is empty")
    Ints.toByteArray(params.height) ++
      params.parametersTable.map { case (k, v) => k +: Ints.toByteArray(v) }.reduce(_ ++ _)
  }

  override def parseBytes(bytes: Array[Byte]): Try[Parameters] = Try {
    assert(bytes.length % 5 == 4)
    val height = Ints.fromByteArray(bytes.take(4))
    val table = bytes.drop(4).grouped(5).map { bs => bs.head -> Ints.fromByteArray(bs.tail) }.toMap
    Parameters(height, table)
  }
}

object LaunchParameters extends Parameters {
  override val height = 0

  override val parametersTable = Map(
    KIncrease -> Kdefault,
    MinValuePerByteIncrease -> MinValuePerByteDefault,
    MaxBlockSizeIncrease -> 512 * 1024,
    MaxBlockCostIncrease -> 1000000
  )
}