package org.ergoplatform.settings

import com.google.common.primitives.Ints
import scorex.core.serialization.Serializer

import scala.util.Try


/**
  * System parameters which could be readjusted via collective miners decision.
  */
abstract class Parameters {

  def parametersTable: Map[Byte, Int]

  // Max size of transactions section of a block.
  lazy val MaxBlockSize: Int = parametersTable(MaxBlockSizeIncrease)

  // Max total computation cost of a block.
  lazy val MaxBlockCost: Long = parametersTable(MaxBlockCostIncrease)

  val Kdefault = 1250000
  val Kmax = 2500000
  val Kmin = 0
  val Kstep = 50000

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

  //A vote for nothing
  val NoParameter = 0: Byte

  //Parameter identifiers
  val KIncrease = 1: Byte
  val KDecrease = -KIncrease

  val MinValuePerByteIncrease = 2: Byte
  val MinValuePerByteDecrease = -MinValuePerByteIncrease

  val MaxBlockSizeIncrease = 3: Byte
  val MaxBlockSizeDecrease = -MaxBlockSizeIncrease

  val MaxBlockCostIncrease = 4: Byte
  val MaxBlockCostDecrease = -MaxBlockCostIncrease

  /*
   def changeParameter(paramId: Byte) = {
     ???
   }*/

  override def toString: String = s"Parameters(${parametersTable.mkString("; ")})"
}

object Parameters {
  def apply(paramsTable: Map[Byte, Int]): Parameters = new Parameters {
    override val parametersTable: Map[Byte, Int] = paramsTable
  }
}

object ParametersSerializer extends Serializer[Parameters] {
  override def toBytes(obj: Parameters): Array[Byte] = {
    obj.parametersTable.map { case (k, v) => k +: Ints.toByteArray(v) }.reduce(_ ++ _)
  }

  override def parseBytes(bytes: Array[Byte]): Try[Parameters] = Try {
    assert(bytes.length % 5 == 0)
    val table = bytes.grouped(5).map { bs => bs.head -> Ints.fromByteArray(bs.tail) }.toMap
    Parameters(table)
  }
}

object LaunchParameters extends Parameters {
  override val parametersTable = Map(
    KIncrease -> Kdefault,
    MinValuePerByteIncrease -> MinValuePerByteDefault,
    MaxBlockSizeIncrease -> 512 * 1024,
    MaxBlockCostIncrease -> 1000000
  )
}