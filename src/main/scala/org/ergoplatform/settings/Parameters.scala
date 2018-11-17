package org.ergoplatform.settings

import com.google.common.primitives.Ints
import io.circe.Encoder
import io.circe.syntax._
import org.ergoplatform.api.ApiCodecs
import org.ergoplatform.modifiers.history.{Extension, ExtensionCandidate}
import org.ergoplatform.nodeView.history.ErgoHistory.Height
import scorex.core.serialization.Serializer

import scala.util.Try


/**
  * System parameters which could be readjusted via collective miners decision.
  */
abstract class Parameters {
  import Parameters._

  def height: Height

  def parametersTable: Map[Byte, Int]

  /** Cost of storing 1 byte per Constants.StoragePeriod blocks, in nanoErgs
    */
  lazy val k: Int = parametersTable(KIncrease)

  /** To prevent creation of dust which is not profitable to charge storage fee from, we have this min-value per-byte
    * parameter.
    */
  lazy val minValuePerByte: Int = parametersTable(MinValuePerByteIncrease)

  // Max size of transactions section of a block.
  lazy val maxBlockSize: Int = parametersTable(MaxBlockSizeIncrease)

  // Max total computation cost of a block.
  lazy val maxBlockCost: Long = parametersTable(MaxBlockCostIncrease)

  def update(newHeight: Height, votes: Seq[(Byte, Int)], votingEpochLength: Int): Parameters = {
    val paramsTable = votes.foldLeft(parametersTable) { case (table, (paramId, count)) =>
      val paramIdAbs = if(paramId < 0) (-paramId).toByte else paramId

      if (count > votingEpochLength / 2) {
        val currentValue = parametersTable(paramIdAbs)
        val maxValue = maxValues.getOrElse(paramIdAbs, Int.MaxValue / 2) //todo: more precise upper-bound
        val minValue = minValues.getOrElse(paramIdAbs, 0)
        val step = stepsTable.getOrElse(paramIdAbs, Math.max(1, currentValue / 100))

        val newValue = paramId match {
          case b: Byte if b > 0 =>
            if(currentValue < maxValue) currentValue + step else currentValue
          case b: Byte if b < 0 =>
            if(currentValue > minValue) currentValue - step else currentValue
        }
        table.updated(paramIdAbs, newValue)
      } else {
        table
      }
    }
    Parameters(newHeight, paramsTable)
  }

  private def padVotes(vs: Array[Byte]): Array[Byte] = {
    if (vs.length < 3) vs ++ Array.fill(3 - vs.length)(0: Byte) else vs
  }


  def suggestVotes(ownTargets: Map[Byte, Int]): Array[Byte] = {
    val vs = ownTargets.flatMap{case (paramId, value) =>
      if(value > parametersTable(paramId)) Some(paramId) else
      if(value < parametersTable(paramId)) Some((-paramId).toByte) else None
    }.take(ParametersCount).toArray
    padVotes(vs)
  }

  def vote(ownTargets: Map[Byte, Int], votes: Array[(Byte, Int)]): Array[Byte] = {
    val vs = votes.filter { case (paramId, _) =>
      if(paramId > 0) {
        ownTargets.get(paramId).exists(_ > parametersTable(paramId))
      } else if(paramId < 0) {
        ownTargets.get((-paramId).toByte).exists(_ < parametersTable((-paramId).toByte))
      } else {
        false
      }
    }.map(_._1)
    padVotes(vs)
  }

  def toExtensionCandidate(optionalFields: Seq[(Array[Byte], Array[Byte])] = Seq()): ExtensionCandidate = {
    val mandatoryFields = parametersTable.toSeq.map { case (k, v) => Array(k) -> Ints.toByteArray(v) }
    ExtensionCandidate(mandatoryFields, optionalFields)
  }

  override def toString: String = s"Parameters(height: $height; ${parametersTable.mkString("; ")})"
}

object Parameters {

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

  val Kdefault = 1250000
  val Kmax = 5000000
  val Kmin = 0
  val Kstep = 25000

  val MinValuePerByteDefault: Int = 30 * 12
  val MinValueStep = 10
  val MinValueMin = 0
  val MinValueMax = 10000000 //0.01 Erg

  lazy val stepsTable: Map[Byte, Int] = Map (
    KIncrease -> Kstep,
    MinValuePerByteIncrease -> MinValueStep
  )

  lazy val minValues: Map[Byte, Int] = Map (
    KIncrease -> Kmin,
    MinValuePerByteIncrease -> MinValueMin
  )

  lazy val maxValues: Map[Byte, Int] = Map (
    KIncrease -> Kmax,
    MinValuePerByteIncrease -> MinValueMax
  )

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

object ParametersSerializer extends Serializer[Parameters] with ApiCodecs {
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

  implicit val jsonEncoder: Encoder[Parameters] = (p: Parameters) =>
    Map(
      "height" -> p.height.asJson,
      "K" -> p.k.asJson,
      "minValuePerByte" -> p.minValuePerByte.asJson,
      "maxBlockSize" -> p.maxBlockSize.asJson,
      "maxBlockCost" -> p.maxBlockCost.asJson
    ).asJson
}

object LaunchParameters extends Parameters {
  import Parameters._

  override val height = 0

  override val parametersTable = Map(
    KIncrease -> Kdefault,
    MinValuePerByteIncrease -> MinValuePerByteDefault,
    MaxBlockSizeIncrease -> 512 * 1024,
    MaxBlockCostIncrease -> 1000000
  )
}
