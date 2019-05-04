package org.ergoplatform.settings

import com.google.common.primitives.Ints
import io.circe.Encoder
import io.circe.syntax._
import org.ergoplatform.api.ApiCodecs
import org.ergoplatform.modifiers.history.{Extension, ExtensionCandidate}
import org.ergoplatform.nodeView.history.ErgoHistory.Height
import scorex.core.serialization.ScorexSerializer
import scorex.util.serialization.{Reader, Writer}
import scorex.util.Extensions._

import scala.util.Try
import Extension.SystemParametersPrefix
import org.ergoplatform.wallet.protocol.context.ErgoLikeParameters

/**
  * System parameters which could be readjusted via collective miners decision.
  */
class Parameters(val height: Height, val parametersTable: Map[Byte, Int])
  extends ErgoLikeParameters {

  import Parameters._

  /**
    * Cost of storing 1 byte per Constants.StoragePeriod blocks, in nanoErgs.
    */
  lazy val storageFeeFactor: Int = parametersTable(StorageFeeFactorIncrease)

  /** To prevent creation of dust which is not profitable to charge storage fee from, we have this min-value per-byte
    * parameter.
    */
  lazy val minValuePerByte: Int = parametersTable(MinValuePerByteIncrease)

  /**
    * Max size of transactions section of a block.
    */
  lazy val maxBlockSize: Int = parametersTable(MaxBlockSizeIncrease)

  /**
    * Validation cost of accessing a single token.
    */
  lazy val tokenAccessCost: Int = parametersTable(TokenAccessCostIncrease)

  /**
    * Validation cost per one transaction input.
    */
  lazy val inputCost: Int = parametersTable(InputCostIncrease)

  /**
    * Validation cost per one data input.
    */
  lazy val dataInputCost: Int = parametersTable(DataInputCostIncrease)

  /**
    * Validation cost per one transaction output.
    */
  lazy val outputCost: Int = parametersTable(OutputCostIncrease)

  /**
    * Max total computation cost of a block.
    */
  lazy val maxBlockCost: Long = parametersTable(MaxBlockCostIncrease)

  lazy val softForkStartingHeight: Option[Height] = parametersTable.get(SoftForkStartingHeight)
  lazy val softForkVotesCollected: Option[Int] = parametersTable.get(SoftForkVotesCollected)

  lazy val blockVersion: Byte = parametersTable(BlockVersion).toByte

  def update(height: Height, forkVote: Boolean, epochVotes: Seq[(Byte, Int)], votingSettings: VotingSettings): Parameters = {
    val table1 = updateFork(height, parametersTable, forkVote, epochVotes, votingSettings)
    val table2 = updateParams(table1, epochVotes, votingSettings)
    Parameters(height, table2)
  }


  def updateFork(height: Height, parametersTable: Map[Byte, Int], forkVote: Boolean,
                 epochVotes: Seq[(Byte, Int)], votingSettings: VotingSettings): Map[Byte, Int] = {

    import votingSettings.{activationEpochs, softForkApproved, softForkEpochs => votingEpochs, votingLength => votingEpochLength}

    lazy val votesInPrevEpoch = epochVotes.find(_._1 == SoftFork).map(_._2).getOrElse(0)
    lazy val votes = votesInPrevEpoch + parametersTable(SoftForkVotesCollected)
    var table = parametersTable

    //successful voting - cleaning after activation
    if (softForkStartingHeight.nonEmpty
      && height == softForkStartingHeight.get + votingEpochLength * (votingEpochs + activationEpochs + 1)
      && softForkApproved(votes)) {
      table = table.-(SoftForkStartingHeight).-(SoftForkVotesCollected)
    }
    //unsuccessful voting - cleaning
    if (softForkStartingHeight.nonEmpty
      && height == softForkStartingHeight.get + votingEpochLength * (votingEpochs + 1)
      && !softForkApproved(votes)) {
      table = table.-(SoftForkStartingHeight).-(SoftForkVotesCollected)
    }
    //new voting
    if (forkVote &&
      ((softForkStartingHeight.isEmpty && height % votingEpochLength == 0) ||
        (softForkStartingHeight.nonEmpty &&
          height == softForkStartingHeight.get + (votingEpochLength * (votingEpochs + activationEpochs + 1))) ||
        (softForkStartingHeight.nonEmpty &&
          height == softForkStartingHeight.get + (votingEpochLength * (votingEpochs + 1)) &&
          !softForkApproved(votes)))) {
      table = table.updated(SoftForkStartingHeight, height).updated(SoftForkVotesCollected, 0)
    }
    //new epoch in voting
    if (softForkStartingHeight.nonEmpty
      && height <= softForkStartingHeight.get + votingEpochLength * votingEpochs) {
      table = table.updated(SoftForkVotesCollected, votes)
    }
    //successful voting - activation
    if (softForkStartingHeight.nonEmpty
      && height == softForkStartingHeight.get + votingEpochLength * (votingEpochs + activationEpochs)
      && softForkApproved(votes)) {
      table = table.updated(BlockVersion, table(BlockVersion) + 1)
    }
    table
  }

  //Update non-fork parameters
  def updateParams(parametersTable: Map[Byte, Int],
                   epochVotes: Seq[(Byte, Int)],
                   votingSettings: VotingSettings): Map[Byte, Int] = {
    epochVotes.filter(_._1 < Parameters.SoftFork).foldLeft(parametersTable) { case (table, (paramId, count)) =>

      val paramIdAbs = if (paramId < 0) (-paramId).toByte else paramId

      if (votingSettings.changeApproved(count)) {
        val currentValue = parametersTable(paramIdAbs)
        val maxValue = maxValues.getOrElse(paramIdAbs, Int.MaxValue / 2) //todo: more precise upper-bound
        val minValue = minValues.getOrElse(paramIdAbs, 0)
        val step = stepsTable.getOrElse(paramIdAbs, Math.max(1, currentValue / 100))

        val newValue = paramId match {
          case b: Byte if b > 0 =>
            if (currentValue < maxValue) currentValue + step else currentValue
          case b: Byte if b < 0 =>
            if (currentValue > minValue) currentValue - step else currentValue
        }
        table.updated(paramIdAbs, newValue)
      } else {
        table
      }
    }
  }

  private def padVotes(vs: Array[Byte]): Array[Byte] = {
    val maxVotes = ParamVotesCount + 1
    if (vs.length < maxVotes) vs ++ Array.fill(maxVotes - vs.length)(0: Byte) else vs
  }

  def vote(ownTargets: Map[Byte, Int], epochVotes: Array[(Byte, Int)], voteForFork: Boolean): Array[Byte] = {
    val vs = epochVotes.filter { case (paramId, _) =>
      if (paramId == Parameters.SoftFork) {
        voteForFork
      } else if (paramId > 0) {
        ownTargets.get(paramId).exists(_ > parametersTable(paramId))
      } else if (paramId < 0) {
        ownTargets.get((-paramId).toByte).exists(_ < parametersTable((-paramId).toByte))
      } else {
        false
      }
    }.map(_._1)
    padVotes(vs)
  }

  def suggestVotes(ownTargets: Map[Byte, Int], voteForFork: Boolean): Array[Byte] = {
    val vs = ownTargets.flatMap { case (paramId, value) =>
      if (paramId == SoftFork) {
        None
      } else if (value > parametersTable(paramId)) {
        Some(paramId)
      } else if (value < parametersTable(paramId)) {
        Some((-paramId).toByte)
      } else {
        None
      }
    }.take(ParamVotesCount).toArray
    padVotes(if (voteForFork) vs :+ SoftFork else vs)
  }

  def toExtensionCandidate(otherFields: Seq[(Array[Byte], Array[Byte])]): ExtensionCandidate = {
    val paramFields = parametersTable.toSeq.map { case (k, v) =>
      Array(SystemParametersPrefix, k) -> Ints.toByteArray(v)
    }
    ExtensionCandidate(paramFields ++ otherFields)
  }

  override def toString: String = s"Parameters(height: $height; ${parametersTable.mkString("; ")})"

  def canEqual(o: Any): Boolean = o.isInstanceOf[Parameters]

  override def equals(obj: Any): Boolean = obj match {
    case p: Parameters => matchParameters(this, p).isSuccess
    case _ => false
  }

  override def hashCode(): Height = height.hashCode() + parametersTable.hashCode()
}

object Parameters {

  val SoftFork: Byte = 120
  val SoftForkVotesCollected: Byte = 121
  val SoftForkStartingHeight: Byte = 122
  val BlockVersion: Byte = 123

  //A vote for nothing
  val NoParameter: Byte = 0

  //Parameter identifiers
  val StorageFeeFactorIncrease: Byte = 1
  val StorageFeeFactorDecrease: Byte = (-StorageFeeFactorIncrease).toByte

  val MinValuePerByteIncrease: Byte = 2
  val MinValuePerByteDecrease: Byte = (-MinValuePerByteIncrease).toByte

  val MaxBlockSizeIncrease: Byte = 3
  val MaxBlockSizeDecrease: Byte = (-MaxBlockSizeIncrease).toByte

  val MaxBlockCostIncrease: Byte = 4
  val MaxBlockCostDecrease: Byte = (-MaxBlockCostIncrease).toByte

  val TokenAccessCostIncrease: Byte = 5
  val TokenAccessCostDecrease: Byte = (-TokenAccessCostIncrease).toByte

  val InputCostIncrease: Byte = 6
  val InputCostDecrease: Byte = (-InputCostIncrease).toByte

  val DataInputCostIncrease: Byte = 7
  val DataInputCostDecrease: Byte = (-DataInputCostIncrease).toByte

  val OutputCostIncrease: Byte = 8
  val OutputCostDecrease: Byte = (-OutputCostIncrease).toByte

  val StorageFeeFactorDefault = 1250000
  val StorageFeeFactorMax = 2500000
  val StorageFeeFactorMin = 0
  val StorageFeeFactorStep = 25000

  val MinValuePerByteDefault: Int = 30 * 12
  val MinValueStep = 10
  val MinValueMin = 0
  val MinValueMax = 10000 //0.00001 Erg

  val TokenAccessCostDefault = 100

  val InputCostDefault = 2000

  val DataInputCostDefault = 100

  val OutputCostDefault = 100

  val parametersDescs: Map[Byte, String] = Map(
    StorageFeeFactorIncrease -> "Storage fee factor (per byte per storage period)",
    MinValuePerByteIncrease -> "Minimum monetary value of a box",
    MaxBlockSizeIncrease -> "Maximum block size",
    MaxBlockCostIncrease -> "Maximum cumulative computational cost of a block",
    SoftFork -> "Soft-fork (increasing version of a block)",
    TokenAccessCostIncrease -> "Token access cost",
    InputCostIncrease -> "Cost per one transaction input",
    DataInputCostIncrease -> "Cost per one data input",
    OutputCostIncrease -> "Cost per one transaction output"
  )

  val stepsTable: Map[Byte, Int] = Map(
    StorageFeeFactorIncrease -> StorageFeeFactorStep,
    MinValuePerByteIncrease -> MinValueStep
  )

  val minValues: Map[Byte, Int] = Map(
    StorageFeeFactorIncrease -> StorageFeeFactorMin,
    MinValuePerByteIncrease -> MinValueMin,
    MaxBlockSizeIncrease -> 16 * 1024,
    MaxBlockCostIncrease -> 16 * 1024
  )

  val maxValues: Map[Byte, Int] = Map(
    StorageFeeFactorIncrease -> StorageFeeFactorMax,
    MinValuePerByteIncrease -> MinValueMax
  )

  val ParamVotesCount = 2

  def apply(h: Height, paramsTable: Map[Byte, Int]): Parameters = new Parameters(h, paramsTable)

  def parseExtension(h: Height, extension: Extension): Try[Parameters] = Try {
    val paramsTable = extension.fields.flatMap { case (k, v) =>
      require(k.length == 2, s"Wrong key during parameters parsing in extension: $extension")
      if (k.head == 0) {
        require(v.length == 4, s"Wrong value during parameters parsing in extension: $extension")
        Some(k.last -> Ints.fromByteArray(v))
      } else {
        None
      }
    }.toMap
    require(paramsTable.nonEmpty, s"Parameters table is empty in extension: $extension")
    Parameters(h, paramsTable)
  }

  /**
    * Check that two set of parameters are the same (contain the same records).
    *
    * @param p1 - parameters set
    * @param p2 - parameters set
    * @return Success(p1), if parameters match, Failure(_) otherwise
    */
  def matchParameters(p1: Parameters, p2: Parameters): Try[Parameters] = Try {
    if (p1.height != p2.height) {
      throw new Exception(s"Different height in parameters, p1 = $p1, p2 = $p2")
    }
    if (p1.parametersTable.size != p2.parametersTable.size) {
      throw new Exception(s"Parameters differ in size, p1 = $p1, p2 = $p2")
    }
    p1.parametersTable.foreach { case (k, v) =>
      val v2 = p2.parametersTable(k)
      if (v2 != v) throw new Exception(s"Calculated and received parameters differ in parameter $k ($v != $v2)")
    }
  }.map(_ => p1)

}

object ParametersSerializer extends ScorexSerializer[Parameters] with ApiCodecs {

  override def serialize(params: Parameters, w: Writer): Unit = {
    require(params.parametersTable.nonEmpty, s"$params is empty")
    w.putUInt(params.height)
    w.putUInt(params.parametersTable.size)
    params.parametersTable.foreach { case (k, v) =>
      w.put(k)
      w.putInt(v)
    }
  }

  override def parse(r: Reader): Parameters = {
    val height = r.getUInt().toIntExact
    val tableLength = r.getUInt().toIntExact
    val table = (0 until tableLength).map {_ =>
      r.getByte() -> r.getInt()
    }
    Parameters(height, table.toMap)
  }

  implicit val jsonEncoder: Encoder[Parameters] = { p: Parameters =>
    Map(
      "height" -> p.height.asJson,
      "blockVersion" -> p.blockVersion.asJson,
      "storageFeeFactor" -> p.storageFeeFactor.asJson,
      "minValuePerByte" -> p.minValuePerByte.asJson,
      "maxBlockSize" -> p.maxBlockSize.asJson,
      "maxBlockCost" -> p.maxBlockCost.asJson,
      "tokenAccessCost" -> p.tokenAccessCost.asJson,
      "inputCost" -> p.inputCost.asJson,
      "dataInputCost" -> p.dataInputCost.asJson,
      "outputCost" -> p.outputCost.asJson
    ).asJson
  }

}
