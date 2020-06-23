package org.ergoplatform.settings

import com.typesafe.config.Config
import net.ceedubs.ficus.Ficus._
import org.ergoplatform.settings.ErgoSettings.configPath

import scala.collection.JavaConverters._
import scala.util.Try

/**
  * Local miner settings to determine how to vote
  *
  * @param targets       - desired parameters targets
  * @param desiredUpdate - rules to deactivate if soft-fork is desirable
  */
case class VotingTargets(targets: Map[Byte, Int], desiredUpdate: ErgoValidationSettingsUpdate) {
  val softFork: Int = targets.getOrElse(Parameters.SoftFork, 0)
}

object VotingTargets {

  val empty: VotingTargets = VotingTargets(Map(), ErgoValidationSettingsUpdate.empty)

  def fromConfig(config: Config): VotingTargets = {
    val votingObject = config.getObject(s"$configPath.voting")
    val toDisable = config.getConfig(s"$configPath.voting").as[Array[Int]](s"rulesToDisable")

    val parameterTargets = votingObject
      .keySet()
      .asScala
      .flatMap(id => Try(id.toByte -> votingObject.get(id).render().toInt).toOption)
      .toMap
    val desiredUpdate = ErgoValidationSettingsUpdate(toDisable.map(_.toShort), Seq())

    VotingTargets(parameterTargets, desiredUpdate)
  }
}
