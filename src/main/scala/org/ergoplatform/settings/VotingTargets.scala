package org.ergoplatform.settings

import com.typesafe.config.Config
import net.ceedubs.ficus.Ficus._
import org.ergoplatform.settings.ErgoSettings.configPath

import scala.collection.JavaConverters._

/**
  * Local miner settings to determine, how to vote
  *
  * @param targets        - desired parameters targets
  * @param rulesToDisable - rules to deactivate if sof-fork is desirable
  */
case class VotingTargets(targets: Map[Byte, Int], rulesToDisable: Seq[Short]) {
  val softFork: Int = targets.getOrElse(Parameters.SoftFork, 0)
}

object VotingTargets {

  val empty: VotingTargets = VotingTargets(Map(), Seq())

  def fromConfig(config: Config): VotingTargets = {
    val toDisable = config.as[Array[Int]](s"$configPath.voting.${Parameters.SoftForkDisablingRules}")
    val votingObject = config.getObject(s"$configPath.voting")

    val parameterTargets = votingObject.keySet().asScala
      .filter(_.toInt.toByte != Parameters.SoftForkDisablingRules)
      .map { id =>
        id.toInt.toByte -> votingObject.get(id).render().toInt
      }.toMap

    VotingTargets(parameterTargets, toDisable.map(_.toShort))
  }
}