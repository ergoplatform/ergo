package org.ergoplatform

import java.io.File

import akka.actor.ActorRef
import com.typesafe.config.{Config, ConfigFactory}
import org.ergoplatform.modifiers.ErgoPersistentModifier
import org.ergoplatform.modifiers.mempool.AnyoneCanSpendTransaction
import org.ergoplatform.modifiers.mempool.proposition.AnyoneCanSpendProposition
import org.ergoplatform.nodeView.ErgoNodeViewHolder
import org.ergoplatform.settings.ErgoSettings
import scorex.core.api.http.ApiRoute
import scorex.core.app.Application
import scorex.core.network.message.MessageSpec
import scorex.core.settings.Settings

class ErgoApp(args: Seq[String]) extends Application {
  override type P = AnyoneCanSpendProposition.type
  override type TX = AnyoneCanSpendTransaction
  override type PMOD = ErgoPersistentModifier
  override type NVHT = ErgoNodeViewHolder

  val config:Config = readConfig(args.headOption)
  val ergoSettings = ErgoSettings.fromConfig(config)

  implicit lazy val settings: Settings = ergoSettings.legacySettings

  override val apiRoutes: Seq[ApiRoute] = Seq()
  override val apiTypes: Set[Class[_]] = Set()
  override protected val additionalMessageSpecs: Seq[MessageSpec[_]] = Seq()
  override val nodeViewHolderRef: ActorRef = ???
  override val nodeViewSynchronizer: ActorRef = ???
  override val localInterface: ActorRef = ???

  def forceStopApplication(): Unit = new Thread(() => System.exit(1), "ergo-platform-shutdown-thread").start()

  private def readConfig(userConfigPath: Option[String]): Config = {
    val maybeConfigFile = for {
      maybeFilename <- userConfigPath
      file = new File(maybeFilename)
      if file.exists
    } yield file

    val config = maybeConfigFile match {
      // if no user config is supplied, the library will handle overrides/application/reference automatically
      case None =>
        log.warn("NO CONFIGURATION FILE WAS PROVIDED. STARTING WITH DEFAULT SETTINGS FOR TESTNET!")
        ConfigFactory.load()
      // application config needs to be resolved wrt both system properties *and* user-supplied config.
      case Some(file) =>
        val cfg = ConfigFactory.parseFile(file)
        if (!cfg.hasPath("ergo")) {
          log.error("Malformed configuration file was provided! Aborting!")
          forceStopApplication()
        }
        ConfigFactory
          .defaultOverrides()
          .withFallback(cfg)
          .withFallback(ConfigFactory.defaultApplication())
          .withFallback(ConfigFactory.defaultReference())
          .resolve()
    }

    config
  }

}

object ErgoApp extends App {
  new ErgoApp(args).run()
}
