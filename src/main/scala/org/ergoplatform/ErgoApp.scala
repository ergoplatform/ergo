package org.ergoplatform

import akka.actor.ActorRef
import io.circe
import org.ergoplatform.modifiers.block.ErgoBlock
import org.ergoplatform.modifiers.transaction.AnyoneCanSpendTransaction
import org.ergoplatform.modifiers.transaction.proposition.AnyoneCanSpendProposition
import scorex.core.api.http.ApiRoute
import scorex.core.app.Application
import scorex.core.network.message.MessageSpec
import scorex.core.settings.Settings

import scala.reflect.runtime.universe.Type

class ErgoApp(val settingsFilename: String) extends Application {
  override type P = AnyoneCanSpendProposition
  override type TX = AnyoneCanSpendTransaction
  override type PMOD = ErgoBlock
  override type NVHT = _

  implicit lazy val settings = new Settings {
    override val settingsJSON: Map[String, circe.Json] = settingsFromFile(settingsFilename)
  }
  override val apiRoutes: Seq[ApiRoute] = Seq()
  override val apiTypes: Seq[Type] = Seq()
  override protected val additionalMessageSpecs: Seq[MessageSpec[_]] = Seq()
  override type _1 = this.type
  override val nodeViewHolderRef: ActorRef = ???
  override val nodeViewSynchronizer: ActorRef = ???
  override val localInterface: ActorRef = ???
}

object ErgoApp extends App {
  val settingsFilename = args.headOption.getOrElse("settings.json")
  new ErgoApp(settingsFilename).run()
}