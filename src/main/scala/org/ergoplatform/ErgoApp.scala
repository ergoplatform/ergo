package org.ergoplatform

import akka.actor.ActorRef
import io.circe
import org.ergoplatform.modifiers.ErgoPersistentModifier
import org.ergoplatform.modifiers.mempool.AnyoneCanSpendTransaction
import org.ergoplatform.modifiers.mempool.proposition.AnyoneCanSpendProposition
import org.ergoplatform.nodeView.ErgoNodeViewHolder
import org.ergoplatform.settings.ErgoSettings
import scorex.core.api.http.ApiRoute
import scorex.core.app.Application
import scorex.core.network.message.MessageSpec

class ErgoApp(val settingsFilename: String) extends Application {
  override type P = AnyoneCanSpendProposition
  override type TX = AnyoneCanSpendTransaction
  override type PMOD = ErgoPersistentModifier
  override type NVHT = ErgoNodeViewHolder

  implicit lazy val settings: ErgoSettings = new ErgoSettings {
    override val settingsJSON: Map[String, circe.Json] = settingsFromFile(settingsFilename)
  }
  override val apiRoutes: Seq[ApiRoute] = Seq()
  override val apiTypes: Set[Class[_]] = Set()
  override protected val additionalMessageSpecs: Seq[MessageSpec[_]] = Seq()
  override val nodeViewHolderRef: ActorRef = ???
  override val nodeViewSynchronizer: ActorRef = ???
  override val localInterface: ActorRef = ???
}

object ErgoApp extends App {
  val settingsFilename = args.headOption.getOrElse("settings.json")
  new ErgoApp(settingsFilename).run()
}
