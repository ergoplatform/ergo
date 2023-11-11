package org.ergoplatform.utils

import akka.actor.{ActorRef, ActorSystem}
import akka.testkit.TestProbe
import org.ergoplatform.settings.ErgoSettings

trait NodeViewTestContext {
  def settings: ErgoSettings
  def actorSystem: ActorSystem
  def testProbe: TestProbe
  def nodeViewHolderRef: ActorRef
}
