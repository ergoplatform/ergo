package org.ergoplatform.nodeView

import io.circe
import org.ergoplatform.settings.ErgoSettings
import org.ergoplatform.utils.ErgoGenerators
import org.scalatest.{Matchers, PropSpec}
import org.scalatest.prop.{GeneratorDrivenPropertyChecks, PropertyChecks}
import scorex.testkit.TestkitHelpers

class DigestErgoNodeViewHolderSpecification extends PropSpec
  with PropertyChecks
  with GeneratorDrivenPropertyChecks
  with Matchers
  with ErgoGenerators
  with TestkitHelpers {


  lazy val settings: ErgoSettings = new ErgoSettings {
    override def settingsJSON: Map[String, circe.Json] = Map()
  }

  lazy val digestHolder = new DigestErgoNodeViewHolder(settings)
}
