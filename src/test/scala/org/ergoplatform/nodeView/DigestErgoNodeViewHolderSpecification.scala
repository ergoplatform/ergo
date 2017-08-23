package org.ergoplatform.nodeView

import org.ergoplatform.settings.ErgoSettings
import org.ergoplatform.utils.ErgoGenerators
import org.scalatest.prop.{GeneratorDrivenPropertyChecks, PropertyChecks}
import org.scalatest.{Matchers, PropSpec}
import scorex.testkit.TestkitHelpers

class DigestErgoNodeViewHolderSpecification extends PropSpec
  with PropertyChecks
  with GeneratorDrivenPropertyChecks
  with Matchers
  with ErgoGenerators
  with TestkitHelpers {

  lazy val settings: ErgoSettings = ErgoSettings.read(None)

  lazy val digestHolder = new DigestErgoNodeViewHolder(settings)
}
