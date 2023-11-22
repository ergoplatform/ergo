package org.ergoplatform.utils

import org.ergoplatform.testkit.utils.NoShrink
import org.ergoplatform.utils.generators.{ErgoGenerators, ValidBlocksGenerators}
import org.scalatest.propspec.AnyPropSpec
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

trait ErgoPropertyTest extends AnyPropSpec
  with ScalaCheckPropertyChecks
  with ErgoTestHelpers
  with ErgoGenerators
  with NoShrink
  with ValidBlocksGenerators
