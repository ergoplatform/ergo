package org.ergoplatform.utils

import org.ergoplatform.utils.generators.{ErgoGenerators, ValidBlocksGenerators}
import org.scalatest.PropSpec
import org.scalatest.prop.TableDrivenPropertyChecks
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import scorex.testkit.utils.NoShrink

trait ErgoPropertyTest extends PropSpec
  with ScalaCheckPropertyChecks
  with TableDrivenPropertyChecks
  with ErgoTestHelpers
  with ErgoGenerators
  with NoShrink
  with ValidBlocksGenerators