package org.ergoplatform.utils

import org.ergoplatform.utils.generators.{ErgoGenerators, ValidBlocksGenerators}
import org.scalatest.PropSpec
import org.scalatest.prop.{PropertyChecks, TableDrivenPropertyChecks}
import scorex.testkit.utils.NoShrink

trait ErgoPropertyTest extends PropSpec
  with PropertyChecks
  with TableDrivenPropertyChecks
  with ErgoTestHelpers
  with ErgoGenerators
  with NoShrink
  with ValidBlocksGenerators
