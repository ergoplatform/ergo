package org.ergoplatform.utils

import org.scalatest.PropSpec
import org.scalatest.prop.{PropertyChecks, TableDrivenPropertyChecks}

trait ErgoPropertyTest extends PropSpec
  with PropertyChecks
  with TableDrivenPropertyChecks
  with ErgoTestHelpers
  with ErgoGenerators
  with NoShrink