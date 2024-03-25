package org.ergoplatform.utils

import org.scalatest.matchers.should.Matchers
import org.scalatest.propspec.AnyPropSpec
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

trait ErgoCorePropertyTest extends AnyPropSpec
  with ScalaCheckPropertyChecks
  with Matchers
  with NoShrink
