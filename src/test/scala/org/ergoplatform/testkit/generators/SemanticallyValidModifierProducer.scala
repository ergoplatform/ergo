package org.ergoplatform.testkit.generators

import org.ergoplatform.modifiers.BlockSection
import org.ergoplatform.nodeView.state.ErgoState

trait SemanticallyValidModifierProducer[ST <: ErgoState[ST]] {
  def semanticallyValidModifier(state: ST): BlockSection
}


