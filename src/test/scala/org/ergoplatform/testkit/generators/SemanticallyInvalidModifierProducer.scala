package org.ergoplatform.testkit.generators

import org.ergoplatform.modifiers.BlockSection
import org.ergoplatform.nodeView.state.ErgoState

trait SemanticallyInvalidModifierProducer[ST <: ErgoState[ST]] {
  def semanticallyInvalidModifier(state: ST): BlockSection
}