package org.ergoplatform.testkit.generators

import org.ergoplatform.modifiers.BlockSection
import org.ergoplatform.nodeView.history.ErgoHistory
import org.ergoplatform.nodeView.state.ErgoState


trait TotallyValidModifierProducer[ST <: ErgoState[ST]] {

  def totallyValidModifier(history: ErgoHistory, state: ST): BlockSection

  def totallyValidModifiers(history: ErgoHistory, state: ST, count: Int): Seq[BlockSection]
}
