package scorex.testkit.generators

import org.ergoplatform.modifiers.ErgoPersistentModifier
import org.ergoplatform.nodeView.history.ErgoHistory
import org.ergoplatform.nodeView.state.ErgoState


trait TotallyValidModifierProducer[ST <: ErgoState[ST]] {

  def totallyValidModifier(history: ErgoHistory, state: ST): ErgoPersistentModifier

  def totallyValidModifiers(history: ErgoHistory, state: ST, count: Int): Seq[ErgoPersistentModifier]
}
