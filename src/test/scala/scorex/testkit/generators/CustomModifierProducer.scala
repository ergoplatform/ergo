package scorex.testkit.generators

import org.ergoplatform.modifiers.BlockSection
import org.ergoplatform.nodeView.history.ErgoHistory
import org.ergoplatform.nodeView.state.ErgoState
import org.ergoplatform.testkit.generators

sealed trait ModifierProducerTemplateItem

case object SynInvalid extends generators.ModifierProducerTemplateItem
case object Valid extends generators.ModifierProducerTemplateItem

trait CustomModifierProducer[ST <: ErgoState[ST]] {

  def customModifiers(history: ErgoHistory,
                      state: ST,
                      template: Seq[generators.ModifierProducerTemplateItem]): Seq[BlockSection]
}
