package scorex.testkit.generators

import org.ergoplatform.modifiers.ErgoPersistentModifier
import org.ergoplatform.nodeView.history.ErgoHistory
import scorex.core.transaction.state.MinimalState

sealed trait ModifierProducerTemplateItem

case object SynInvalid extends ModifierProducerTemplateItem
case object Valid extends ModifierProducerTemplateItem

trait CustomModifierProducer[ST <: MinimalState[ST]] {

  def customModifiers(history: ErgoHistory,
                      state: ST,
                      template: Seq[ModifierProducerTemplateItem]): Seq[ErgoPersistentModifier]
}
