package scorex.testkit.generators

import org.ergoplatform.nodeView.state.ErgoState

trait AllModifierProducers[ST <: ErgoState[ST]]
  extends SemanticallyValidModifierProducer[ST]
    with SyntacticallyTargetedModifierProducer
    with ArbitraryTransactionsCarryingModifierProducer
    with TotallyValidModifierProducer[ST]
    with SemanticallyValidTransactionsCarryingModifier[ST]