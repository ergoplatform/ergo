package scorex.testkit.generators

import org.ergoplatform.nodeView.state.ErgoState
import org.ergoplatform.testkit.generators.{ArbitraryTransactionsCarryingModifierProducer, SemanticallyValidModifierProducer, SemanticallyValidTransactionsCarryingModifier, SyntacticallyTargetedModifierProducer, TotallyValidModifierProducer}


trait AllModifierProducers[ST <: ErgoState[ST]]
  extends SemanticallyValidModifierProducer[ST]
    with SyntacticallyTargetedModifierProducer
    with ArbitraryTransactionsCarryingModifierProducer
    with TotallyValidModifierProducer[ST]
    with SemanticallyValidTransactionsCarryingModifier[ST]
