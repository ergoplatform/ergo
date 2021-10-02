package scorex.testkit.generators

import scorex.core.transaction.state.MinimalState


trait AllModifierProducers[ST <: MinimalState[ST]]
  extends SemanticallyValidModifierProducer[ST]
    with SyntacticallyTargetedModifierProducer
    with ArbitraryTransactionsCarryingModifierProducer
    with TotallyValidModifierProducer[ST]
    with SemanticallyValidTransactionsCarryingModifier[ST]
