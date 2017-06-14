package org.ergoplatform.modifiers

import org.ergoplatform.transaction.proposition.AnyoneCanSpendProposition
import scorex.core.PersistentNodeViewModifier
import scorex.core.block.Block

trait ErgoBlock extends PersistentNodeViewModifier[AnyoneCanSpendProposition, AnyoneCanSpendTransaction]
  with Block[AnyoneCanSpendProposition, AnyoneCanSpendTransaction]
