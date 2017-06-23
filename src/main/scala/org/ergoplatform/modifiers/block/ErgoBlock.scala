package org.ergoplatform.modifiers.block

import org.ergoplatform.modifiers.transaction.AnyoneCanSpendTransaction
import org.ergoplatform.modifiers.transaction.proposition.AnyoneCanSpendProposition
import scorex.core.PersistentNodeViewModifier
import scorex.core.block.Block

trait ErgoBlock extends PersistentNodeViewModifier[AnyoneCanSpendProposition, AnyoneCanSpendTransaction]
  with Block[AnyoneCanSpendProposition, AnyoneCanSpendTransaction]

