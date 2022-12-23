package org.ergoplatform.nodeView.history.extra

import org.ergoplatform.modifiers.ErgoFullBlock
import org.ergoplatform.nodeView.state.StateType
import org.ergoplatform.utils.{ErgoPropertyTest, HistoryTestHelpers}


class ExtraIndexerSpecification extends ErgoPropertyTest with ExtraIndexerBase with HistoryTestHelpers {

  override protected val saveLimit: Int = 1 // save every block

  property("extra indexer rollback") {

    _history = generateHistory(verifyTransactions = true, StateType.Utxo, PoPoWBootstrap = false, BlocksToKeep)
    val chain: Seq[ErgoFullBlock] = genChain(10, _history) // this does not work
    _history = applyChain(_history, chain)

    run()

    // TODO: save balances and numeric indexes

    removeAfter(5)

    // TODO: check balances and numeric indexes

  }

}
