package org.ergoplatform.nodeView.history

import org.ergoplatform.modifiers.BlockSection
import org.ergoplatform.modifiers.history.HeaderChain
import org.ergoplatform.nodeView.state.StateType
import org.ergoplatform.utils.ErgoCorePropertyTest
import org.ergoplatform.validation.MalformedModifierError
import scorex.db.ByteArrayWrapper

class HeaderParentWriteGapSpecification extends ErgoCorePropertyTest {
  import org.ergoplatform.utils.HistoryTestHelpers._
  import org.ergoplatform.utils.generators.ChainGenerator._

  // HistoryStorage.insert writes a header's object and then its indexes. A child header validated in between finds
  // the parent's object but not its height index; its validity must not depend on that index.
  property("a child of a header whose indexes are not written yet is not judged invalid") {
    var history = generateHistory(verifyTransactions = true, StateType.Utxo, PoPoWBootstrap = false, BlocksToKeep)
    val headers = genHeaderChain(4, history, diffBitsOpt = None, useRealTs = false).headers
    history = applyHeaderChain(history, HeaderChain(headers.take(2)))
    val parent = headers(2)
    val child = headers(3)

    // the state between the two writes: the parent's object stored, none of its indexes
    history.historyStorage.insert(Array.empty[(ByteArrayWrapper, Array[Byte])], Array[BlockSection](parent)).get
    history.typedModifierById[org.ergoplatform.modifiers.history.header.Header](parent.id) shouldBe defined
    history.heightOf(parent.id) shouldBe None

    val result = history.applicableTry(child)
    result.failed.toOption.foreach(e => e should not be a[MalformedModifierError])
    result shouldBe 'success
  }

  // the age check itself still holds: a header whose parent is keepVersions or more below the best full block
  // is rejected by hdrTooOld (a fork too deep to apply)
  property("a header whose parent is keepVersions below the best full block is still rejected as too old") {
    var history = generateHistory(verifyTransactions = true, StateType.Utxo, PoPoWBootstrap = false,
                                  blocksToKeep = -1, epochLength = 10000, useLastEpochs = 3)
    val chain = genChain(203, history)
    history = applyChain(history, chain)
    val keepVersions = org.ergoplatform.utils.ErgoNodeTestConstants.settings.nodeSettings.keepVersions
    val parent = chain.head.header
    (history.fullBlockHeight - parent.height) should be >= keepVersions

    val forkChild = genHeaderChain(1, Some(parent), history.difficultyCalculator, diffBitsOpt = None, useRealTs = false).last
    forkChild.parentId shouldBe parent.id
    val result = history.applicableTry(forkChild)
    result shouldBe 'failure
    result.failed.get shouldBe a[MalformedModifierError]
    result.failed.get.getMessage should include("older than current height minus")
  }
}
