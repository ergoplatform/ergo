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
}
