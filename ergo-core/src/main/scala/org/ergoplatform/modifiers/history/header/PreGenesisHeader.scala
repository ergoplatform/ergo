package org.ergoplatform.modifiers.history.header

import org.ergoplatform.core.idToBytes
import org.ergoplatform.nodeView.history.ErgoHistoryUtils.EmptyHistoryHeight

/**
  * A fake header that is used to fill the chain that starts from the beginning
  */
object PreGenesisHeader extends Header(
  0.toByte,
  parentId = Header.GenesisParentId,
  ADProofsRoot = null,
  stateRoot = null,
  transactionsRoot = null,
  timestamp = 0L,
  nBits = 0L,
  height = EmptyHistoryHeight,
  extensionRoot = null,
  powSolution = null,
  votes = null,
  unparsedBytes = Array.emptyByteArray,
  sizeOpt = None) {

  override def serializedId: Array[Byte] = idToBytes(Header.GenesisParentId)

}
