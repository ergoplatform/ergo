package org.ergoplatform.http.api

import org.ergoplatform.modifiers.history.header.Header
import org.ergoplatform.nodeView.history.extra.IndexedErgoTransaction

case class IndexedBlock(
  header: Header,
  transactions: Seq[IndexedErgoTransaction],
  height: Int,
  size: Int
)
