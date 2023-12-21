package org.ergoplatform.modifiers

import io.circe.Encoder
import org.ergoplatform.PersistentNodeViewModifier
import org.ergoplatform.modifiers.history.extension.Extension
import org.ergoplatform.modifiers.history.header.Header
import org.ergoplatform.modifiers.history.{ADProofs, BlockTransactions}


/**
  * Block section, so a header, or block transactions, or extension, or ADProofs.
  */
trait BlockSection extends PersistentNodeViewModifier with ErgoNodeViewModifier

object BlockSection {

  implicit val jsonEncoder: Encoder[BlockSection] = Encoder.instance {
    case h: Header => Header.jsonEncoder(h)
    case bt: BlockTransactions => BlockTransactions.jsonEncoder(bt)
    case adp: ADProofs => ADProofs.jsonEncoder(adp)
    case ext: Extension => Extension.jsonEncoder(ext)
    case other => throw new Exception(s"Unknown block section type: $other")
  }

  /** Immutable empty array can be shared to avoid allocations. */
  val emptyArray: Array[BlockSection] = Array.empty[BlockSection]
}
