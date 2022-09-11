package org.ergoplatform.modifiers

import io.circe.Encoder
import org.ergoplatform.modifiers.history.extension.Extension
import org.ergoplatform.modifiers.history.header.Header
import org.ergoplatform.modifiers.history.{ADProofs, BlockTransactions}
import scorex.core.{ModifierTypeId, PersistentNodeViewModifier}


/**
  * Block section, so a header, or block transactions, or extension, or ADProofs.
  */
trait BlockSection extends PersistentNodeViewModifier with ErgoNodeViewModifier

object BlockSection {

  implicit val jsonEncoder: Encoder[BlockSection] = {
    case h: Header => Header.jsonEncoder(h)
    case bt: BlockTransactions => BlockTransactions.jsonEncoder(bt)
    case adp: ADProofs => ADProofs.jsonEncoder(adp)
    case ext: Extension => Extension.jsonEncoder(ext)
    case other => throw new Exception(s"Unknown block section type: $other")
  }

  def typeToString(blockSectionTypeId: ModifierTypeId): String = {
    if (blockSectionTypeId == Header.modifierTypeId) {
      "Header"
    } else if (blockSectionTypeId == BlockTransactions.modifierTypeId) {
      "BlockTransactions"
    } else if (blockSectionTypeId == ADProofs.modifierTypeId) {
      "ADProofs"
    } else if (blockSectionTypeId == Extension.modifierTypeId) {
      "Extension"
    } else {
      "unknown!"
    }
  }

}
