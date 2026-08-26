package org.ergoplatform.network.message.inputblocks

import org.ergoplatform.mining.AutolykosPowScheme
import org.ergoplatform.modifiers.history.extension.ExtensionCandidate
import org.ergoplatform.modifiers.history.header.Header
import org.ergoplatform.modifiers.mempool.ErgoTransaction
import scorex.util.ModifierId

/**
  * Ordering block announcement data
  * @param version - message version (to allow injection of new fields)
  * @param header - ordering block header
  * @param nonBroadcastedTransactions - transactions which were not broadcasted by miner (like emission and fee but could be arb)
  * @param broadcastedTransactionIds - ids of ordering block transactions which were broadcasted previously
  * @param extensionFields - all the extension block section values
  * @param unparsedBytes - bytes of fields added in future versions of the protocol and not parseable (for forward compatibility)
  */
case class OrderingBlockAnnouncement(version: Byte,
                                     header: Header,
                                     nonBroadcastedTransactions: Seq[ErgoTransaction],
                                     broadcastedTransactionIds: Seq[ModifierId],
                                     extensionFields: Seq[(Array[Byte], Array[Byte])],
                                     unparsedBytes: Array[Byte] = Array.emptyByteArray) {

  def valid(powScheme: AutolykosPowScheme,
            expectedNBits: Option[Long] = None): Boolean = {
    val extValid = ExtensionCandidate(extensionFields).digest.sameElements(header.extensionRoot)
    val nBitsValid = expectedNBits.forall(header.nBits == _)
    powScheme.validate(header).isSuccess && extValid && nBitsValid
  }
}

object OrderingBlockAnnouncement {
  /**
    * Current protocol version for OrderingBlockAnnouncement messages
    */
  val CurrentVersion: Byte = 1.toByte
}
