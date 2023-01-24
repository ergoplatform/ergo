package org.ergoplatform.modifiers

import supertagged.TaggedType
import NetworkObjectTypeId._

/**
  * Hierarchy encoding blockchain objects being sent over wire: block sections, chunks of UTXO set snapshot etc
  */
sealed trait NetworkObjectTypeId {
  /**
    * 1-byte ID of network object type
    */
  val value: NetworkObjectTypeId.Value
}

object NetworkObjectTypeId {
  object Value extends TaggedType[Byte]
  type Value = Value.Type

  @inline
  def fromByte(value: Byte): Value = Value @@ value
}

/**
  * Unconfirmed transactions sent outside blocks
  */
object TransactionTypeId extends NetworkObjectTypeId {
  override val value: Value = fromByte(2)
}

/**
  * Block header, section of a block PoW is done on top of. This section is committing to other sections
  */
object HeaderTypeId extends NetworkObjectTypeId {
  override val value: Value = fromByte(101)
}

/**
  * Block transactions sections. Contains all the transactions for a block.
  */
object BlockTransactionsTypeId extends NetworkObjectTypeId {
  override val value: Value = fromByte(102)
}

/**
  * Block section which contains proofs of correctness for UTXO set transformations.
  * The section contains proofs for all the transformations (i.e. for all the block transactions)
  */
object ProofsTypeId extends NetworkObjectTypeId {
  override val value: Value = fromByte(104)
}


/**
  * Block section which contains key-value pairs with different additional data.
  * Interlinks vector (for nipopow proofs) written there, as well as current network parameters
  * (at the beginning of voting epoch), but miners can also put arbitrary data there.
  */
object ExtensionTypeId extends NetworkObjectTypeId {
  override val value: Value = fromByte(108)
}

/**
  * Virtual object which is not being sent over the wire rather, constructed locally from different sections
  * got over the wire (header, transactions, extension in the "utxo" mode, those three sections plus proofs in
  * the "digest" mode).
  */
object FullBlockTypeId extends NetworkObjectTypeId {
  override val value: Value = fromByte(-127)
}

/**
  * Not a block section, but a chunk of UTXO set
  */
object UtxoSnapshotChunkTypeId extends NetworkObjectTypeId {
  override val value: Value = fromByte(-126)
}

/**
  * Not a block section, but registry of UTXO set snapshots available
  */
object SnapshotsInfoTypeId extends NetworkObjectTypeId {
  override val value: Value = fromByte(-125)
}
