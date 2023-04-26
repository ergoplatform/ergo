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

  /**
    * Threshold for block section type ids
    * Block section could have ids >= this threshold only
    * Other p2p network objects have type id below the threshold
    */
  val BlockSectionThreshold: Value = Value @@ 50.toByte

  /**
    * Whether network object type corresponding to block sections, returns true if so
    */
  def isBlockSection(typeId: Value): Boolean = {
    typeId >= BlockSectionThreshold
  }

}

/**
  * Block section to be sent over the wire (header, transactions section, extension, UTXO set transformation proofs)
  */
sealed trait BlockSectionTypeId extends NetworkObjectTypeId {
  require(value >= BlockSectionThreshold, s"Type id for block section must be >= $BlockSectionThreshold")
}

/**
  * Non-block network objects: unconfirmed transactions, utxo set snapshot related data, nipopow related data etc
  */
sealed trait AuxiliaryTypeId extends NetworkObjectTypeId {
  require(value < BlockSectionThreshold, s"Type id for auxiliary network object must be < DistinguihsingValue")
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
object HeaderTypeId extends BlockSectionTypeId {
  override val value: Value = fromByte(101)
}

/**
  * Block transactions sections. Contains all the transactions for a block.
  */
object BlockTransactionsTypeId extends AuxiliaryTypeId {
  override val value: Value = fromByte(102)
}

/**
  * Block section which contains proofs of correctness for UTXO set transformations.
  * The section contains proofs for all the transformations (i.e. for all the block transactions)
  */
object ProofsTypeId extends BlockSectionTypeId {
  override val value: Value = fromByte(104)
}


/**
  * Block section which contains key-value pairs with different additional data.
  * Interlinks vector (for nipopow proofs) written there, as well as current network parameters
  * (at the beginning of voting epoch), but miners can also put arbitrary data there.
  */
object ExtensionTypeId extends BlockSectionTypeId {
  override val value: Value = fromByte(108)
}

/**
  * Virtual object which is not being sent over the wire rather, constructed locally from different sections
  * got over the wire (header, transactions, extension in the "utxo" mode, those three sections plus proofs in
  * the "digest" mode).
  */
object FullBlockTypeId extends AuxiliaryTypeId {
  override val value: Value = fromByte(-127)
}

/**
  * Not a block section, but a chunk of UTXO set
  */
object UtxoSnapshotChunkTypeId extends AuxiliaryTypeId {
  override val value: Value = fromByte(-126)
}

/**
  * Not a block section, but registry of UTXO set snapshots available
  */
object SnapshotsInfoTypeId extends AuxiliaryTypeId {
  override val value: Value = fromByte(-125)
}
