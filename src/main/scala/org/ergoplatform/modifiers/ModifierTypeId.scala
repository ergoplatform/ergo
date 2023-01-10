package org.ergoplatform.modifiers

import supertagged.TaggedType
import ModifierTypeId._

sealed trait ModifierTypeId {
  val value: ModifierTypeId.Value
}

object ModifierTypeId {
  object Value extends TaggedType[Byte]
  type Value = Value.Type

  @inline
  def fromByte(value: Byte): Value = Value @@ value
}

object TransactionTypeId extends ModifierTypeId {
  override val value: Value = fromByte(2)
}

object HeaderTypeId extends ModifierTypeId {
  override val value: Value = fromByte(101)
}

object BlockTransactionsTypeId extends ModifierTypeId {
  override val value: Value = fromByte(102)
}

object ProofsTypeId extends ModifierTypeId {
  override val value: Value = fromByte(104)
}

object ExtensionTypeId extends ModifierTypeId {
  override val value: Value = fromByte(108)
}

object FullBlockTypeId extends ModifierTypeId {
  override val value: Value = fromByte(-127)
}

object UtxoSnapshotChunkTypeId extends ModifierTypeId {
  override val value: Value = fromByte(-126)
}

object SnapshotsInfoTypeId extends ModifierTypeId {
  override val value: Value = fromByte(-125)
}
