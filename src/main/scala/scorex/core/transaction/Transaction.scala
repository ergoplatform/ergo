package scorex.core.transaction

import org.ergoplatform.modifiers.{ModifierTypeId, TransactionTypeId}
import scorex.core.EphemerealNodeViewModifier
import scorex.crypto.hash.Blake2b256
import scorex.util.{ModifierId, bytesToId}


/**
  * A transaction is an atomic state modifier
  */
trait Transaction extends EphemerealNodeViewModifier {
  override val modifierTypeId: ModifierTypeId.Value = TransactionTypeId.value

  val messageToSign: Array[Byte]

  override lazy val id: ModifierId = bytesToId(Blake2b256(messageToSign))
}
