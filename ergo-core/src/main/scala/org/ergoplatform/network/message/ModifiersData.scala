package org.ergoplatform.network.message

import org.ergoplatform.modifiers.NetworkObjectTypeId
import scorex.util.ModifierId

/**
 * Wrapper for block sections of the same type. Used to send multiple block sections at once ove the wire.
 */
case class ModifiersData(typeId: NetworkObjectTypeId.Value, modifiers: Map[ModifierId, Array[Byte]])
