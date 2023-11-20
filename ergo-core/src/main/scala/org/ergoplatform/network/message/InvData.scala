package org.ergoplatform.network.message

import org.ergoplatform.modifiers.NetworkObjectTypeId
import scorex.util.ModifierId

case class InvData(typeId: NetworkObjectTypeId.Value, ids: Seq[ModifierId])
