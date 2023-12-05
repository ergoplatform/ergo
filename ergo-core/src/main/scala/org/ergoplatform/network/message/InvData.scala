package org.ergoplatform.network.message

import org.ergoplatform.modifiers.NetworkObjectTypeId
import scorex.util.ModifierId

/**
  * P2P network message which is encoding "inventory", 
  *
  * @param typeId
  * @param ids
  */
case class InvData(typeId: NetworkObjectTypeId.Value, ids: Seq[ModifierId])
