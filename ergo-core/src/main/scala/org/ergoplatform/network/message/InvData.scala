package org.ergoplatform.network.message

import org.ergoplatform.modifiers.NetworkObjectTypeId
import scorex.util.ModifierId

/**
  * P2P network message which is encoding "inventory", transactions or block sections the node has
  *
  * @param typeId
  * @param ids
  */
case class InvData(typeId: NetworkObjectTypeId.Value, ids: Seq[ModifierId])
