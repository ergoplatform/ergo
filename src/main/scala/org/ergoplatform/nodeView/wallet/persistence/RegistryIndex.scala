package org.ergoplatform.nodeView.wallet.persistence

import org.ergoplatform.ErgoBox.BoxId
import scorex.util.ModifierId

final case class RegistryIndex(balance: Long, tokensBalance: Seq[(ModifierId, Long)], uncertainBoxes: Seq[BoxId])
