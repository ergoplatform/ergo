package org.ergoplatform.nodeView.wallet.persistence

import org.ergoplatform.nodeView.wallet.IdUtils._

final case class Balance(id: EncodedBoxId,
                         value: Long,
                         assets: Map[EncodedTokenId, Long])
