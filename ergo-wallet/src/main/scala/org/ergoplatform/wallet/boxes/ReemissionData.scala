package org.ergoplatform.wallet.boxes

import scorex.util.ModifierId

/**
  * Re-emission settings which are needed in order to construct transactions
  * (any of them, except ones using re-emission contract (as this class does not have all the needed data to
  *  obtain re-emission contract. However, it is possible to use re-emission contracts in apps using Ergo Wallet API
  *  by providing re-emission contract from outside).
  */
case class ReemissionData(reemissionNftId: ModifierId, reemissionTokenId: ModifierId)
