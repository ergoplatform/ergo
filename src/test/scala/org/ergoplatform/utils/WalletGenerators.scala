package org.ergoplatform.utils

import io.iohk.iodb.ByteArrayWrapper
import org.ergoplatform.modifiers.mempool.ErgoTransaction
import org.ergoplatform.nodeView.history.ErgoHistory.Height
import org.ergoplatform.nodeView.wallet.{BoxSpent, BoxUnspent}
import org.scalacheck.Gen

trait WalletGenerators extends ErgoTransactionGenerators {

  lazy val assetsGen: Gen[Map[ByteArrayWrapper, Long]] = Gen.mapOf(for {
    id <- modifierIdGen
    value <- positiveLongGen
  } yield (ByteArrayWrapper(id), value))

  def boxUnspentGen: Gen[BoxUnspent] = {
    for {
      tx <- invalidErgoTransactionGen
      outIndex <- Gen.choose(0: Short, tx.outputCandidates.length.toShort)
      heightOpt <- Gen.option(smallPositiveInt)
      ergoValue <- positiveLongGen
      assets <- assetsGen
    } yield BoxUnspent(tx, outIndex, heightOpt, ergoValue, assets)
  }

  def boxSpentGen: Gen[BoxSpent] = {
    for {
      box <- ergoBoxGen
      parentTx <- invalidErgoTransactionGen
      spendingTx <- invalidErgoTransactionGen
      heightOpt <- Gen.option(smallPositiveInt)
      heightSpentOpt <- Gen.option(smallPositiveInt)
      ergoValue <- positiveLongGen
      assets <- assetsGen
    } yield BoxSpent(box, parentTx, spendingTx, heightOpt, heightSpentOpt, ergoValue, assets)
  }

}
