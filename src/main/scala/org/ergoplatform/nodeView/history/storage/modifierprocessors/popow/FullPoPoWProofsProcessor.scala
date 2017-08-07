package org.ergoplatform.nodeView.history.storage.modifierprocessors.popow

import com.google.common.primitives.Ints
import io.iohk.iodb.ByteArrayWrapper
import org.ergoplatform.modifiers.ErgoPersistentModifier
import org.ergoplatform.modifiers.history.{HistoryModifierSerializer, PoPoWProof}
import org.ergoplatform.nodeView.history.storage.modifierprocessors.HeadersProcessor
import org.ergoplatform.settings.Constants
import scorex.core.consensus.History.ProgressInfo

import scala.util.{Failure, Success, Try}

trait FullPoPoWProofsProcessor extends PoPoWProofsProcessor with HeadersProcessor {

  def validate(m: PoPoWProof): Try[Unit] = m.validate.map { _ =>
    //TODO what if we trying to apply better popow proof?
    //TODO validate difficulty for suffix
    if (height > 1) Failure(new Error("Trying to apply PoPoW proof to nonempty history"))
    else Success()
  }

  def process(m: PoPoWProof): ProgressInfo[ErgoPersistentModifier] = {
    val headers = m.innerchain ++ m.suffix
    val bestHeader = m.suffix.last
    val headersRows: Seq[(ByteArrayWrapper, ByteArrayWrapper)] = headers.zipWithIndex.flatMap { case (h, i) =>
      //TODO howto?
      val requiredDifficulty: BigInt = Constants.InitialDifficulty
      Seq((ByteArrayWrapper(h.id), ByteArrayWrapper(HistoryModifierSerializer.toBytes(h))),
        //TODO howto?
        (headerHeightKey(h.id), ByteArrayWrapper(Ints.toByteArray(2 + i))),
        //TODO howto?
        (headerScoreKey(h.id), ByteArrayWrapper((requiredDifficulty * (1 + i)).toByteArray)),
        (headerDiffKey(h.id), ByteArrayWrapper(requiredDifficulty.toByteArray)))
    }
    val bestHeaderRow = (BestHeaderKey, ByteArrayWrapper(bestHeader.id))
    historyStorage.insert(bestHeader.id, bestHeaderRow +: headersRows)

    ProgressInfo(None, Seq(), Seq(m.suffix.last))
  }
}

