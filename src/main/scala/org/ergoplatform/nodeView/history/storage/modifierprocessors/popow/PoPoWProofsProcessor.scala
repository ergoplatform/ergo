package org.ergoplatform.nodeView.history.storage.modifierprocessors.popow

import org.ergoplatform.modifiers.ErgoPersistentModifier
import org.ergoplatform.modifiers.history.{Header, HeaderChain, PoPoWProof}
import org.ergoplatform.nodeView.history.storage.modifierprocessors.HeadersProcessor
import scorex.core.consensus.History.ProgressInfo
import scorex.core.utils.ScorexLogging

import scala.annotation.tailrec
import scala.util.Try

trait PoPoWProofsProcessor extends HeadersProcessor with ScorexLogging {

  def validate(m: PoPoWProof): Try[Unit]

  def process(m: PoPoWProof): ProgressInfo[ErgoPersistentModifier]

  def lastHeaders(count: Int): HeaderChain

  /**
    * Constructs SPV Proof from KLS16 paper
    *
    * @param m - parameter "m" from the paper (minimal length of innerchain to include)
    * @param k - parameter "k" from the paper (chain suffix)
    * @return
    */
  def constructPoPoWProof(m: Int, k: Int): Try[PoPoWProof] = Try {
    val currentHeight = height
    require(m > 0 && m < currentHeight, s"$m > 0 && $m < $currentHeight")
    require(k > 0 && k < currentHeight, s"$k > 0 && $k < $currentHeight")

    val suffix: HeaderChain = lastHeaders(k)
    val firstSuffix = suffix.head


    def headerById(id: Array[Byte]): Header = typedModifierById[Header](id).get

    @tailrec
    def constructProof(i: Int): (Int, Seq[Header]) = {
      @tailrec
      def loop(acc: Seq[Header]): Seq[Header] = {
        val interHeader = acc.head
        if (interHeader.interlinks.length > i) {
          val header = headerById(interHeader.interlinks(i))
          loop(header +: acc)
        } else {
          acc.reverse.tail.reverse
        }
      }

      val innerchain = loop(Seq(firstSuffix))
      if (innerchain.length >= m) (i, innerchain) else constructProof(i - 1)
    }

    val (depth, innerchain) = constructProof(firstSuffix.interlinks.length)

    PoPoWProof(m.toByte, k.toByte, depth.toByte, innerchain, suffix.headers)
  }

}

