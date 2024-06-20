package org.ergoplatform.nodeView.history.extra

import org.ergoplatform.utils.ErgoCorePropertyTest
import scorex.util.ModifierId
import scorex.util.encode.Base16

import scala.collection.mutable.ArrayBuffer
import org.ergoplatform.consensus.ProgressInfo
import org.ergoplatform.mining.AutolykosPowScheme
import org.ergoplatform.modifiers.{BlockSection, NonHeaderBlockSection}
import org.ergoplatform.nodeView.history.ErgoHistoryReader
import org.ergoplatform.nodeView.history.storage.HistoryStorage
import org.ergoplatform.settings.ErgoSettings
import scala.util.{Random, Try}

class SegmentSpec extends ErgoCorePropertyTest {

  property("getSegmentsForRange") {
    val hash = ModifierId @@ Base16.encode(Array.fill(32)(0.toByte))
    val ia = IndexedErgoAddress(hash, new ArrayBuffer[Long](), new ArrayBuffer[Long]())
    println(ia.getSegmentsForRange(1800,200)(512).mkString(", "))
  }

  property("correct slicing in getFromSegments") {
    val hr = new ErgoHistoryReader {
      override protected[history] val historyStorage: HistoryStorage = null
      override protected val settings: ErgoSettings = null

      /**
        * Whether state requires to download adProofs before full block application
        */
      override protected def requireProofs: Boolean = ???

      /**
        * @param m - modifier to process
        * @return ProgressInfo - info required for State to be consistent with History
        */
      override protected def process(m: NonHeaderBlockSection): Try[ProgressInfo[BlockSection]] = ???

      /**
        * @param m - modifier to validate
        * @return Success() if modifier is valid from History point of view, Failure(error) otherwise
        */
      override protected def validate(m: NonHeaderBlockSection): Try[Unit] = ???

      override val powScheme: AutolykosPowScheme = null
    }

    val hash = ModifierId @@ Base16.encode(Array.fill(32)(0.toByte))
    var boxes = new ArrayBuffer[Long]()
    (1 to 1706).foreach {i =>
      boxes.append(i)
    }
    boxes = Random.shuffle(boxes)
    val ia = IndexedErgoAddress(hash, new ArrayBuffer[Long](), boxes)

    implicit val segmentTreshold: Int = 512

    ia.getFromSegments(
      history = hr,
      offset = 0,
      limit = 200,
      segmentCount = 3,
      boxes.take(170),
      idOf = {case (_, i) => ModifierId @@ Base16.encode(Array.fill(32)(i.toByte))},
      arraySelector = null,
      retrieve = {case (arr, _) => arr.toArray}
    )
  }
}
