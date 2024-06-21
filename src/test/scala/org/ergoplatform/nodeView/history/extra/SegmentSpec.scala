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
import scala.util.{Try}

class SegmentSpec extends ErgoCorePropertyTest {

  property("getSegmentsForRange") {
    val hash = ModifierId @@ Base16.encode(Array.fill(32)(0.toByte))
    val ia = IndexedErgoAddress(hash, new ArrayBuffer[Long](), new ArrayBuffer[Long]())
    println(ia.getSegmentsForRange(1800,200)(512).mkString(", "))
  }

  property("correct slicing in getFromSegments") {
    implicit val segmentTreshold: Int = 512

    val hash = ModifierId @@ Base16.encode(Array.fill(32)(0.toByte))
    val boxes = new ArrayBuffer[Long]()
    (1 to 1706).foreach{i =>
      boxes.append(i)
    }

    val segment0 = IndexedErgoAddress(hash, new ArrayBuffer[Long](), boxes.take(segmentTreshold))
    val segment1 = IndexedErgoAddress(hash, new ArrayBuffer[Long](), boxes.slice(segmentTreshold, segmentTreshold * 2))
    val segment2 = IndexedErgoAddress(hash, new ArrayBuffer[Long](), boxes.slice(segmentTreshold * 2, segmentTreshold * 3))
    val ia = IndexedErgoAddress(hash, new ArrayBuffer[Long](), boxes.slice(segmentTreshold * 3, boxes.size).reverse)

    val hr = new ErgoHistoryReader {
      override protected[history] val historyStorage: HistoryStorage = new HistoryStorage(null, null, null ,null) {
        override def getExtraIndex(id: ModifierId): Option[ExtraIndex] = {
          val b = Base16.decode(id).get.head
          if(b == 1) {
            Some(segment2)
          } else if(b == 2) {
            Some(segment1)
          } else if(b == 3) {
            Some(segment0)
          } else {
            ???
          }
        }
      }

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

    def getFromSegments(offset: Int, limit: Int) = {
      ia.getFromSegments(
        history = hr,
        offset,
        limit,
        segmentCount = 3,
        ia.boxes,
        idOf = {
          case (_, i) => ModifierId @@ Base16.encode(Array.fill(32)(i.toByte))
        },
        arraySelector = { ai => ai.boxes },
        retrieve = {
          case (arr, _) => arr.toArray
        }
      )
    }

    val lim = 200

    //getFromSegments(3 * lim, lim).distinct.length shouldBe lim


    val a1 = getFromSegments(0, lim)
    val a2 = getFromSegments(lim, lim)
    val a3 = getFromSegments(2 * lim, lim)
    val a4 = getFromSegments(3 * lim, lim)
    val a5 = getFromSegments(4 * lim, lim)
    val a6 = getFromSegments(5 * lim, lim)
    val a7 = getFromSegments(6 * lim, lim)
    val a8 = getFromSegments(7 * lim, lim)
    val a9 = getFromSegments(8 * lim, lim)
    val a10 = getFromSegments(9 * lim, lim)

    a1.distinct.length shouldBe(lim)
    a2.distinct.length shouldBe(lim)
    a3.distinct.length shouldBe(lim)
    a4.distinct.length shouldBe(lim)

    ((a2 ++ a4).distinct.length shouldBe lim*2)

    a5.distinct.length shouldBe(lim)

    ((a3 ++ a5).distinct.length shouldBe lim*2)

    a6.distinct.length shouldBe(lim)
    a7.distinct.length shouldBe(lim)

    (a1 ++ a2 ++ a3 ++ a4 ++ a5 ++ a6 ++ a7).distinct.length shouldBe(1400)

    a8.distinct.length shouldBe(lim)
    a9.distinct.length shouldBe(106)
    a10.distinct.length shouldBe(0)


    (a1 ++ a2 ++ a3 ++ a4 ++ a5 ++ a6 ++ a7 ++ a8 ++ a9).distinct.length shouldBe(1706)

  }
}
