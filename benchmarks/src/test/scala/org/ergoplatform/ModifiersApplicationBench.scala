package org.ergoplatform

import java.nio.file.{Files, Paths}

import com.google.common.primitives.Ints
import org.ergoplatform.modifiers.ErgoPersistentModifier
import org.ergoplatform.modifiers.history.{BlockTransactions, Extension, Header, HistoryModifierSerializer}
import org.ergoplatform.nodeView.ErgoModifiersCache
import org.ergoplatform.nodeView.history.ErgoHistory
import org.ergoplatform.nodeView.state.StateType
import org.ergoplatform.settings.CacheSettings
import org.ergoplatform.tools.ChainGenerator
import org.ergoplatform.utils.HistoryTestHelpers

import scala.annotation.tailrec

object ModifiersApplicationBench extends HistoryTestHelpers with App {

  override def main(args: Array[String]): Unit = {

    val cache = new ErgoModifiersCache(maxSize = 1024)

    val headers: Seq[Header] = readModifiers[Header]("benchmarks/src/test/resources/headers.dat")
    val payloads: Seq[BlockTransactions] = readModifiers[BlockTransactions]("benchmarks/src/test/resources/payloads.dat")
    val extensions: Seq[Extension] = readModifiers[Extension]("benchmarks/src/test/resources/extensions.dat")

    def bench(benchCase: String)
             (applicator: (Seq[ErgoPersistentModifier], ErgoHistory) => Any,
              mods: Seq[ErgoPersistentModifier]): String = {
      val preparedHistory = applyModifiers(headers.take(mods.size / 2), unlockedHistory())._1
      val et = time(applicator(mods, preparedHistory))
      s"Performance of `$benchCase`: $et ms"
    }

    def applyModifiersWithCache(mods: Seq[ErgoPersistentModifier], his: ErgoHistory): (ErgoHistory, Int) = {
      mods.foreach(m => cache.put(m.id, m))
      @tailrec def applyLoop(applied: Seq[ErgoPersistentModifier]): Seq[ErgoPersistentModifier] = {
        cache.popCandidate(his) match {
          case Some(mod) =>
            his.append(mod).get
            applyLoop(mod +: applied)
          case None =>
            applied
        }
      }

      val appliedModsQty = applyLoop(Seq()).size
      his -> appliedModsQty
    }

    def applyModifiers(mods: Seq[ErgoPersistentModifier], his: ErgoHistory): (ErgoHistory, Int) = {
      @tailrec def applyLoop(rem: Seq[ErgoPersistentModifier],
                             applied: Seq[ErgoPersistentModifier]): Seq[ErgoPersistentModifier] = {
        rem match {
          case m :: tail =>
            his.applicableTry(m)
            his.append(m)
            applyLoop(tail, m +: applied)
          case Nil =>
            applied
        }
      }

      val appliedModsQty = applyLoop(mods, Seq()).size
      his -> appliedModsQty
    }

    val modifiersDirectOrd = payloads.zip(extensions).flatMap(x => Seq(x._1, x._2)).take(800)
    val modifiersReversedOrd = modifiersDirectOrd.reverse
    val report0 = bench("Modifiers application in direct order")(applyModifiers, modifiersDirectOrd)
    val report1 = bench("Modifiers application in reversed order")(applyModifiers, modifiersReversedOrd)
//    val report2 = bench("Modifiers application in direct order (cache)")(applyModifiersWithCache, modifiersDirectOrd)
//    val report3 = bench("Modifiers application in reversed order (cache)")(applyModifiersWithCache, modifiersReversedOrd)

    println(report0)
    println(report1)
//    println(report2)
//    println(report3)

    System.exit(0)
  }

  def history(): ErgoHistory = generateHistory(verifyTransactions = true, StateType.Utxo,
    PoPoWBootstrap = false, blocksToKeep = -1, cacheSettings = CacheSettings(1000, 1000))

  def unlockedHistory(): ErgoHistory = {
    val h = history()
    ChainGenerator.allowToApplyOldBlocks(h)
    h
  }

  def readModifiers[M <: ErgoPersistentModifier](path: String): Seq[M] = {
    def readMods(rem: Array[Byte], acc: Seq[M] = Seq.empty): Seq[M] = {
      if (rem.nonEmpty) {
        val len = Ints.fromByteArray(rem.take(4))
        val mod = HistoryModifierSerializer.parseBytes(rem.slice(4, 4 + len)).get.asInstanceOf[M]
        readMods(rem.drop(4 + len), acc :+ mod)
      } else {
        acc
      }
    }
    readMods(Files.readAllBytes(Paths.get(path)))
  }

  private def time[R](block: => R): Double = {
    val t0 = System.nanoTime()
    block // call-by-name
    val t1 = System.nanoTime()
    (t1.toDouble - t0) / 1000000
  }

}
