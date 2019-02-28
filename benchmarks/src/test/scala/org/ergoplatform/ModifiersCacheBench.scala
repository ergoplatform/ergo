package org.ergoplatform

import java.nio.file.{Files, Paths}

import com.google.common.primitives.Ints
import org.ergoplatform.modifiers.ErgoPersistentModifier
import org.ergoplatform.modifiers.history.{BlockTransactions, Extension, Header, HistoryModifierSerializer}
import org.ergoplatform.nodeView.history.ErgoHistory
import org.ergoplatform.nodeView.state.StateType
import org.ergoplatform.tools.ChainGenerator
import org.ergoplatform.utils.HistoryTestHelpers
import org.scalameter.Bench
import scorex.core.DefaultModifiersCache

import scala.annotation.tailrec

object ModifiersCacheBench extends HistoryTestHelpers with App {

  val headers: Seq[Header] = readModifiers[Header]("benchmarks/src/test/resources/headers.dat")
  val payloads: Seq[BlockTransactions] = readModifiers[BlockTransactions]("benchmarks/src/test/resources/payloads.dat")
  val extensions: Seq[Extension] = readModifiers[Extension]("benchmarks/src/test/resources/extensions.dat")

  val cache = new DefaultModifiersCache[ErgoPersistentModifier, ErgoHistory](maxSize = 1024)

  val historyWithHeaders0 = applyModifiers(headers, unlockedHistory())._1
  val payloadsDirectOrder = time("payloads direct")(applyModifiers(payloads, historyWithHeaders0))
  val payloadsSeqIndirect = payloads.take(100) ++ payloads.slice(100, 110).reverse ++ payloads.drop(110)
  val payloadsIndirectOrder = time("payloads indirect")(applyModifiers(payloadsSeqIndirect, historyWithHeaders0))

  def applyModifiers(mods: Seq[ErgoPersistentModifier], his: ErgoHistory): (ErgoHistory, Int) = {
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

    val a = applyLoop(Seq()).size
    println(a)
    his -> a
  }

  def history(): ErgoHistory = generateHistory(
    verifyTransactions = true, StateType.Utxo, PoPoWBootstrap = false, blocksToKeep = -1)

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

  def time[R](tag: String)(block: => R): R = {
    val t0 = System.nanoTime()
    val result = block    // call-by-name
    val t1 = System.nanoTime()
    val et = (t1.toDouble - t0) / 1000000
    println(s"(:$tag) ET: $et")
    result
  }

}
