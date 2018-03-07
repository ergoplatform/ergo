package org.ergoplatform.bench

import java.io.{FileOutputStream, PrintWriter}

import akka.actor.{ActorRef, ActorSystem, Props}
import org.ergoplatform.modifiers.ErgoPersistentModifier
import org.ergoplatform.nodeView.ErgoNodeViewHolder
import org.ergoplatform.nodeView.state.UtxoState
import org.ergoplatform.settings.ErgoSettings
import scorex.core.utils.NetworkTimeProvider
import scorex.crypto.encode.Base58

class BenchmarkErgoNodeViewHolder(settings: ErgoSettings, timeProvider: NetworkTimeProvider, b: BenchmarkConfig)
  extends ErgoNodeViewHolder[UtxoState](settings, timeProvider) {

  var counter = 0
  val writer = new PrintWriter(b.fileToSave)

  val fos = new FileOutputStream(b.fileToSave)

  override protected def pmodModify(pmod: ErgoPersistentModifier): Unit =
    if (counter >= b.modifiersThreshold) {
      log.info("Shutting Down")
      writer.flush()
      writer.close()
      System.exit(0)
    } else {
      counter += 1
      log.info(s"GOT ${counter} modifiers")
      val bytes: Array[Byte] = pmod.bytes
      val typeId: Byte = pmod.modifierTypeId
      writer.println(Base58.encode(typeId +: bytes))
      super.pmodModify(pmod)
    }
}

object BenchmarkErgoNodeViewHolderRef {
  def apply(settings: ErgoSettings, timeProvider: NetworkTimeProvider, b: BenchmarkConfig)
           (implicit system: ActorSystem): ActorRef =
    system.actorOf(props(settings, timeProvider, b))

  def props(settings: ErgoSettings, timeProvider: NetworkTimeProvider, b: BenchmarkConfig): Props =
    Props.create(classOf[BenchmarkErgoNodeViewHolder], settings, timeProvider, b)
}

case class BenchmarkConfig(fileToSave: String = "/", modifiersThreshold: Int = 15000)

case class Result(t: Long, v: Long) extends WritableData {
  override def toDataLine: String = s"$t,$v"
}