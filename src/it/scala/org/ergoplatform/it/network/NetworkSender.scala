package org.ergoplatform.it.network

import java.io.IOException
import java.net.InetSocketAddress
import java.nio.channels.ClosedChannelException
import java.util.concurrent.atomic.AtomicLong

import io.netty.channel.Channel
import io.netty.channel.group.DefaultChannelGroup
import io.netty.util.concurrent.GlobalEventExecutor
import scorex.util.ScorexLogging

import scala.concurrent.{Future, Promise}

class NetworkSender(chainId: Char, networkNodeName: String, nonce: Long) extends ScorexLogging {

  private val allChannels = new DefaultChannelGroup(GlobalEventExecutor.INSTANCE)
  private val client = new NetworkClient(chainId, networkNodeName, nonce, allChannels)

  def connect(address: InetSocketAddress): Future[Channel] = {
    client.connect(address)
  }

  def send(channel: Channel, messages: Array[Byte]*): Future[Unit] = {
    if (channel.isOpen) {
      val p = Promise[Unit]
      val counter = new AtomicLong(messages.size)

      messages.foreach { msg =>
        channel.write(msg).addListener { f: io.netty.util.concurrent.Future[Void] =>
          if (!f.isSuccess) {
            val cause = Option(f.cause()).getOrElse(new IOException("Can't send a message to the channel"))
            log.error(s"Can't send a message to the channel: $msg", cause)
          }

          if (counter.decrementAndGet() == 0) {
            p.success(())
          }
        }
      }
      channel.flush()

      p.future
    } else {
      Future.failed(new ClosedChannelException)
    }
  }

  def close(): Unit = client.shutdown()
}
