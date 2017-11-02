package org.ergoplatform.it.api

import java.net.InetSocketAddress

import scala.concurrent.Future

trait NetworkNodeApi {

  def networkAddress: String
  def networkPort: Int
  def chainId: Char
  def nodeName: String
  def nonce: Long = System.currentTimeMillis()

  def sendByNetwork(message: RawBytes*): Future[Unit] = {
    val sender = new NetworkSender(chainId, nodeName, nonce)
    sender.connect(new InetSocketAddress(networkAddress, networkPort)).map { ch =>
      if (ch.isActive) sender.send(ch, message: _*).map(_ => sender.close()) else sender.close()
    }
  }
}
