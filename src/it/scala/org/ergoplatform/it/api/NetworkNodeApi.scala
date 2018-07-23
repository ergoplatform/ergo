package org.ergoplatform.it.api

import java.net.InetSocketAddress

import org.ergoplatform.it.network.NetworkSender

import scala.concurrent.{ExecutionContext, Future}

trait NetworkNodeApi {

  def networkAddress: String
  def networkPort: Int
  def networkNodeName: String
  def chainId: Char
  def nonce: Long = System.currentTimeMillis()

  def sendByNetwork(message: Array[Byte]*)(implicit ec: ExecutionContext): Future[Unit] = {
    val sender = new NetworkSender(chainId, networkNodeName, nonce)
    sender.connect(new InetSocketAddress(networkAddress, networkPort)).map { ch =>
      if (ch.isActive) sender.send(ch, message: _*).map(_ => sender.close()) else sender.close()
    }
  }
}
