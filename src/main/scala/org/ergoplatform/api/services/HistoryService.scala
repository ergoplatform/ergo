package org.ergoplatform.api.services

import org.ergoplatform.modifiers.{ErgoFullBlock, ErgoPersistentModifier}
import org.ergoplatform.modifiers.history.{Header, HeaderChain}

import scala.concurrent.Future


trait HistoryService {

  def getHeight: Future[Int]

  def getBestHeader: Future[Option[Header]]

  def getBestFullBlock: Future[Option[ErgoFullBlock]]

  def getLastHeaders(n: Int): Future[HeaderChain]

  def getModifierById(id: String): Future[Option[ErgoPersistentModifier]]

  def getCurrentDifficulty: Future[BigInt]

}
