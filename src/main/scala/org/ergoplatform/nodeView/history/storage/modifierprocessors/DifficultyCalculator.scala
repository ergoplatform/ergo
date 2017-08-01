package org.ergoplatform.nodeView.history.storage.modifierprocessors

import com.sun.xml.internal.messaging.saaj.packaging.mime.Header

import scala.concurrent.duration.FiniteDuration

class DifficultyCalculator {

  //TODO
  def recalculationRequired(height: Int): Boolean = false

  //TODO
  def calculate(headersToRecalculate: Seq[Header], desired: FiniteDuration): BigInt = ???

}
