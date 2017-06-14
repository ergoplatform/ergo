package org.ergoplatform.settings

import scorex.core.block.Block._
import scorex.crypto.encode.Base58

import scala.annotation.tailrec
import scala.util.Try

object Algos {

  def blockIdDifficulty(id: Array[Version]): BigInt = {
    val blockTarget = BigInt(1, id)
    Constants.MaxTarget / blockTarget
  }

}
