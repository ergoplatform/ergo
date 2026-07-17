package org.ergoplatform.network.message.inputblocks

object InputBlockMessageLimits {

  /**
    * Maximum allowed count for array and sequence allocations during input-block message parsing.
    */
  val MaxArraySize: Long = 32768L

  def requireArraySize(count: Long, label: String): Unit = {
    require(count <= MaxArraySize, s"$label count too large: $count")
  }

}
