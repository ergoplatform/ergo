package org.ergoplatform.nodeView.state

import org.ergoplatform.serialization.ErgoSerializer
import scorex.util.serialization.{Reader, Writer}
import scorex.util.Extensions._

case class VotingData(epochVotes: Array[(Byte, Int)]) {

  def update(voteFor: Byte): VotingData = {
    this.copy(epochVotes = epochVotes.map { case (id, votes) =>
      if (id == voteFor) id -> (votes + 1) else id -> votes
    })
  }

  override def canEqual(that: Any): Boolean = that.isInstanceOf[VotingData]

  override def equals(obj: scala.Any): Boolean = obj match {
    case v: VotingData => v.epochVotes.sameElements(this.epochVotes)
    case _ => false
  }

}

object VotingData {
  val empty = VotingData(Array.empty)
}

object VotingDataSerializer extends ErgoSerializer[VotingData] {

  override def serialize(obj: VotingData, w: Writer): Unit = {
    w.putUShort(obj.epochVotes.length)
    obj.epochVotes.foreach { case (id, cnt) =>
      w.put(id)
      w.putUInt(cnt.toLong)
    }
  }

  override def parse(r: Reader): VotingData = {
    val votesCount = r.getUShort()
    val epochVotes = (0 until votesCount).map {_ =>
      r.getByte() -> r.getUInt().toIntExact
    }
    VotingData(epochVotes.toArray)
  }

}
