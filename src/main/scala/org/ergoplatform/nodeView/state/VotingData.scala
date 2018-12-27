package org.ergoplatform.nodeView.state

import com.google.common.primitives.Ints
import scorex.core.serialization.Serializer

import scala.util.Try

case class VotingData(epochVotes: Array[(Byte, Int)]) {
  def update(voteFor: Byte): VotingData = {
    this.copy(epochVotes = epochVotes.map { case (id, votes) =>
      if (id == voteFor) id -> (votes + 1) else id -> votes
    })
  }

  override def equals(obj: scala.Any): Boolean = obj match {
    case v: VotingData => v.epochVotes.sameElements(this.epochVotes)
    case _ => false
  }
}

object VotingData {
  val empty = VotingData(Array.empty)
}

object VotingDataSerializer extends Serializer[VotingData] {
  override def toBytes(obj: VotingData): Array[Byte] = {
    val votesCount = obj.epochVotes.length.toByte

    val epochVotesBytes =
      if (votesCount > 0) {
        obj.epochVotes.map { case (id, cnt) =>
          id +: Ints.toByteArray(cnt)
        }.reduce(_ ++ _)
      } else {
        Array.emptyByteArray
      }

    votesCount +: epochVotesBytes
  }

  override def parseBytes(bytes: Array[Byte]): Try[VotingData] = Try {
    val votesCount = bytes.head
    val epochVotesBytes = bytes.tail.take(votesCount * 5)
    val epochVotes = epochVotesBytes.grouped(5).toArray.map(bs => bs.head -> Ints.fromByteArray(bs.tail))

    VotingData(epochVotes)
  }
}