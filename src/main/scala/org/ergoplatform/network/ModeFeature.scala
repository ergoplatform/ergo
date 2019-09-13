package org.ergoplatform.network

import org.ergoplatform.nodeView.state.StateType
import org.ergoplatform.settings.{NodeConfigurationSettings, PeerFeatureIds}
import scorex.core.network.PeerFeature
import scorex.core.network.PeerFeature.Id
import scorex.core.serialization.ScorexSerializer
import scorex.util.serialization.{Reader, Writer}

/**
  * A peer feature that is storing information on operating mode of the peer. Currently it is storing the following
  * fields, in future more information could be stored without any problems for nodes not upgraded, see
  * ModeFeatureSerializer comments.
  *
  * @param stateType - information on whether UTXO set is store (so state type is UTXO/Digest)
  * @param verifyingTransactions - whether the peer is verifying transactions
  * @param blocksToKeep - how many last full blocks the peer is storing
  */
case class ModeFeature(stateType: StateType,
                       verifyingTransactions: Boolean,
                       blocksToKeep: Int) extends PeerFeature {
  override type M = ModeFeature

  override val featureId: Id = PeerFeatureIds.ModeFeatureId

  override def serializer: ScorexSerializer[ModeFeature] = ModeFeatureSerializer
}

object ModeFeature {

  def apply(nodeSettings: NodeConfigurationSettings): ModeFeature =
    ModeFeature(
      nodeSettings.stateType,
      nodeSettings.verifyTransactions,
      nodeSettings.blocksToKeep
    )

}

/**
  * When the node is parsing operating mode information from a peer, it allows additional information to be stored there.
  * Please note that the serialized mode information could be no longer than 512 bytes. Please note that serialized
  * handshake which contains mode information (along with other features supported by the peer) has separate length
  * limit provided in settings ("maxHandshakeSize" field in network settings).
  */
object ModeFeatureSerializer extends ScorexSerializer[ModeFeature] {

  val MaxSize = 512

  //we use these methods due to absence of getBoolean in Reader atm of writing the code
  private def booleanToByte(bool: Boolean): Byte = if (bool) 1: Byte else 0: Byte

  private def byteToBoolean(byte: Byte): Boolean = if (byte > 0) true else false

  override def serialize(mf: ModeFeature, w: Writer): Unit = {
    w.put(mf.stateType.stateTypeCode)
    w.put(booleanToByte(mf.verifyingTransactions))
    w.putInt(mf.blocksToKeep)
  }

  override def parse(r: Reader): ModeFeature = {
    require(r.remaining < MaxSize)

    val stateType = StateType.fromCode(r.getByte())
    val verifyingTransactions = byteToBoolean(r.getByte())
    val blocksToKeep = r.getInt()

    ModeFeature(
      stateType,
      verifyingTransactions,
      blocksToKeep
    )
  }

}
