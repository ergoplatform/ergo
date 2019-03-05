package org.ergoplatform.network

import org.ergoplatform.nodeView.state.StateType
import org.ergoplatform.settings.NodeConfigurationSettings
import scorex.core.network.PeerFeature
import scorex.core.network.PeerFeature.Id
import scorex.core.serialization.ScorexSerializer
import scorex.util.serialization.{Reader, Writer}


case class ModeFeature(stateType: StateType,
                       verifyingTransactions: Boolean,
                       popowBootstrapping: Boolean,
                       popowSuffix: Int,
                       blocksToKeep: Int) extends PeerFeature {
  override type M = ModeFeature

  override val featureId: Id = 16: Byte

  override def serializer: ScorexSerializer[ModeFeature] = ModeFeatureSerializer
}

object ModeFeature {
  def apply(nodeSettings: NodeConfigurationSettings): ModeFeature =
    new ModeFeature(
      nodeSettings.stateType,
      nodeSettings.verifyTransactions,
      nodeSettings.poPoWBootstrap,
      nodeSettings.minimalSuffix,
      nodeSettings.blocksToKeep
    )
}


object ModeFeatureSerializer extends ScorexSerializer[ModeFeature] {

  //we use these methods due to absence of getBoolean in Reader atm of writing the code
  private def booleanToByte(bool: Boolean): Byte = if (bool) 1: Byte else 0: Byte
  private def byteToBoolean(byte: Byte): Boolean = if (byte > 0) true else false
  
  override def serialize(mf: ModeFeature, w: Writer): Unit = {
    w.put(mf.stateType.stateTypeCode)
    w.put(booleanToByte(mf.verifyingTransactions))
    w.put(booleanToByte(mf.popowBootstrapping))
    w.putInt(mf.popowSuffix)
    w.putInt(mf.blocksToKeep)
  }

  override def parse(r: Reader): ModeFeature = {
    require(r.remaining < 512)

    val stateType = StateType.fromCode(r.getByte())
    val verifyingTransactions = byteToBoolean(r.getByte())
    val popowBootstrap = byteToBoolean(r.getByte())
    val popowSuffix = r.getInt()
    val blocksToKeep = r.getInt()

    new ModeFeature(
      stateType,
      verifyingTransactions,
      popowBootstrap,
      popowSuffix,
      blocksToKeep
    )
  }
}
