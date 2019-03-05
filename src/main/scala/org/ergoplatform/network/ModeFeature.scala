package org.ergoplatform.network

import org.ergoplatform.nodeView.state.StateType
import org.ergoplatform.settings.NodeConfigurationSettings
import scorex.core.network.PeerFeature
import scorex.core.network.PeerFeature.Id
import scorex.core.serialization.ScorexSerializer
import scorex.util.serialization.{Reader, Writer}


case class ModeFeature(stateType: StateType,
                       verifyingTransactions: Boolean,
                       popowSuffix: Option[Int],
                       blocksToKeep: Int) extends PeerFeature {
  override type M = ModeFeature

  override val featureId: Id = 16: Byte

  override def serializer: ScorexSerializer[ModeFeature] = ModeFeatureSerializer
}

object ModeFeature {
  def apply(nodeSettings: NodeConfigurationSettings): ModeFeature = {
    val popowSuffix = if (nodeSettings.poPoWBootstrap) Some(nodeSettings.minimalSuffix) else None

    new ModeFeature(
      nodeSettings.stateType,
      nodeSettings.verifyTransactions,
      popowSuffix,
      nodeSettings.blocksToKeep
    )
  }
}


object ModeFeatureSerializer extends ScorexSerializer[ModeFeature] {

  //we use these methods due to absence of getBoolean in Reader atm of writing the code
  private def booleanToByte(bool: Boolean): Byte = if (bool) 1: Byte else 0: Byte

  private def byteToBoolean(byte: Byte): Boolean = if (byte > 0) true else false

  override def serialize(mf: ModeFeature, w: Writer): Unit = {
    w.put(mf.stateType.stateTypeCode)
    w.put(booleanToByte(mf.verifyingTransactions))
    w.putOption(mf.popowSuffix)(_.putInt(_))
    w.putInt(mf.blocksToKeep)
  }

  override def parse(r: Reader): ModeFeature = {
    require(r.remaining < 512)

    val stateType = StateType.fromCode(r.getByte())
    val verifyingTransactions = byteToBoolean(r.getByte())
    val popowSuffix = r.getOption(r.getInt())
    val blocksToKeep = r.getInt()

    new ModeFeature(
      stateType,
      verifyingTransactions,
      popowSuffix,
      blocksToKeep
    )
  }
}
