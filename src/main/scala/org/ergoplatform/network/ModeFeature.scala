package org.ergoplatform.network

import com.google.common.primitives.Ints
import org.ergoplatform.nodeView.state.StateType
import org.ergoplatform.settings.NodeConfigurationSettings
import scorex.core.network.PeerFeature
import scorex.core.network.PeerFeature.Id
import scorex.core.serialization.Serializer
import scorex.core.utils

import scala.util.Try

case class ModeFeature(stateType: StateType,
                       verifyingTransactions: Boolean,
                       popowBootstrapping: Boolean,
                       popowSuffix: Int,
                       blocksToKeep: Int) extends PeerFeature {
  override type M = ModeFeature

  override val featureId: Id = 16: Byte

  def booleanToByte(bool: Boolean): Byte = if (bool) 1: Byte else 0: Byte

  def byteToBoolean(byte: Byte): Boolean = if (byte > 0) true else false

  override def serializer: Serializer[ModeFeature] = new Serializer[ModeFeature] {
    override def toBytes(mf: ModeFeature): Array[Id] = {

      val stateTypeByte = mf.stateType.stateTypeCode
      val verifyingTransactionsByte = booleanToByte(mf.verifyingTransactions)

      val popowBootstrap = booleanToByte(mf.popowBootstrapping)

      utils.concatBytes(Seq(
        Array(stateTypeByte, verifyingTransactionsByte, popowBootstrap),
        Ints.toByteArray(mf.popowSuffix),
        Ints.toByteArray(mf.blocksToKeep)
      ))
    }

    override def parseBytes(bytes: Array[Id]): Try[ModeFeature] = Try {
      require(bytes.length < 512)

      val stateTypeByte = bytes(0)
      val verifyingTransactions = bytes(1)
      val popowBootstrap = bytes(2)

      val popowSuffixBytes = bytes.slice(3, 7)
      val blocksToKeepBytes = bytes.slice(7, 11)

      val stateType = StateType.fromCode(stateTypeByte)

      new ModeFeature(
        stateType,
        byteToBoolean(verifyingTransactions),
        byteToBoolean(popowBootstrap),
        Ints.fromByteArray(popowSuffixBytes),
        Ints.fromByteArray(blocksToKeepBytes)
      )
    }
  }
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
