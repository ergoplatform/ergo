package org.ergoplatform.network

import io.circe.{Encoder, Json}
import org.ergoplatform.nodeView.state.StateType
import org.ergoplatform.settings.{ClientCapabilities, PeerFeatureDescriptors}
import scorex.core.network.PeerFeature
import scorex.core.network.PeerFeature.Id
import scorex.core.serialization.ErgoSerializer
import scorex.util.serialization.{Reader, Writer}

/**
  * A peer feature that is storing information on operating mode of the peer. Currently it is storing the following
  * fields, in future more information could be stored without any problems for nodes not upgraded, see
  * ModeFeatureSerializer comments.
  *
  * @param stateType - information on whether UTXO set is store (so state type is UTXO/Digest)
  * @param verifyingTransactions - whether the peer is verifying transactions
  * @param nipopowBootstrapped - whether the peer has bootstrapped via Nipopows,
  *                              and if so, supported bootstrapping options (only one currently)
  * @param blocksToKeep - how many last full blocks the peer is storing
  */
case class ModePeerFeature(stateType: StateType,
                           verifyingTransactions: Boolean,
                           nipopowBootstrapped: Option[Int],
                           blocksToKeep: Int) extends PeerFeature {
  override type M = ModePeerFeature

  override val featureId: Id = PeerFeatureDescriptors.ModeFeatureId

  override def serializer: ErgoSerializer[ModePeerFeature] = ModeFeatureSerializer

  /**
    * @return whether the peer has all the full blocks
    */
  def allBlocksAvailable: Boolean = blocksToKeep == ModePeerFeature.AllBlocksKept


  /**
    * @return whether the peer has all the headers
    */
  def allHeadersAvailable: Boolean = nipopowBootstrapped.isEmpty
}

object ModePeerFeature {

  import io.circe.syntax._

  /**
    * Flag which is indicating NiPoPoW bootstrap mode. Currently there is only one option (suffix proof done
    * according to KMZ17 paper), which does not exclude possibility for more options in future
    */
  val NiPoPoWDefaultFlag = 1

  /**
    * Flag value which is when used as length of blockchain suffix kept locally means that all the full blocks are
    * stored
    */
  val AllBlocksKept = -1

  /**
    * Flag value which is when used as length of blockchain suffix kept locally means that a node was bootstrapped
    * via UTXO set snapshot, so not all the full blocks stored, but at the same time there is no fixed-length suffix
    * as after bootstrapping there is no pruning
    */
  val UTXOSetBootstrapped = -2

  def apply(clientCapabilities: ClientCapabilities): ModePeerFeature = {
    val popowBootstrapped = if (clientCapabilities.nipopowSettings.nipopowBootstrap) {
      Some(NiPoPoWDefaultFlag)
    } else {
      None
    }

    val blocksKept = if (clientCapabilities.utxoSettings.utxoBootstrap) {
      UTXOSetBootstrapped
    } else {
      clientCapabilities.blocksToKeep
    }

    new ModePeerFeature(
      clientCapabilities.stateType,
      clientCapabilities.verifyTransactions,
      popowBootstrapped,
      blocksKept
    )
  }

  implicit val jsonEncoder: Encoder[ModePeerFeature] = { mf: ModePeerFeature =>
    Json.obj(
      "state" -> mf.stateType.toString.asJson,
      "verifyingTransactions" -> mf.verifyingTransactions.asJson,
      "fullBlocksSuffix" -> mf.blocksToKeep.asJson
    )
  }

}

/**
  * When the node is parsing operating mode information from a peer, it allows additional information to be stored there.
  * Please note that the serialized mode information could be no longer than 512 bytes. Please note that serialized
  * handshake which contains mode information (along with other features supported by the peer) has separate length
  * limit provided in settings ("maxHandshakeSize" field in network settings).
  */
object ModeFeatureSerializer extends ErgoSerializer[ModePeerFeature] {

  val MaxSize = 512

  //we use these methods due to absence of getBoolean in Reader atm of writing the code
  private def booleanToByte(bool: Boolean): Byte = if (bool) 1: Byte else 0: Byte

  private def byteToBoolean(byte: Byte): Boolean = if (byte > 0) true else false

  override def serialize(mf: ModePeerFeature, w: Writer): Unit = {
    w.put(mf.stateType.stateTypeCode)
    w.put(booleanToByte(mf.verifyingTransactions))
    w.putOption(mf.nipopowBootstrapped)(_.putInt(_))
    w.putInt(mf.blocksToKeep)
  }

  override def parse(r: Reader): ModePeerFeature = {
    require(r.remaining < MaxSize)

    val stateType = StateType.fromCode(r.getByte())
    val verifyingTransactions = byteToBoolean(r.getByte())
    val popowBootstrapped = r.getOption(r.getInt())
    val blocksToKeep = r.getInt()

    new ModePeerFeature(
      stateType,
      verifyingTransactions,
      popowBootstrapped,
      blocksToKeep
    )
  }

}
