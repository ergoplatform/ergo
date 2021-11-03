package scorex.crypto.authds.avltree.batch

import java.io.File

import org.ergoplatform.nodeView.state.{StateConstants, UtxoState}
import org.ergoplatform.settings.{Algos, Args, ErgoSettings, NetworkType}
import scorex.crypto.authds.avltree.batch.serialization.BatchAVLProverSerializer
import scorex.crypto.hash
import scorex.crypto.hash.Digest32

object UtxoSnapshotExample extends App {
  import Algos.HF

  /**
    * Count number of leaves and their cumulative size (so total weight of boxes)
    *
    * @param prover
    * @return
    */
  private def leafDataStats(prover: BatchAVLProver[hash.Digest32, HF]): (Long, Long) = {

    def step(node: ProverNodes[Digest32], acc: (Long, Long)): (Long, Long) = {
      node match {
        case in: InternalProverNode[Digest32] =>
          val leftRes = step(in.left, acc)
          val rightRes = step(in.right, acc)
          (leftRes._1 + rightRes._1, leftRes._2 + rightRes._2)
        case l: ProverLeaf[Digest32] =>
          (1, l.value.size)
      }
    }

    step(prover.topNode, (0, 0))
  }

  val stateConstants = StateConstants(None, ErgoSettings.read(Args(Some("/home/kushti/ergo/mainnet/mainnet.conf"), Some(NetworkType.MainNet))))

  println("Init started")
  val ims0 = System.currentTimeMillis()
  val state = UtxoState.create(new File("/home/kushti/ergo/mainnet/.ergo/state"), stateConstants)
  val ims = System.currentTimeMillis()
  println("Init took: " + (ims-ims0) + " ms.")

  implicit val hf: HF = Algos.hash
  val serializer = new BatchAVLProverSerializer[Digest32, HF]

  val (elems, valSize) = leafDataStats(state.persistentProver.prover())

  println("elems: " + elems)

  println("boxes total bytes count: " + valSize)

  println("height: " + state.persistentProver.height)

  val avlProver = state.persistentProver
  val ms0 = System.currentTimeMillis()
  val sliced = serializer.slice(avlProver.avlProver, subtreeDepth = 16)
  println("sliced: " + sliced._2)
  val ms = System.currentTimeMillis()
  println("Time: " + (ms - ms0))

}
