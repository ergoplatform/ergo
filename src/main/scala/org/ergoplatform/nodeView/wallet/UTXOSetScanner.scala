package org.ergoplatform.nodeView.wallet

import akka.actor.{Actor, Props}
import org.ergoplatform.ErgoBox
import org.ergoplatform.nodeView.state.UtxoState
import org.ergoplatform.nodeView.wallet.UTXOSetScanner._
import org.ergoplatform.wallet.boxes.ErgoBoxSerializer
import scorex.crypto.authds.avltree.batch.{InternalProverNode, ProverLeaf, ProverNodes}
import scorex.crypto.hash.Digest32
import scorex.util.{ScorexLogging, bytesToId}

import scala.annotation.tailrec
import scala.util.{Failure, Success}

class UTXOSetScanner extends Actor with ScorexLogging {

  private def scanBox(box: ErgoBox): Unit = {
    log.info(s"Scanning box ${bytesToId(box.id)}")
    // perform scan
  }

  private def run(state: UtxoState): Unit = {

    @tailrec
    def walk(rNode: ProverNodes[Digest32], ir: Int): Int = {
      rNode match {
        case leaf: ProverLeaf[Digest32] =>
          leafFn(leaf, ir)

        case r: InternalProverNode[Digest32] =>
          val i = internalNodeFn(r, ir)
          walk(i._1, i._2)
      }
    }

    def internalNodeFn(r: InternalProverNode[Digest32], depth: Int): (ProverNodes[Digest32],Int) =
      (r.left, walk(r.right, depth + 1))

    def leafFn(leaf: ProverLeaf[Digest32], depth: Int): Int = {
      ErgoBoxSerializer.parseBytesTry(leaf.value) match {
        case Success(box) => scanBox(box)
        case Failure(e) => log.error(s"Failed to parse box from state, $e")
      }
      depth + 1
    }

    state.persistentProver.avlProver.treeWalk(internalNodeFn, leafFn, 0)
  }

  override def receive: Receive = {
    case StartScan(state: UtxoState) => run(state)
  }

}

object UTXOSetScanner {

  case class StartScan(state: UtxoState)

  def props(): Props = Props(new UTXOSetScanner())
}
