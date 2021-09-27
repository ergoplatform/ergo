package org.ergoplatform.examples

import java.io.File

import org.ergoplatform.nodeView.state.{StateConstants, UtxoState}
import org.ergoplatform.settings.{Algos, Args, ErgoSettings, NetworkType}
import scorex.crypto.authds.avltree.batch.serialization.BatchAVLProverSerializer
import scorex.crypto.hash.Digest32

object UtxoSnapshotExample extends App {
  import Algos.HF

  val stateConstants = StateConstants(None, ErgoSettings.read(Args(Some("/home/kushti/ergo/mainnet/mainnet.conf"), Some(NetworkType.MainNet))))
  val state = UtxoState.create(new File("/home/kushti/ergo/mainnet/.ergo/state"), stateConstants)

  implicit val hf: HF = Algos.hash
  val serializer = new BatchAVLProverSerializer[Digest32, HF]

  val avlProver = state.persistentProver
  val ms0 = System.currentTimeMillis()
  println(avlProver.height)
  val sliced = serializer.slice(avlProver.avlProver, subtreeDepth = 16)
  println("sliced: " + sliced._2)
  val ms = System.currentTimeMillis()
  println("Time: " + (ms - ms0))

}
