package org.ergoplatform.network

import scorex.util.ModifierId

/**
  * Decides how a request for a NiPoPoW proof of the chain up to the current best header is answered.
  *
  * A proof is built at most once per `maxAgeBlocks` blocks and then kept. The kept proof is used while the
  * header it was built for is on the best chain and less than `maxAgeBlocks` blocks below the best header.
  * A build which failed is remembered in the same way, so a node which can not build a proof (it does not keep
  * the extensions of old blocks, which a proof is made of) tries once per `maxAgeBlocks` blocks, not once per
  * request. When a build has not ended `maxAgeBlocks` blocks after it was started, another is started, and
  * what the earlier one builds is not kept.
  *
  * Not thread-safe: to be used from one actor.
  */
final class NipopowTipProofCache(maxAgeBlocks: Int) {

  import NipopowTipProofCache._

  private var kept: Option[Kept] = None
  // the build which is under way, named by the best header at the time it was started
  private var building: Option[Build] = None

  /**
    * @param bestHeaderId - id of the current best header
    * @param bestHeight   - its height
    * @param onBestChain  - whether the header with given id is the best chain's header at given height
    * @return what to do with the request
    */
  def request(bestHeaderId: ModifierId, bestHeight: Int, onBestChain: (ModifierId, Int) => Boolean): Decision = {
    val usable = kept.filter { k =>
      bestHeight >= k.height && bestHeight - k.height < maxAgeBlocks && onBestChain(k.headerId, k.height)
    }
    usable match {
      case Some(k) => k.proofBytes.map(Serve).getOrElse(Unavailable)
      case None if building.exists(b => bestHeight >= b.height && bestHeight - b.height < maxAgeBlocks) => Wait
      case None =>
        val build = Build(bestHeaderId, bestHeight)
        building = Some(build)
        build
    }
  }

  /**
    * To be called when a build asked for by `request` has ended.
    *
    * @param build      - the build which has ended, as `request` returned it
    * @param headerId   - last header of the proof built, which can be a later one than `build` names; if no proof
    *                   could be built, the header `build` names
    * @param height     - its height
    * @param proofBytes - the proof, or None if it could not be built
    * @return whether the result was kept: false if another build was started since, in which case the peers
    *         which wait are to be answered when that one has ended
    */
  def built(build: Build, headerId: ModifierId, height: Int, proofBytes: Option[Array[Byte]]): Boolean = {
    if (building.contains(build)) {
      building = None
      kept = Some(Kept(headerId, height, proofBytes))
      true
    } else {
      false
    }
  }

}

object NipopowTipProofCache {

  private case class Kept(headerId: ModifierId, height: Int, proofBytes: Option[Array[Byte]])

  sealed trait Decision

  /** Answer with the kept proof */
  case class Serve(proofBytes: Array[Byte]) extends Decision

  /** The last build failed and is not to be tried again yet */
  case object Unavailable extends Decision

  /** A build is under way: answer when it has ended */
  case object Wait extends Decision

  /** Build a proof now, then call `built` with this; it names the best header at the time of the request */
  case class Build(headerId: ModifierId, height: Int) extends Decision

}
