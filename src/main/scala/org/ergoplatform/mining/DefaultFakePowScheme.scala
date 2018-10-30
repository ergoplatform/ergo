package org.ergoplatform.mining

import org.bouncycastle.math.ec.ECPoint
import org.ergoplatform.autoleakus.pow.ksum.hashBinding.HKSumNonce
import org.ergoplatform.modifiers.history.Header
import scorex.core.block.Block.Timestamp
import scorex.crypto.authds.ADDigest
import scorex.crypto.hash.Digest32

import scala.math.BigInt

/**
  * Fake Pow Scheme for tests.
  * All blocks are correct here
  */
object DefaultFakePowScheme extends AutoleakusPowScheme(1, 1) {
  override def verify(header: Header): Boolean = true

  override def prove(parentOpt: Option[Header],
                     nBits: Long,
                     stateRoot:
                     ADDigest,
                     adProofsRoot: Digest32,
                     transactionsRoot: Digest32,
                     timestamp: Timestamp,
                     extensionHash: Digest32,
                     sk: PrivateKey,
                     minNonce: Long = Long.MinValue,
                     maxNonce: Long = Long.MaxValue): Option[Header] = {
    val (parentId, version, interlinks, height) = derivedHeaderFields(parentOpt)
    val pk: ECPoint = genPk(sk)
    val w: ECPoint = genPk(1123)
    val n: HKSumNonce = HKSumNonce(Array.fill(8)(0: Byte))
    val s = AutoleakusSolution(pk, w, n: HKSumNonce, 0)
    Some(Header(version, parentId, interlinks, adProofsRoot, stateRoot, transactionsRoot, timestamp,
      nBits, height, extensionHash, s))
  }

  override def realDifficulty(header: Header): BigInt = q

}