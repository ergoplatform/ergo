package org.ergoplatform.mining

import org.bouncycastle.math.ec.ECPoint
import org.ergoplatform.modifiers.history.Header
import scorex.core.block.Block.Timestamp
import scorex.crypto.authds.ADDigest
import scorex.crypto.hash.Digest32

import scala.math.BigInt
import scala.util.{Random, Success, Try}

/**
  * Fake Pow Scheme for tests.
  * Fill solution with random values, all blocks are valid during validation
  */
object DefaultFakePowScheme extends AutolykosPowScheme(1, 1) {
  override def validate(header: Header): Try[Unit] = Success(Unit)

  override def prove(parentOpt: Option[Header],
                     nBits: Long,
                     stateRoot: ADDigest,
                     adProofsRoot: Digest32,
                     transactionsRoot: Digest32,
                     timestamp: Timestamp,
                     extensionHash: Digest32,
                     sk: PrivateKey,
                     minNonce: Long = Long.MinValue,
                     maxNonce: Long = Long.MaxValue): Option[Header] = {
    val (parentId, version, interlinks, height) = derivedHeaderFields(parentOpt)
    val pk: ECPoint = genPk(sk)
    val w: ECPoint = genPk(Random.nextLong())
    val n: Array[Byte] = Array.fill(8)(0: Byte)
    val d: BigInt = q / (height + 10)
    val s = AutolykosSolution(pk, w, n, d)
    Some(Header(version, parentId, interlinks, adProofsRoot, stateRoot, transactionsRoot, timestamp,
      nBits, height, extensionHash, s))
  }

  override def realDifficulty(header: Header): PrivateKey = header.requiredDifficulty
}