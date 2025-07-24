package org.ergoplatform.mining

import org.ergoplatform.{AutolykosSolution, OrderingBlockHeaderFound, ProveBlockResult}
import org.ergoplatform.modifiers.history.header.Header
import scorex.crypto.authds.ADDigest
import scorex.crypto.hash.Digest32
import sigma.crypto.EcPointType

import scala.util.{Random, Success, Try}

/**
  * Fake Pow Scheme for tests.
  * Fill solution with random values, as any block is valid during validation
  */
class DefaultFakePowScheme(k: Int, n: Int) extends AutolykosPowScheme(k, n) {
  override def validate(header: Header): Try[Unit] = Success(())

  override def prove(parentOpt: Option[Header],
                     version: Header.Version,
                     nBits: Long,
                     stateRoot: ADDigest,
                     adProofsRoot: Digest32,
                     transactionsRoot: Digest32,
                     timestamp: Header.Timestamp,
                     extensionHash: Digest32,
                     votes: Array[Byte],
                     sk: PrivateKey,
                     minNonce: Long = Long.MinValue,
                     maxNonce: Long = Long.MaxValue): ProveBlockResult = {
    val (parentId, height) = AutolykosPowScheme.derivedHeaderFields(parentOpt)
    val pk: EcPointType = genPk(sk)
    val w: EcPointType = genPk(Random.nextLong())
    val n: Array[Byte] = Array.fill(8)(0: Byte)
    val d: BigInt = q / (height + 10)
    val s = new AutolykosSolution(pk, w, n, d)
    OrderingBlockHeaderFound(Header(version, parentId, adProofsRoot, stateRoot, transactionsRoot, timestamp,
      nBits, height, extensionHash, s, votes, Array.emptyByteArray))
  }

  override def realDifficulty(header: Header): PrivateKey = header.requiredDifficulty

}
