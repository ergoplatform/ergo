package org.ergoplatform.mining

import org.ergoplatform.autoleakus.Autoleakus
import org.ergoplatform.autoleakus.pow.ksum.hashBinding.HKSumPowTask
import org.ergoplatform.mining.difficulty.RequiredDifficulty
import org.ergoplatform.modifiers.ErgoFullBlock
import org.ergoplatform.modifiers.history._
import org.ergoplatform.modifiers.mempool.ErgoTransaction
import scorex.core.block.Block.Timestamp
import scorex.crypto.authds.{ADDigest, SerializedAdProof}
import scorex.crypto.hash.Digest32
import scorex.util.ModifierId

import scala.math.BigInt

class AutoleakusPowScheme(k: Int, N: Int) {

  protected val powTask = new HKSumPowTask(k: Int, N: Int)
  protected val autoleakus = new Autoleakus(powTask)

  def verify(header: Header): Boolean = {
    val b = getB(header.nBits)
    val msg = HeaderSerializer.bytesWithoutPow(header)
    autoleakus.verify(header.powSolution.solution(msg), b).isSuccess
  }

  def realDifficulty(header: Header): BigInt = {
    q / header.powSolution.d
  }

  def prove(parentOpt: Option[Header],
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

    val h = Header(version, parentId, interlinks, adProofsRoot, stateRoot, transactionsRoot, timestamp,
      nBits, height, extensionHash, null)
    val msg = HeaderSerializer.bytesWithoutPow(h)
    val b = getB(nBits)
    powTask.initialize(msg, sk)
    powTask.checkNonces(msg, sk, b, minNonce, maxNonce).map(s => h.copy(powSolution = AutoleakusSolution(s)))
  }

  def proveBlock(parentOpt: Option[Header],
                 nBits: Long,
                 stateRoot: ADDigest,
                 adProofBytes: SerializedAdProof,
                 transactions: Seq[ErgoTransaction],
                 timestamp: Timestamp,
                 extensionCandidate: ExtensionCandidate,
                 sk: PrivateKey,
                 minNonce: Long = Long.MinValue,
                 maxNonce: Long = Long.MaxValue): Option[ErgoFullBlock] = {

    val transactionsRoot = BlockTransactions.transactionsRoot(transactions)
    val adProofsRoot = ADProofs.proofDigest(adProofBytes)
    val extensionRoot: Digest32 = Extension.rootHash(extensionCandidate)

    prove(parentOpt, nBits, stateRoot, adProofsRoot, transactionsRoot,
      timestamp, extensionRoot, sk, minNonce, maxNonce).map { h =>
      val adProofs = ADProofs(h.id, adProofBytes)
      val blockTransactions = BlockTransactions(h.id, transactions)
      val extension = Extension(h.id, extensionCandidate.mandatoryFields, extensionCandidate.optionalFields)
      new ErgoFullBlock(h, blockTransactions, extension, Some(adProofs))
    }
  }

  def proveCandidate(candidateBlock: CandidateBlock,
                     sk: PrivateKey,
                     minNonce: Long = Long.MinValue,
                     maxNonce: Long = Long.MaxValue): Option[ErgoFullBlock] = {
    proveBlock(candidateBlock.parentOpt,
      candidateBlock.nBits,
      candidateBlock.stateRoot,
      candidateBlock.adProofBytes,
      candidateBlock.transactions,
      candidateBlock.timestamp,
      candidateBlock.extension,
      sk,
      minNonce,
      maxNonce
    )
  }

  protected def getB(nBits: Long): BigInt = {
    q / RequiredDifficulty.decodeCompactBits(nBits)
  }

  protected def derivedHeaderFields(parentOpt: Option[Header]): (ModifierId, Byte, Seq[ModifierId], Int) = {
    val interlinks: Seq[ModifierId] =
      parentOpt.map(parent => new PoPoWProofUtils(this).constructInterlinkVector(parent)).getOrElse(Seq.empty)

    val height = parentOpt.map(parent => parent.height + 1).getOrElse(0)

    val version = Header.CurrentVersion

    val parentId: ModifierId = parentOpt.map(_.id).getOrElse(Header.GenesisParentId)

    (parentId, version, interlinks, height)
  }


}
