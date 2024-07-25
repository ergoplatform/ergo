package org.ergoplatform.wallet.interpreter

import org.ergoplatform.sdk.BlockchainParameters
import org.ergoplatform.sdk.wallet.protocol.context.BlockchainStateContext
import scorex.util.encode.Base16
import sigmastate.crypto.CryptoConstants
import sigmastate.eval.Extensions.ArrayOps
import sigmastate.eval.{CGroupElement, CPreHeader}
import sigma.{Coll, Colls, Header, PreHeader}

trait InterpreterSpecCommon {

  protected val parameters = new BlockchainParameters {

    override def storageFeeFactor: Int = 1250000

    override def minValuePerByte: Int = 360

    override def maxBlockSize: Int = 524288

    override def tokenAccessCost: Int = 100

    override def inputCost: Int = 2000

    override def dataInputCost: Int = 100

    override def outputCost: Int = 100

    override def maxBlockCost: Int = 1000000

    override def softForkStartingHeight: Option[Int] = None

    override def softForkVotesCollected: Option[Int] = None

    override def blockVersion: Byte = 1
  }

  protected val stateContext = new BlockchainStateContext {

    override def sigmaLastHeaders: Coll[Header] = Colls.emptyColl

    override def previousStateDigest: Coll[Byte] =
      Base16.decode("a5df145d41ab15a01e0cd3ffbab046f0d029e5412293072ad0f5827428589b9302")
        .getOrElse(throw new Error(s"Failed to parse genesisStateDigest"))
        .toColl

    override def sigmaPreHeader: PreHeader = CPreHeader(
      version = 0,
      parentId = Colls.emptyColl[Byte],
      timestamp = 0,
      nBits = 0,
      height = 0,
      minerPk = CGroupElement(CryptoConstants.dlogGroup.generator),
      votes = Colls.emptyColl[Byte]
    )
  }

}
