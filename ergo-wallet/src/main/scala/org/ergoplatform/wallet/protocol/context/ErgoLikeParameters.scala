package org.ergoplatform.wallet.protocol.context

/**
  * Blockchain parameters readjustable via miners voting and voting-related data.
  * All these fields are included into extension section of a first block of a voting epoch.
  */
trait ErgoLikeParameters {

  /**
    * @return cost of storing 1 byte in UTXO for four years, in nanoErgs
    */
  def storageFeeFactor: Int

  /**
    * @return cost of a transaction output, in computation unit
    */
  def minValuePerByte: Int

  /**
    * @return max block size, in bytes
    */
  def maxBlockSize: Int

  /**
    * @return cost of a token contained in a transaction, in computation unit
    */
  def tokenAccessCost: Int

  /**
    * @return cost of a transaction input, in computation unit
    */
  def inputCost: Int

  /**
    * @return cost of a transaction data input, in computation unit
    */
  def dataInputCost: Int

  /**
    * @return cost of a transaction output, in computation unit
    */
  def outputCost: Int

  /**
    * @return computation units limit per block
    */
  def maxBlockCost: Long

  /**
    * @return height when voting for a soft-fork had been started
    */
  def softForkStartingHeight: Option[Int]

  /**
    * @return votes for soft-fork collected in previous epochs
    */
  def softForkVotesCollected: Option[Int]

  /**
    * @return Protocol version
    */
  def blockVersion: Byte
}
