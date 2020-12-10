package org.ergoplatform.wallet.interpreter

import sigmastate.interpreter.{HintsBag, OwnCommitment}

case class TransactionHintsBag(secretHints: Map[Int, HintsBag], publicHints: Map[Int, HintsBag]) {

  /**
    * Replaces hints for transaction input
    * @param index index of the input
    * @param hintsBag - hints for the input
    * @return - updated transaction hints
    */
  def replaceHintsForInput(index: Int, hintsBag: HintsBag): TransactionHintsBag = {
    val (secret, public) = hintsBag.hints.partition(_.isInstanceOf[OwnCommitment])

    TransactionHintsBag(secretHints.updated(index, HintsBag(secret)), publicHints.updated(index, HintsBag(public)))
  }

  /**
    * Add hints for transaction input
    * @param index index of the input
    * @param hintsBag - hints for the input
    * @return - updated transaction hints
    */
  def addHintsForInput(index: Int, hintsBag: HintsBag): TransactionHintsBag = {
    val (secret, public) = hintsBag.hints.partition(_.isInstanceOf[OwnCommitment])
    val oldSecret = this.secretHints.getOrElse(0, HintsBag.empty)
    val oldPublic = this.publicHints.getOrElse(0, HintsBag.empty)

    val newSecret = secretHints.updated(index, HintsBag(secret) ++ oldSecret)
    val newPublic = publicHints.updated(index, HintsBag(public) ++ oldPublic)

    TransactionHintsBag(newSecret, newPublic)
  }

  /**
    * @param index - transaction input index
    * @return both public and secret hints for the input
    */
  def allHintsForInput(index: Int): HintsBag = {
    secretHints.getOrElse(index, HintsBag.empty) ++ publicHints.getOrElse(index, HintsBag.empty)
  }

}

object TransactionHintsBag {

  val empty: TransactionHintsBag = new TransactionHintsBag(Map.empty, Map.empty)

  def apply(mixedHints: Map[Int, HintsBag]): TransactionHintsBag = {
    mixedHints.keys.foldLeft(TransactionHintsBag.empty){ case (thb, idx) =>
      thb.replaceHintsForInput(idx, mixedHints(idx))
    }
  }

}
