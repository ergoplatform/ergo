package org.ergoplatform.settings

case class VotingSettings(votingLength: Int,
                          softForkEpochs: Int,
                          activationEpochs: Int,
                          version2ActivationHeight: Int,
                          version2ActivationDifficultyHex: String) {

  def softForkApproved(votesCount: Int): Boolean = votesCount > votingLength * softForkEpochs * 9 / 10

  def changeApproved(count: Int): Boolean = count > votingLength / 2
}
