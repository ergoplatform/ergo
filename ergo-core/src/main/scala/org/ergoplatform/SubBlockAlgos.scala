package org.ergoplatform

import org.ergoplatform.mining.AutolykosPowScheme
import org.ergoplatform.settings.Parameters

/**
  * Implementation steps:
  * * implement basic input block algorithms (isInput etc)
  * * implement input block network message
  * * implement input block info support in sync tracker
  * * implement downloading input blocks chain
  * * implement avoiding downloading full-blocks
  * * input blocks support in /mining API
  * * sub confirmations API
  */
object SubBlockAlgos {

  // sub blocks per block, adjustable via miners voting
  // todo: likely we need to update rule exMatchParameters (#409) to add new parameter to vote
  val subsPerBlock = Parameters.SubsPerBlockDefault

  lazy val powScheme = new AutolykosPowScheme(32, 26)

}

