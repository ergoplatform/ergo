package org.ergoplatform.consensus

/**
  * Status of a peer's chain relatively to our
  */
sealed trait PeerChainStatus

/**
  * Peer has the same latest reported block as our best block
  */
case object Equal extends PeerChainStatus

/**
  * Peer's best block is in our best chain, but we have continuation of it
  */
case object Younger extends PeerChainStatus

/**
  * Peer has another block on the same height as our best block (and we know a common block)
  */
case object Fork extends PeerChainStatus

/**
  * Peer's chain is seemingly more developed
  */
case object Older extends PeerChainStatus

/**
  * Peer is likely trying to fool us, or its chain is confusing in regards with comparing to our
  */
case object Nonsense extends PeerChainStatus

/**
  * We do not know about peer's chain yet
  */
case object Unknown extends PeerChainStatus
