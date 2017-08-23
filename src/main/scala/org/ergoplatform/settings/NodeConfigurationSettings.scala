package org.ergoplatform.settings

import scala.concurrent.duration.FiniteDuration

case class NodeConfigurationSettings(/**
                                       * Keep state root hash only and validate transactions via ADProofs
                                       */
                                     ADState: Boolean,

                                     /**
                                       * Download block transactions and verify them (requires BlocksToKeep == 0)
                                       */
                                     verifyTransactions: Boolean,

                                     /**
                                       * Number of last blocks to keep with transactions and ADproofs,
                                       * for all other blocks only header will be stored.
                                       * Keep all blocks from genesis if negative
                                       */
                                     blocksToKeep: Int,

                                     /**
                                       * Download PoPoW proof on node bootstrap
                                       */
                                     poPoWBootstrap: Boolean,

                                     /**
                                       * Minimal suffix size for PoPoW proof (may be pre-defined constant or settings parameter)
                                       */
                                     minimalSuffix: Int,

                                     /**
                                       * Desired time interval between blocks
                                       */
                                     blockInterval: FiniteDuration,

                                     /**
                                       * EpochLength
                                       */
                                     epochLength: Int)
