package org.ergoplatform.nodeView

import java.io.File

import akka.actor.{ActorRef, Props}
import io.iohk.iodb.ByteArrayWrapper
import org.ergoplatform.modifiers.ErgoFullBlock
import org.ergoplatform.modifiers.history.{DefaultFakePowScheme, Header}
import org.ergoplatform.nodeView.history.ErgoHistory
import org.ergoplatform.nodeView.mempool.ErgoMemPool
import org.ergoplatform.nodeView.state.{DigestState, ErgoState}
import org.ergoplatform.nodeView.wallet.ErgoWallet
import org.ergoplatform.settings.ErgoSettings
import org.ergoplatform.utils.{ErgoGenerators, SequentialAkkaFixture}
import scorex.core.LocalInterface.LocallyGeneratedModifier
import scorex.core.NodeViewHolder
import scorex.core.NodeViewHolder.EventType._
import scorex.core.NodeViewHolder.{GetDataFromCurrentView, SuccessfulModification}
import scorex.testkit.utils.FileUtils

class DigestErgoNodeViewHolderSpecification extends SequentialAkkaFixture with ErgoGenerators {

  class DigestHolderFixture extends AkkaFixture with FileUtils {
    val dir: File = createTempDir
    val defaultSettings: ErgoSettings = ErgoSettings.read(None).copy(directory = dir.getAbsolutePath)
    val settings: ErgoSettings = defaultSettings.copy(
      nodeSettings = defaultSettings.nodeSettings.copy(ADState = true),
      chainSettings = defaultSettings.chainSettings.copy(poWScheme = DefaultFakePowScheme)
    )
    val digestHolder: ActorRef = system.actorOf(Props(classOf[DigestErgoNodeViewHolder], settings))
  }

  override type Fixture = DigestHolderFixture

  override def createAkkaFixture(): Fixture = new DigestHolderFixture

  property("genesis - state digest") { fixture =>
    import fixture._
     digestHolder ! GetDataFromCurrentView[ErgoHistory, DigestState, ErgoWallet, ErgoMemPool, Boolean] { v =>
      v.state.rootHash.sameElements(ErgoState.afterGenesisStateDigest)
    }
    expectMsg(true)
  }

  property("genesis - history (no genesis block there yet)") { fixture => import fixture._
    digestHolder ! GetDataFromCurrentView[ErgoHistory, DigestState, ErgoWallet, ErgoMemPool, Option[Header]] { v =>
      v.history.bestHeaderOpt
    }
    expectMsg(None)
  }

  property("genesis - apply valid block header") { fixture =>
    import fixture._
    val dir = createTempDir
    val (us, bh) = ErgoState.generateGenesisUtxoState(dir)
    val block = validFullBlock(None, us, bh)

    digestHolder ! GetDataFromCurrentView[ErgoHistory, DigestState, ErgoWallet, ErgoMemPool, Option[Header]] { v =>
      v.history.bestHeaderOpt
    }
    expectMsg(None)

    digestHolder ! GetDataFromCurrentView[ErgoHistory, DigestState, ErgoWallet, ErgoMemPool, Int] { v =>
      v.history.height
    }
    expectMsg(-1)

    digestHolder ! NodeViewHolder.Subscribe(Seq(SuccessfulPersistentModifier, FailedPersistentModifier))

    //sending header
    digestHolder ! LocallyGeneratedModifier[Header](block.header)

    expectMsg(SuccessfulModification(block.header, None))

    digestHolder ! GetDataFromCurrentView[ErgoHistory, DigestState, ErgoWallet, ErgoMemPool, Int] { v =>
      v.history.height
    }
    expectMsg(0)

    digestHolder ! GetDataFromCurrentView[ErgoHistory, DigestState, ErgoWallet, ErgoMemPool, Option[Int]] { v =>
      v.history.heightOf(block.header.id)
    }
    expectMsg(Some(0))


    digestHolder ! GetDataFromCurrentView[ErgoHistory, DigestState, ErgoWallet, ErgoMemPool, Int] { v =>
      v.history.lastHeaders(10).size
    }
    expectMsg(1)

    digestHolder ! GetDataFromCurrentView[ErgoHistory, DigestState, ErgoWallet, ErgoMemPool, Seq[ByteArrayWrapper]] { v =>
      v.history.openSurfaceIds().map(ByteArrayWrapper.apply)
    }
    expectMsg(Seq(ByteArrayWrapper(block.header.id)))

    digestHolder ! GetDataFromCurrentView[ErgoHistory, DigestState, ErgoWallet, ErgoMemPool, Option[Header]] { v =>
      v.history.bestHeaderOpt
    }
    expectMsg(Some(block.header))
  }


  property("apply valid full block as genesis") { fixture =>
    import fixture._
    val dir = createTempDir
    val (us, bh) = ErgoState.generateGenesisUtxoState(dir)
    val genesis = validFullBlock(parentOpt = None, us, bh)
    digestHolder ! NodeViewHolder.Subscribe(Seq(SuccessfulPersistentModifier, FailedPersistentModifier))

    digestHolder ! LocallyGeneratedModifier(genesis.header)
    expectMsg(SuccessfulModification(genesis.header, None))

    digestHolder ! LocallyGeneratedModifier(genesis.blockTransactions)
   //   expectMsg(SuccessfulModification(genesis.blockTransactions, None))  todo: why no message?

    digestHolder ! LocallyGeneratedModifier(genesis.aDProofs.get)
    expectMsg(SuccessfulModification(genesis.aDProofs.get, None))

    digestHolder ! GetDataFromCurrentView[ErgoHistory, DigestState, ErgoWallet, ErgoMemPool, Option[ErgoFullBlock]] { v =>
      v.history.bestFullBlockOpt
    }
    expectMsg(Some(genesis))
  }

  property("apply full blocks after genesis") { fixture =>
    import fixture._
    val dir = createTempDir
    val (us, bh) = ErgoState.generateGenesisUtxoState(dir)
    val genesis = validFullBlock(parentOpt = None, us, bh)
    val wusAfterGenesis = WrappedUtxoState(us, bh).applyModifier(genesis).get

    //digestHolder ! NodeViewHolder.Subscribe(Seq(SuccessfulPersistentModifier, FailedPersistentModifier))

    digestHolder ! LocallyGeneratedModifier(genesis.header)
    digestHolder ! LocallyGeneratedModifier(genesis.blockTransactions)
    digestHolder ! LocallyGeneratedModifier(genesis.aDProofs.get)

    val block = validFullBlock(Some(genesis.header), wusAfterGenesis)

    digestHolder ! LocallyGeneratedModifier(block.header)
    digestHolder ! LocallyGeneratedModifier(block.blockTransactions)
    digestHolder ! LocallyGeneratedModifier(block.aDProofs.get)


    digestHolder ! GetDataFromCurrentView[ErgoHistory, DigestState, ErgoWallet, ErgoMemPool, Option[ErgoFullBlock]] { v =>
      v.history.bestFullBlockOpt
    }
    expectMsg(Some(block))

    digestHolder ! GetDataFromCurrentView[ErgoHistory, DigestState, ErgoWallet, ErgoMemPool, Option[Header]] { v =>
      v.history.bestHeaderOpt
    }
    expectMsg(Some(block.header))

    digestHolder ! GetDataFromCurrentView[ErgoHistory, DigestState, ErgoWallet, ErgoMemPool, Int] { v =>
      v.history.height
    }
    expectMsg(1)

    digestHolder ! GetDataFromCurrentView[ErgoHistory, DigestState, ErgoWallet, ErgoMemPool, Int] { v =>
      v.history.lastHeaders(10).size
    }
    expectMsg(2)
  }

  ignore("apply invalid full block after a valid one"){ fixture =>
    import fixture._
    val dir = createTempDir
    val (us, bh) = ErgoState.generateGenesisUtxoState(dir)
    val genesis = validFullBlock(parentOpt = None, us, bh)
    val wusAfterGenesis = WrappedUtxoState(us, bh).applyModifier(genesis).get

    //digestHolder ! NodeViewHolder.Subscribe(Seq(SuccessfulPersistentModifier, FailedPersistentModifier))

    digestHolder ! LocallyGeneratedModifier(genesis.header)
    digestHolder ! LocallyGeneratedModifier(genesis.blockTransactions)
    digestHolder ! LocallyGeneratedModifier(genesis.aDProofs.get)

    val block = validFullBlock(Some(genesis.header), wusAfterGenesis)

    digestHolder ! LocallyGeneratedModifier(block.header)
    digestHolder ! LocallyGeneratedModifier(block.blockTransactions)
    digestHolder ! LocallyGeneratedModifier(block.aDProofs.get)

    val wusAfterBlock = wusAfterGenesis.applyModifier(block).get

    digestHolder ! GetDataFromCurrentView[ErgoHistory, DigestState, ErgoWallet, ErgoMemPool, ByteArrayWrapper] { v =>
      ByteArrayWrapper(v.state.rootHash)
    }
    expectMsg(ByteArrayWrapper(wusAfterBlock.rootHash))

    val brokenBlock = validFullBlock(Some(block.header), wusAfterBlock)

    digestHolder ! LocallyGeneratedModifier(brokenBlock.header)

    val brokenTransactions = brokenBlock.blockTransactions.copy(txs = brokenBlock.blockTransactions.txs.tail)
    digestHolder ! LocallyGeneratedModifier(brokenTransactions)
    digestHolder ! LocallyGeneratedModifier(brokenBlock.aDProofs.get)

    digestHolder ! GetDataFromCurrentView[ErgoHistory, DigestState, ErgoWallet, ErgoMemPool, Option[ErgoFullBlock]] { v =>
      v.history.bestFullBlockOpt
    }
    expectMsg(Some(block))

    digestHolder ! GetDataFromCurrentView[ErgoHistory, DigestState, ErgoWallet, ErgoMemPool, Option[Header]] { v =>
      v.history.bestHeaderOpt
    }
    expectMsg(Some(block.header))
  }

  property("switching to a better chain") { fixture =>
    import fixture._
    val dir = createTempDir
    val (us, bh) = ErgoState.generateGenesisUtxoState(dir)
    val genesis = validFullBlock(parentOpt = None, us, bh)
    val wusAfterGenesis = WrappedUtxoState(us, bh).applyModifier(genesis).get

    digestHolder ! LocallyGeneratedModifier(genesis.header)
    digestHolder ! LocallyGeneratedModifier(genesis.blockTransactions)
    digestHolder ! LocallyGeneratedModifier(genesis.aDProofs.get)

    val chain1block1 = validFullBlock(Some(genesis.header), wusAfterGenesis)

    digestHolder ! LocallyGeneratedModifier(chain1block1.header)
    digestHolder ! LocallyGeneratedModifier(chain1block1.blockTransactions)
    digestHolder ! LocallyGeneratedModifier(chain1block1.aDProofs.get)

    val chain2block1 = validFullBlock(Some(genesis.header), wusAfterGenesis)

    digestHolder ! LocallyGeneratedModifier(chain2block1.header)
    digestHolder ! LocallyGeneratedModifier(chain2block1.blockTransactions)
    digestHolder ! LocallyGeneratedModifier(chain2block1.aDProofs.get)

    val wusChain2Block1 = wusAfterGenesis.applyModifier(chain2block1).get

    val chain2block2 = validFullBlock(Some(genesis.header), wusChain2Block1)

    digestHolder ! LocallyGeneratedModifier(chain2block2.header)
    digestHolder ! LocallyGeneratedModifier(chain2block2.blockTransactions)
    digestHolder ! LocallyGeneratedModifier(chain2block2.aDProofs.get)


    digestHolder ! GetDataFromCurrentView[ErgoHistory, DigestState, ErgoWallet, ErgoMemPool, Option[ErgoFullBlock]] { v =>
      v.history.bestFullBlockOpt
    }
    expectMsg(Some(chain2block2))

    digestHolder ! GetDataFromCurrentView[ErgoHistory, DigestState, ErgoWallet, ErgoMemPool, Option[Header]] { v =>
      v.history.bestHeaderOpt
    }
    expectMsg(Some(chain2block2.header))
  }

  ignore("switching to a better chain, but a new chain is invalid") {fixture =>

  }
}