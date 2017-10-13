package org.ergoplatform.nodeView

import java.io.File

import akka.actor.{ActorRef, Props}
import io.iohk.iodb.ByteArrayWrapper
import org.ergoplatform.modifiers.ErgoFullBlock
import org.ergoplatform.modifiers.history.{DefaultFakePowScheme, Header}
import org.ergoplatform.modifiers.mempool.AnyoneCanSpendTransaction
import org.ergoplatform.modifiers.mempool.proposition.AnyoneCanSpendProposition
import org.ergoplatform.nodeView.history.ErgoHistory
import org.ergoplatform.nodeView.mempool.ErgoMemPool
import org.ergoplatform.nodeView.state.{DigestState, ErgoState, UtxoState}
import org.ergoplatform.nodeView.wallet.ErgoWallet
import org.ergoplatform.settings.ErgoSettings
import org.ergoplatform.utils.{ErgoGenerators, SequentialAkkaFixture}
import org.scalatest.Matchers
import scorex.core.LocalInterface.{LocallyGeneratedModifier, LocallyGeneratedTransaction}
import scorex.core.NodeViewHolder
import scorex.core.NodeViewHolder.EventType._
import scorex.core.NodeViewHolder.{GetDataFromCurrentView, SuccessfulModification}
import scorex.testkit.utils.FileUtils

import scala.concurrent.duration._

class ErgoNodeViewHolderSpecification extends SequentialAkkaFixture with ErgoGenerators with Matchers {

  class HolderFixture extends AkkaFixture with FileUtils {
    def getDigestHolder: ActorRef = {
      val dir: File = createTempDir
      val defaultSettings: ErgoSettings = ErgoSettings.read(None).copy(directory = dir.getAbsolutePath)

      val settings: ErgoSettings = defaultSettings.copy(
        nodeSettings = defaultSettings.nodeSettings.copy(ADState = true),
        chainSettings = defaultSettings.chainSettings.copy(poWScheme = DefaultFakePowScheme)
      )
      system.actorOf(Props(classOf[DigestErgoNodeViewHolder], settings))
    }

    def getUtxoHolder: ActorRef = {
      val dir: File = createTempDir
      val defaultSettings: ErgoSettings = ErgoSettings.read(None).copy(directory = dir.getAbsolutePath)
      val settings: ErgoSettings = defaultSettings.copy(
        nodeSettings = defaultSettings.nodeSettings.copy(ADState = false),
        chainSettings = defaultSettings.chainSettings.copy(poWScheme = DefaultFakePowScheme)
      )
      system.actorOf(Props(classOf[UtxoErgoNodeViewHolder], settings))
    }
  }

  override type Fixture = HolderFixture

  override def createAkkaFixture(): Fixture = new HolderFixture

  property("DigestState: genesis - state digest") { fixture => import fixture._
    val digestHolder = getDigestHolder

    digestHolder ! GetDataFromCurrentView[ErgoHistory, DigestState, ErgoWallet, ErgoMemPool, Boolean] { v =>
      v.state.rootHash.sameElements(ErgoState.afterGenesisStateDigest)
    }
    expectMsg(true)
  }

  property("DigestState: genesis - history (no genesis block there yet)") { fixture => import fixture._
    val digestHolder = getDigestHolder
    digestHolder ! GetDataFromCurrentView[ErgoHistory, DigestState, ErgoWallet, ErgoMemPool, Option[Header]] { v =>
      v.history.bestHeaderOpt
    }
    expectMsg(None)
  }

  property("DigestState: genesis - apply valid block header") { fixture => import fixture._
    val digestHolder = getDigestHolder
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


  property("DigestState: apply valid full block as genesis") { fixture => import fixture._
    val digestHolder = getDigestHolder
    val dir = createTempDir
    val (us, bh) = ErgoState.generateGenesisUtxoState(dir)
    val genesis = validFullBlock(parentOpt = None, us, bh)

    digestHolder ! NodeViewHolder.Subscribe(Seq(SuccessfulPersistentModifier, FailedPersistentModifier))

    digestHolder ! LocallyGeneratedModifier(genesis.header)
    expectMsg(SuccessfulModification(genesis.header, None))

    digestHolder ! LocallyGeneratedModifier(genesis.blockTransactions)
    //expectMsg(SuccessfulModification(genesis.blockTransactions, None)) // todo: why no message?

    digestHolder ! LocallyGeneratedModifier(genesis.aDProofs.get)
    expectMsg(SuccessfulModification(genesis.aDProofs.get, None))

    digestHolder ! GetDataFromCurrentView[ErgoHistory, DigestState, ErgoWallet, ErgoMemPool, Option[ErgoFullBlock]] { v =>
      v.history.bestFullBlockOpt
    }
    expectMsg(Some(genesis))
  }

  property("DigestState: apply full blocks after genesis") { fixture => import fixture._
    val digestHolder = getDigestHolder
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

  property("DigestState: add transaction to the memory pool") { fixture => import fixture._
    val digestHolder = getDigestHolder

    val dir = createTempDir
    val (us, bh) = ErgoState.generateGenesisUtxoState(dir)
    val genesis = validFullBlock(parentOpt = None, us, bh)
    val wusAfterGenesis = WrappedUtxoState(us, bh).applyModifier(genesis).get

    val toSpend = wusAfterGenesis.takeBoxes(1).head

    val tx = AnyoneCanSpendTransaction(IndexedSeq(toSpend.nonce), IndexedSeq(toSpend.value))

    digestHolder ! LocallyGeneratedTransaction[AnyoneCanSpendProposition.type, AnyoneCanSpendTransaction](tx)

    digestHolder ! GetDataFromCurrentView[ErgoHistory, DigestState, ErgoWallet, ErgoMemPool, Int] { v =>
      v.pool.size
    }

    expectMsg(1)
  }

  property("Digest State: apply invalid full block") { fixture => import fixture._

    val digestHolder = getDigestHolder
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

    digestHolder ! GetDataFromCurrentView[ErgoHistory, DigestState, ErgoWallet, ErgoMemPool, Option[Header]] { v =>
      v.history.bestHeaderOpt
    }
    expectMsg(Some(block.header))

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
    //TODO Note and verify!
    expectMsg(Some(brokenBlock.header))
  }

  property("UtxoState: genesis - state digest") { fixture => import fixture._
    val utxoHolder = getUtxoHolder

    utxoHolder ! GetDataFromCurrentView[ErgoHistory, UtxoState, ErgoWallet, ErgoMemPool, Boolean] { v =>
      v.state.rootHash.sameElements(ErgoState.afterGenesisStateDigest)
    }
    expectMsg(true)
  }

  property("UtxoState: genesis - history (no genesis block there yet)") { fixture => import fixture._
    val utxoHolder = getUtxoHolder
    utxoHolder ! GetDataFromCurrentView[ErgoHistory, UtxoState, ErgoWallet, ErgoMemPool, Option[Header]] { v =>
      v.history.bestHeaderOpt
    }
    expectMsg(None)
  }

  property("UtxoState: genesis - apply valid block header") { fixture => import fixture._
    val utxoHolder = getUtxoHolder
    val dir = createTempDir
    val (us, bh) = ErgoState.generateGenesisUtxoState(dir)
    val block = validFullBlock(None, us, bh)

    utxoHolder ! GetDataFromCurrentView[ErgoHistory, UtxoState, ErgoWallet, ErgoMemPool, Option[Header]] { v =>
      v.history.bestHeaderOpt
    }
    expectMsg(None)

    utxoHolder ! GetDataFromCurrentView[ErgoHistory, UtxoState, ErgoWallet, ErgoMemPool, Int] { v =>
      v.history.height
    }
    expectMsg(-1)

    utxoHolder ! NodeViewHolder.Subscribe(Seq(SuccessfulPersistentModifier, FailedPersistentModifier))

    //sending header
    utxoHolder ! LocallyGeneratedModifier[Header](block.header)

    expectMsg(SuccessfulModification(block.header, None))

    utxoHolder ! GetDataFromCurrentView[ErgoHistory, UtxoState, ErgoWallet, ErgoMemPool, Int] { v =>
      v.history.height
    }
    expectMsg(0)

    utxoHolder ! GetDataFromCurrentView[ErgoHistory, UtxoState, ErgoWallet, ErgoMemPool, Option[Int]] { v =>
      v.history.heightOf(block.header.id)
    }
    expectMsg(Some(0))


    utxoHolder ! GetDataFromCurrentView[ErgoHistory, UtxoState, ErgoWallet, ErgoMemPool, Int] { v =>
      v.history.lastHeaders(10).size
    }
    expectMsg(1)

    utxoHolder ! GetDataFromCurrentView[ErgoHistory, UtxoState, ErgoWallet, ErgoMemPool, Seq[ByteArrayWrapper]] { v =>
      v.history.openSurfaceIds().map(ByteArrayWrapper.apply)
    }
    expectMsg(Seq(ByteArrayWrapper(block.header.id)))

    utxoHolder ! GetDataFromCurrentView[ErgoHistory, UtxoState, ErgoWallet, ErgoMemPool, Option[Header]] { v =>
      v.history.bestHeaderOpt
    }
    expectMsg(Some(block.header))
  }


  property("UtxoState: apply valid full block as genesis") { fixture => import fixture._
    val utxoHolder = getUtxoHolder
    val dir = createTempDir
    val (us, bh) = ErgoState.generateGenesisUtxoState(dir)
    val genesis = validFullBlock(parentOpt = None, us, bh)

    utxoHolder ! NodeViewHolder.Subscribe(Seq(SuccessfulPersistentModifier, FailedPersistentModifier))

    utxoHolder ! LocallyGeneratedModifier(genesis.header)
    expectMsg(SuccessfulModification(genesis.header, None))

    utxoHolder ! LocallyGeneratedModifier(genesis.blockTransactions)
    expectMsg(SuccessfulModification(genesis.blockTransactions, None))

    utxoHolder ! LocallyGeneratedModifier(genesis.aDProofs.get)
    //expectMsg(SuccessfulModification(genesis.aDProofs.get, None)) // todo: why no message?

    utxoHolder ! GetDataFromCurrentView[ErgoHistory, UtxoState, ErgoWallet, ErgoMemPool, Option[ErgoFullBlock]] { v =>
      v.history.bestFullBlockOpt
    }
    expectMsg(Some(genesis))
  }

  property("UtxoState: apply full blocks after genesis") { fixture => import fixture._
    val utxoHolder = getUtxoHolder
    val dir = createTempDir
    val (us, bh) = ErgoState.generateGenesisUtxoState(dir)
    val genesis = validFullBlock(parentOpt = None, us, bh)
    val wusAfterGenesis = WrappedUtxoState(us, bh).applyModifier(genesis).get

    //digestHolder ! NodeViewHolder.Subscribe(Seq(SuccessfulPersistentModifier, FailedPersistentModifier))

    utxoHolder ! LocallyGeneratedModifier(genesis.header)
    utxoHolder ! LocallyGeneratedModifier(genesis.blockTransactions)
    utxoHolder ! LocallyGeneratedModifier(genesis.aDProofs.get)

    val block = validFullBlock(Some(genesis.header), wusAfterGenesis)

    utxoHolder ! LocallyGeneratedModifier(block.header)
    utxoHolder ! LocallyGeneratedModifier(block.blockTransactions)
    utxoHolder ! LocallyGeneratedModifier(block.aDProofs.get)


    utxoHolder ! GetDataFromCurrentView[ErgoHistory, UtxoState, ErgoWallet, ErgoMemPool, Option[ErgoFullBlock]] { v =>
      v.history.bestFullBlockOpt
    }
    expectMsg(Some(block))

    utxoHolder ! GetDataFromCurrentView[ErgoHistory, UtxoState, ErgoWallet, ErgoMemPool, Option[Header]] { v =>
      v.history.bestHeaderOpt
    }
    expectMsg(Some(block.header))

    utxoHolder ! GetDataFromCurrentView[ErgoHistory, UtxoState, ErgoWallet, ErgoMemPool, Int] { v =>
      v.history.height
    }
    expectMsg(1)

    utxoHolder ! GetDataFromCurrentView[ErgoHistory, UtxoState, ErgoWallet, ErgoMemPool, Int] { v =>
      v.history.lastHeaders(10).size
    }
    expectMsg(2)
  }

  property("UtxoState: add transaction to the memory pool") { fixture => import fixture._
    val utxoHolder = getUtxoHolder

    val dir = createTempDir
    val (us, bh) = ErgoState.generateGenesisUtxoState(dir)
    val genesis = validFullBlock(parentOpt = None, us, bh)
    val wusAfterGenesis = WrappedUtxoState(us, bh).applyModifier(genesis).get

    val toSpend = wusAfterGenesis.takeBoxes(1).head

    val tx = AnyoneCanSpendTransaction(IndexedSeq(toSpend.nonce), IndexedSeq(toSpend.value))

    utxoHolder ! LocallyGeneratedTransaction[AnyoneCanSpendProposition.type, AnyoneCanSpendTransaction](tx)

    utxoHolder ! GetDataFromCurrentView[ErgoHistory, UtxoState, ErgoWallet, ErgoMemPool, Int] { v =>
      v.pool.size
    }

    expectMsg(1)
  }

  property("Utxo State: apply invalid full block") { fixture => import fixture._

    val utxoHolder = getUtxoHolder
    val dir = createTempDir
    val (us, bh) = ErgoState.generateGenesisUtxoState(dir)
    val genesis = validFullBlock(parentOpt = None, us, bh)
    val wusAfterGenesis = WrappedUtxoState(us, bh).applyModifier(genesis).get

    //digestHolder ! NodeViewHolder.Subscribe(Seq(SuccessfulPersistentModifier, FailedPersistentModifier))

    utxoHolder ! LocallyGeneratedModifier(genesis.header)
    utxoHolder ! LocallyGeneratedModifier(genesis.blockTransactions)
    utxoHolder ! LocallyGeneratedModifier(genesis.aDProofs.get)

    val block = validFullBlock(Some(genesis.header), wusAfterGenesis)

    utxoHolder ! LocallyGeneratedModifier(block.header)
    utxoHolder ! LocallyGeneratedModifier(block.blockTransactions)
    utxoHolder ! LocallyGeneratedModifier(block.aDProofs.get)

    val wusAfterBlock = wusAfterGenesis.applyModifier(block).get

    utxoHolder ! GetDataFromCurrentView[ErgoHistory, UtxoState, ErgoWallet, ErgoMemPool, ByteArrayWrapper] { v =>
      ByteArrayWrapper(v.state.rootHash)
    }
    expectMsg(ByteArrayWrapper(wusAfterBlock.rootHash))

    utxoHolder ! GetDataFromCurrentView[ErgoHistory, UtxoState, ErgoWallet, ErgoMemPool, Option[Header]] { v =>
      v.history.bestHeaderOpt
    }
    expectMsg(Some(block.header))

    val brokenBlock = validFullBlock(Some(block.header), wusAfterBlock)

    utxoHolder ! LocallyGeneratedModifier(brokenBlock.header)

    val brokenTransactions = brokenBlock.blockTransactions.copy(txs = brokenBlock.blockTransactions.txs.tail)
    utxoHolder ! LocallyGeneratedModifier(brokenTransactions)
    utxoHolder ! LocallyGeneratedModifier(brokenBlock.aDProofs.get)

    utxoHolder ! GetDataFromCurrentView[ErgoHistory, UtxoState, ErgoWallet, ErgoMemPool, Option[ErgoFullBlock]] { v =>
      v.history.bestFullBlockOpt
    }
    expectMsg(Some(block))

    utxoHolder ! GetDataFromCurrentView[ErgoHistory, UtxoState, ErgoWallet, ErgoMemPool, Option[Header]] { v =>
      v.history.bestHeaderOpt
    }
    //TODO Note and verify!
    expectMsg(Some(brokenBlock.header))
  }



  property("UTXOState: switching to a better chain") { fixture => import fixture._

    val utxoHolder = getUtxoHolder
    val dir = createTempDir
    val (us, bh) = ErgoState.generateGenesisUtxoState(dir)
    val genesis = validFullBlock(parentOpt = None, us, bh)
    val wusAfterGenesis = WrappedUtxoState(us, bh).applyModifier(genesis).get

    utxoHolder ! LocallyGeneratedModifier(genesis.header)
    utxoHolder ! LocallyGeneratedModifier(genesis.blockTransactions)
    utxoHolder ! LocallyGeneratedModifier(genesis.aDProofs.get)

    val chain1block1 = validFullBlock(Some(genesis.header), wusAfterGenesis)

    utxoHolder ! LocallyGeneratedModifier(chain1block1.header)
    utxoHolder ! LocallyGeneratedModifier(chain1block1.blockTransactions)
    utxoHolder ! LocallyGeneratedModifier(chain1block1.aDProofs.get)

    val chain2block1 = validFullBlock(Some(genesis.header), wusAfterGenesis)

    utxoHolder ! LocallyGeneratedModifier(chain2block1.header)
    utxoHolder ! LocallyGeneratedModifier(chain2block1.blockTransactions)
    utxoHolder ! LocallyGeneratedModifier(chain2block1.aDProofs.get)

    val wusChain2Block1 = wusAfterGenesis.applyModifier(chain2block1).get
    val chain2block2 = validFullBlock(Some(chain2block1.header), wusChain2Block1)

    utxoHolder ! LocallyGeneratedModifier(chain2block2.header)
    utxoHolder ! LocallyGeneratedModifier(chain2block2.blockTransactions)
    utxoHolder ! LocallyGeneratedModifier(chain2block2.aDProofs.get)

    utxoHolder ! GetDataFromCurrentView[ErgoHistory, DigestState, ErgoWallet, ErgoMemPool, Option[String]] { v =>
      v.history.bestFullBlockOpt.map(_.header.encodedId)
    }
    expectMsg(10 seconds, Some(chain2block2.header.encodedId))

    utxoHolder ! GetDataFromCurrentView[ErgoHistory, DigestState, ErgoWallet, ErgoMemPool, Option[Header]] { v =>
      v.history.bestHeaderOpt
    }
    expectMsg(Some(chain2block2.header))
  }

  property("DigestState: switching to a better chain") { fixture => import fixture._

    val digestHolder = getDigestHolder
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
    val chain2block2 = validFullBlock(Some(chain2block1.header), wusChain2Block1)

    chain2block1.header.stateRoot shouldEqual wusChain2Block1.rootHash

    digestHolder ! LocallyGeneratedModifier(chain2block2.header)
    digestHolder ! LocallyGeneratedModifier(chain2block2.blockTransactions)
    digestHolder ! LocallyGeneratedModifier(chain2block2.aDProofs.get)

    digestHolder ! GetDataFromCurrentView[ErgoHistory, DigestState, ErgoWallet, ErgoMemPool, Option[String]] { v =>
      v.history.bestFullBlockOpt.map(_.header.encodedId)
    }
    expectMsg(10 seconds, Some(chain2block2.header.encodedId))

    digestHolder ! GetDataFromCurrentView[ErgoHistory, DigestState, ErgoWallet, ErgoMemPool, Option[Header]] { v =>
      v.history.bestHeaderOpt
    }
    expectMsg(Some(chain2block2.header))
  }

  ignore("switching to a better chain, but a new chain is invalid") { _ => }
}