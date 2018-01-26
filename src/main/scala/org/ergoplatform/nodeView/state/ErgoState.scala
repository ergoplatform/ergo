package org.ergoplatform.nodeView.state

import java.io.File

import akka.actor.ActorRef
import org.ergoplatform.modifiers.ErgoPersistentModifier
import org.ergoplatform.modifiers.mempool.AnyoneCanSpendTransaction
import org.ergoplatform.modifiers.mempool.proposition.{AnyoneCanSpendNoncedBox, AnyoneCanSpendNoncedBoxSerializer, AnyoneCanSpendProposition}
import org.ergoplatform.settings.{Algos, ErgoSettings, NodeConfigurationSettings}
import scorex.core.VersionTag
import scorex.core.transaction.state.{BoxStateChanges, Insertion, MinimalState, Removal}
import scorex.core.utils.ScorexLogging
import scorex.crypto.authds.ADDigest

import scala.util.Try


/**
  * Implementation of minimal state concept in Scorex. Minimal state (or just state from now) is some data structure
  * enough to validate a new blockchain element(e.g. block).
  * State in Ergo could be UTXO, like in Bitcoin or just a single digest. If the state is about UTXO, transaction set
  * of a block could be verified with no help of additional data. If the state is about just a digest, then proofs for
  * transformations of UTXO set presented in form of authenticated dynamic dictionary are needed to check validity of
  * a transaction set (see https://eprint.iacr.org/2016/994 for details).
  */
trait ErgoState[IState <: MinimalState[ErgoPersistentModifier, IState]]
  extends MinimalState[ErgoPersistentModifier, IState] with ScorexLogging with ErgoStateReader {

  self: IState =>

  //TODO implement correctly
  def stateHeight: Int = 0

  override def applyModifier(mod: ErgoPersistentModifier): Try[IState]

  override def rollbackTo(version: VersionTag): Try[IState]

  def rollbackVersions: Iterable[VersionTag]

  override type NVCT = this.type
}

object ErgoState extends ScorexLogging {

  val BoxSize = AnyoneCanSpendNoncedBoxSerializer.Length

  //TODO move to settings?
  val KeepVersions = 200

  def stateDir(settings: ErgoSettings) = new File(s"${settings.directory}/state")

  def generateGenesisUtxoState(stateDir: File, nodeViewHolderRef: Option[ActorRef]): (UtxoState, BoxHolder) = {
    log.info("Generating genesis UTXO state")
    lazy val genesisSeed = Long.MaxValue
    lazy val rndGen = new scala.util.Random(genesisSeed)
    lazy val initialBoxesNumber = 10000

    lazy val initialBoxes: Seq[AnyoneCanSpendNoncedBox] =
      (1 to initialBoxesNumber).map(_ => AnyoneCanSpendNoncedBox(nonce = rndGen.nextLong(), value = 10000))

    val bh = BoxHolder(initialBoxes)

    UtxoState.fromBoxHolder(bh, stateDir, nodeViewHolderRef).ensuring(us => {
      log.info("Genesis UTXO state generated")
      us.rootHash.sameElements(afterGenesisStateDigest) && us.version.sameElements(genesisStateVersion)
    }) -> bh
  }

  def generateGenesisDigestState(stateDir: File, settings: NodeConfigurationSettings): DigestState = {
    DigestState.create(Some(genesisStateVersion), Some(afterGenesisStateDigest), stateDir, settings)
  }

  val preGenesisStateDigest: ADDigest = ADDigest @@ Array.fill(32)(0: Byte)
  //33 bytes in Base58 encoding
  val afterGenesisStateDigestHex: String = "2Ex5aoUXVCg47AYAsGwRBKarv5PEdig5ZuJwdzkvoxqu6o"
  val afterGenesisStateDigest: ADDigest = ADDigest @@ Algos.decode(afterGenesisStateDigestHex).get

  lazy val genesisStateVersion: VersionTag = VersionTag @@ Algos.hash(afterGenesisStateDigest.tail)

  def readOrGenerate(settings: ErgoSettings, nodeViewHolderRef: Option[ActorRef]): ErgoState[_] = {
    val dir = stateDir(settings)
    dir.mkdirs()

    if (settings.nodeSettings.ADState) DigestState.create(None, None, dir, settings.nodeSettings)
    else if(dir.listFiles().isEmpty) ErgoState.generateGenesisUtxoState(dir, nodeViewHolderRef)._1
    else UtxoState.create(dir, nodeViewHolderRef)
  }
}
