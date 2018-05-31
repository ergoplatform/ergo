package org.ergoplatform.nodeView.state

import java.io.File

import akka.actor.ActorRef
import io.iohk.iodb.{ByteArrayWrapper, Store}
import org.ergoplatform.ErgoBox.R3
import org.ergoplatform.mining.emission.CoinsEmission
import org.ergoplatform.modifiers.ErgoPersistentModifier
import org.ergoplatform.modifiers.mempool.ErgoTransaction
import org.ergoplatform.modifiers.state.{Insertion, Removal, StateChanges}
import org.ergoplatform.settings.{Algos, ErgoSettings, MonetarySettings, NodeConfigurationSettings}
import org.ergoplatform.{ErgoBox, Height, Outputs, Self}
import scorex.core.VersionTag
import scorex.core.transaction.state.MinimalState
import scorex.core.utils.ScorexLogging
import scorex.crypto.authds.{ADDigest, ADKey}
import scorex.crypto.encode.Base16
import sigmastate.Values.{IntConstant, LongConstant}
import sigmastate.utxo.{ByIndex, ExtractAmount, ExtractRegisterAs, ExtractScriptBytes}
import sigmastate.{SLong, _}

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

  val store: Store

  def closeStorage: Unit = {
    log.warn("Closing state's store.")
    store.close()
  }

  override def applyModifier(mod: ErgoPersistentModifier): Try[IState]

  override def rollbackTo(version: VersionTag): Try[IState]

  def rollbackVersions: Iterable[VersionTag]

  override type NVCT = this.type
}

object ErgoState extends ScorexLogging {

  //TODO move to settings?
  val KeepVersions = 200

  def stateDir(settings: ErgoSettings): File = new File(s"${settings.directory}/state")

  /**
    * Extract ordered sequence of operations on UTXO set from set of transactions
    */
  def boxChanges(txs: Seq[ErgoTransaction]): StateChanges = {
    val opened: Seq[ADKey] = txs.flatMap(t => t.inputs.map(_.boxId))
    val openedSet: Set[ByteArrayWrapper] = opened.map(o => ByteArrayWrapper(o)).toSet
    val inserted: Seq[ErgoBox] = txs.flatMap(t => t.outputs)
    val insertedSet: Set[ByteArrayWrapper] = inserted.map(b => ByteArrayWrapper(b.id)).toSet
    val both: Set[ByteArrayWrapper] = insertedSet.intersect(openedSet)
    val toRemove = opened.filterNot(s => both.contains(ByteArrayWrapper(s)))
      .map(id => Removal(id))
    val toInsert = inserted.filterNot(s => both.contains(ByteArrayWrapper(s.id)))
      .map(b => Insertion(b))
    StateChanges(toRemove ++ toInsert)
  }

  /**
    * @param emission - emission curve
    * @return Genesis box that contains all the coins in the system, protected by the script,
    *         that allows to take part of them every block.
    */
  def genesisEmissionBox(emission: CoinsEmission): ErgoBox = {
    val s = emission.settings

    val register = R3
    val out = ByIndex(Outputs, LongConstant(0))
    val epoch = Plus(LongConstant(1), Divide(Minus(Height, LongConstant(s.fixedRatePeriod)), LongConstant(s.epochLength)))
    val coinsToIssue = If(LT(Height, LongConstant(s.fixedRatePeriod)),
      s.fixedRate,
      Minus(s.fixedRate, Multiply(s.oneEpochReduction, epoch))
    )
    val sameScriptRule = EQ(ExtractScriptBytes(Self), ExtractScriptBytes(out))
    val heightCorrect = EQ(ExtractRegisterAs[SLong.type](out, register), Height)
    val heightIncreased = GT(Height, ExtractRegisterAs[SLong.type](Self, register))
    val correctCoinsConsumed = EQ(coinsToIssue, Minus(ExtractAmount(Self), ExtractAmount(out)))
    val lastCoins = LE(ExtractAmount(Self), s.oneEpochReduction)

    val prop = AND(heightIncreased, OR(AND(sameScriptRule, correctCoinsConsumed, heightCorrect), lastCoins))
    ErgoBox(emission.coinsTotal, prop, Map(register -> LongConstant(-1)))
  }

  def generateGenesisUtxoState(stateDir: File,
                               monetary: MonetarySettings,
                               nodeViewHolderRef: Option[ActorRef]): (UtxoState, BoxHolder) = {
    val emission = new CoinsEmission(monetary)

    log.info("Generating genesis UTXO state")
    val bh = BoxHolder(Seq(genesisEmissionBox(emission)))

    UtxoState.fromBoxHolder(bh, stateDir, emission, nodeViewHolderRef).ensuring(us => {
      log.info(s"Genesis UTXO state generated with hex digest ${Base16.encode(us.rootHash)}")
      us.rootHash.sameElements(monetary.afterGenesisStateDigest) && us.version.sameElements(genesisStateVersion)
    }) -> bh
  }

  def generateGenesisDigestState(stateDir: File, settings: ErgoSettings): DigestState = {
    DigestState.create(Some(genesisStateVersion), Some(settings.chainSettings.monetary.afterGenesisStateDigest),
      stateDir, settings)
  }

  val preGenesisStateDigest: ADDigest = ADDigest @@ Array.fill(32)(0: Byte)

  lazy val genesisStateVersion: VersionTag = VersionTag @@ Array.fill(32)(1: Byte)

  def readOrGenerate(settings: ErgoSettings, nodeViewHolderRef: Option[ActorRef]): ErgoState[_] = {
    val dir = stateDir(settings)
    dir.mkdirs()

    settings.nodeSettings.stateType match {
      case StateType.Digest => DigestState.create(None, None, dir, settings)
      case StateType.Utxo if dir.listFiles().nonEmpty => UtxoState.create(dir, nodeViewHolderRef)
      case _ => ErgoState.generateGenesisUtxoState(dir, settings.chainSettings.monetary, nodeViewHolderRef)._1
    }
  }
}
