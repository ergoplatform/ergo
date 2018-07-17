package org.ergoplatform.nodeView.state

import java.io.File
import io.iohk.iodb.ByteArrayWrapper
import org.ergoplatform.ErgoBox.R4
import org.ergoplatform.mining.emission.CoinsEmission
import org.ergoplatform.modifiers.ErgoPersistentModifier
import org.ergoplatform.modifiers.mempool.ErgoTransaction
import org.ergoplatform.modifiers.state.{Insertion, Removal, StateChanges}
import org.ergoplatform.settings.ErgoSettings
import org.ergoplatform.{ErgoBox, Height, Outputs, Self}
import scorex.core.VersionTag
import scorex.core.transaction.state.MinimalState
import scorex.core.utils.ScorexLogging
import scorex.crypto.authds.{ADDigest, ADKey}
import scorex.crypto.encode.Base16
import sigmastate.Values.{IntConstant, LongConstant}
import sigmastate.utxo.{ByIndex, ExtractAmount, ExtractRegisterAs, ExtractScriptBytes}
import sigmastate.{SLong, _}

import scala.collection.mutable
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
  extends MinimalState[ErgoPersistentModifier, IState] with ErgoStateReader {

  self: IState =>

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

  def stateDir(settings: ErgoSettings): File = new File(s"${settings.directory}/state")

  /**
    * @param txs - sequence of transactions
    * @return ordered sequence of operations on UTXO set from this sequence of transactions
    *         if some box was created and later spend in this sequence - it is not included in the result at all
    *         if box was first spend and created after that - it is in both toInsert and toRemove
    */
  def stateChanges(txs: Seq[ErgoTransaction]): StateChanges = {
    val (toRemove, toInsert) = boxChanges(txs)
    val toRemoveChanges = toRemove.map(id => Removal(id))
    val toInsertChanges = toInsert.map(b => Insertion(b))
    StateChanges(toRemoveChanges, toInsertChanges)
  }

  /**
    * @param txs - sequence of transactions
    * @return modifications from `txs` - sequence of ids to remove, and sequence of ErgoBoxes to create.
    *         if some box was created and later spend in this sequence - it is not included in the result at all
    *         if box was first spend and created after that - it is in both toInsert and toRemove
    */
  def boxChanges(txs: Seq[ErgoTransaction]): (Seq[ADKey], Seq[ErgoBox]) = {
    val toInsert: mutable.HashMap[ByteArrayWrapper, ErgoBox] = mutable.HashMap.empty
    val toRemove: mutable.HashMap[ByteArrayWrapper, ADKey] = mutable.HashMap.empty
    txs.foreach { tx =>
      tx.inputs.foreach { i =>
        toInsert.remove(ByteArrayWrapper(i.boxId)) match {
          case None => toRemove += ByteArrayWrapper(i.boxId) -> i.boxId
          case _ => // old value removed, do nothing
        }
      }
      tx.outputs.foreach(o => toInsert += ByteArrayWrapper(o.id) -> o)
    }
    (toRemove.toSeq.sortBy(_._1).map(_._2), toInsert.toSeq.sortBy(_._1).map(_._2))
  }

  /**
    * @param emission - emission curve
    * @return Genesis box that contains all the coins in the system, protected by the script,
    *         that allows to take part of them every block.
    */
  def genesisEmissionBox(emission: CoinsEmission): ErgoBox = {
    val s = emission.settings

    val register = R4
    val out = ByIndex(Outputs, IntConstant(0))
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
    ErgoBox(emission.coinsTotal, prop, Seq(), Map(register -> LongConstant(-1)))
  }

  def generateGenesisUtxoState(stateDir: File,
                               constants: StateConstants): (UtxoState, BoxHolder) = {

    log.info("Generating genesis UTXO state")
    val emissionBox = Some(genesisEmissionBox(constants.emission))
    val bh = BoxHolder(emissionBox.toSeq)

    UtxoState.fromBoxHolder(bh, emissionBox, stateDir, constants).ensuring(us => {
      log.info(s"Genesis UTXO state generated with hex digest ${Base16.encode(us.rootHash)}")
      us.rootHash.sameElements(constants.emission.settings.afterGenesisStateDigest) && us.version.sameElements(genesisStateVersion)
    }) -> bh
  }

  def generateGenesisDigestState(stateDir: File, settings: ErgoSettings): DigestState = {
    DigestState.create(Some(genesisStateVersion), Some(settings.chainSettings.monetary.afterGenesisStateDigest),
      stateDir, settings)
  }

  val preGenesisStateDigest: ADDigest = ADDigest @@ Array.fill(32)(0: Byte)

  lazy val genesisStateVersion: VersionTag = VersionTag @@ Array.fill(32)(1: Byte)

  def readOrGenerate(settings: ErgoSettings,
                     constants: StateConstants): ErgoState[_] = {
    val dir = stateDir(settings)
    dir.mkdirs()

    settings.nodeSettings.stateType match {
      case StateType.Digest => DigestState.create(None, None, dir, settings)
      case StateType.Utxo if dir.listFiles().nonEmpty =>
        UtxoState.create(dir, constants, settings.chainSettings.monetary.afterGenesisStateDigest)
      case _ => ErgoState.generateGenesisUtxoState(dir, constants)._1
    }
  }
}
