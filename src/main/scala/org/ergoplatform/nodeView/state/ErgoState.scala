package org.ergoplatform.nodeView.state

import java.io.File

import org.ergoplatform._
import org.ergoplatform.mining.emission.EmissionRules
import org.ergoplatform.mining.{PublicKeyLength, group}
import org.ergoplatform.modifiers.ErgoPersistentModifier
import org.ergoplatform.modifiers.mempool.ErgoTransaction
import org.ergoplatform.modifiers.state.{Insertion, Removal, StateChanges}
import org.ergoplatform.nodeView.history.ErgoHistory
import org.ergoplatform.settings.ErgoSettings
import scapi.sigma.DLogProtocol.ProveDlog
import scorex.core.transaction.state.MinimalState
import scorex.core.{VersionTag, bytesToVersion}
import scorex.crypto.authds.{ADDigest, ADKey}
import scorex.util.encode.Base16
import scorex.util.{ModifierId, ScorexLogging, bytesToId}
import sigmastate.Values.{IntConstant, LongConstant, Value}
import sigmastate._
import sigmastate.lang.Terms._
import sigmastate.utxo._

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

  def closeStorage(): Unit = {
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
    *         if box was first spend and created after that - it is in both toInsert and toRemove,
    *         and an error will be thrown further during tree modification
    */
  def boxChanges(txs: Seq[ErgoTransaction]): (Seq[ADKey], Seq[ErgoBox]) = {
    val toInsert: mutable.HashMap[ModifierId, ErgoBox] = mutable.HashMap.empty
    val toRemove: mutable.ArrayBuffer[(ModifierId, ADKey)] = mutable.ArrayBuffer()
    txs.foreach { tx =>
      tx.inputs.foreach { i =>
        val wrapped = bytesToId(i.boxId)
        toInsert.remove(wrapped) match {
          case None => toRemove.append((wrapped, i.boxId))
          case _ => // old value removed, do nothing
        }
      }
      tx.outputs.foreach(o => toInsert += bytesToId(o.id) -> o)
    }
    (toRemove.sortBy(_._1).map(_._2), toInsert.toSeq.sortBy(_._1).map(_._2))
  }

  /**
    * @param emission - emission curve
    * @return Genesis box that contains all the coins in the system, protected by the script,
    *         that allows to take part of them every block.
    */
  def genesisEmissionBox(emission: EmissionRules): ErgoBox = {
    val emptyHeight = ErgoHistory.EmptyHistoryHeight
    val s = emission.settings

    val rewardOut = ByIndex(Outputs, IntConstant(0))
    val minerOut = ByIndex(Outputs, IntConstant(1))
    val expectedBytes = rewardOutputScriptStartBytes(s.minerRewardDelay)

    val epoch = Plus(LongConstant(1), Divide(Minus(Height, LongConstant(s.fixedRatePeriod)), LongConstant(s.epochLength)))
    val coinsToIssue = If(LT(Height, LongConstant(s.fixedRatePeriod)),
      s.fixedRate,
      Minus(s.fixedRate, Multiply(s.oneEpochReduction, epoch))
    )
    val sameScriptRule = EQ(ExtractScriptBytes(Self), ExtractScriptBytes(rewardOut))
    val heightCorrect = EQ(SelectField(ExtractCreationInfo(rewardOut), 1).asLongValue, Height)
    val heightIncreased = GT(Height, SelectField(ExtractCreationInfo(Self), 1).asLongValue)
    val correctCoinsConsumed = EQ(coinsToIssue, Minus(ExtractAmount(Self), ExtractAmount(rewardOut)))
    val lastCoins = LE(ExtractAmount(Self), s.oneEpochReduction)
    val outputsNum = EQ(SizeOf(Outputs), 2)
    val correctMinerOutput = AND(
      EQ(ExtractScriptBytes(minerOut), Append(expectedBytes, MinerPubkey)),
      EQ(Height, SelectField(ExtractCreationInfo(minerOut), 1).asLongValue)
    )

    val prop = AND(
      heightIncreased,
      correctMinerOutput,
      OR(AND(outputsNum, sameScriptRule, correctCoinsConsumed, heightCorrect), lastCoins)
    )
    ErgoBox(emission.coinsTotal, prop, emptyHeight, Seq(), Map())
  }

  def generateGenesisUtxoState(stateDir: File,
                               constants: StateConstants): (UtxoState, BoxHolder) = {

    log.info("Generating genesis UTXO state")
    val emissionBox = Some(genesisEmissionBox(constants.emission))
    val bh = BoxHolder(emissionBox.toSeq)

    UtxoState.fromBoxHolder(bh, emissionBox, stateDir, constants).ensuring(us => {
      log.info(s"Genesis UTXO state generated with hex digest ${Base16.encode(us.rootHash)}")
      java.util.Arrays.equals(us.rootHash, constants.emission.settings.afterGenesisStateDigest) && us.version == genesisStateVersion
    }) -> bh
  }

  def generateGenesisDigestState(stateDir: File, settings: ErgoSettings): DigestState = {
    DigestState.create(Some(genesisStateVersion), Some(settings.chainSettings.monetary.afterGenesisStateDigest),
      stateDir, settings)
  }

  val preGenesisStateDigest: ADDigest = ADDigest @@ Array.fill(32)(0: Byte)

  lazy val genesisStateVersion: VersionTag = bytesToVersion(Array.fill(32)(1: Byte))

  def readOrGenerate(settings: ErgoSettings,
                     constants: StateConstants): ErgoState[_] = {
    val dir = stateDir(settings)
    dir.mkdirs()

    settings.nodeSettings.stateType match {
      case StateType.Digest => DigestState.create(None, None, dir, settings)
      case StateType.Utxo if dir.listFiles().nonEmpty => UtxoState.create(dir, constants)
      case _ => ErgoState.generateGenesisUtxoState(dir, constants)._1
    }
  }

  /**
    * Required script of the box, that collects mining rewards
    */
  def rewardOutputScript(delta: Int, minerPk: ProveDlog): Value[SBoolean.type] = {
    /*
    val compiler = new SigmaCompiler

    val compiled = compiler.compile(Map("delta" -> delta, "minerPk" -> minerPk),
      """{
        |    val minimalHeight = SELF.R4[Long].get + delta
        |    val correctHeight = HEIGHT >= minimalHeight
        |    correctHeight && minerPk
        |}""".stripMargin).asBoolValue
        */

    AND(
      GE(Height, Plus(SelectField(ExtractCreationInfo(Self), 1).asLongValue, LongConstant(delta))),
      minerPk
    )
  }

  /**
    * Starting bytes for rewardOutputScript
    */
  def rewardOutputScriptStartBytes(delta: Int): Array[Byte] = delta match {
    //    case -1000 => Algos.decode("9683020192a39ae4c6a7040505cf0fcd07").get
    //    case 720 => Algos.decode("9683020192a39ae4c6a7040505a00bcd07").get
    case _ => rewardOutputScript(delta, ProveDlog(group.generator)).bytes.dropRight(PublicKeyLength)
  }

  /**
    * Proposition, that allows to send coins to a box, that is protected by the following proposition:
    * prove dlog of miners public key and height is at least `delta` blocks bigger then the current one
    *
    * TODO it is possible to use creation height instead of R4, but there is no easy access to in in a script
    */
  def feeProposition(delta: Int = 720): Value[SBoolean.type] = {
    val expectedBytes = rewardOutputScriptStartBytes(delta)

    val out = ByIndex(Outputs, IntConstant(0))

    AND(
      EQ(Height, SelectField(ExtractCreationInfo(out), 1).asLongValue),
      EQ(ExtractScriptBytes(out), Append(expectedBytes, MinerPubkey)),
      EQ(SizeOf(Outputs), 1)
    )
  }

}
