package org.ergoplatform.nodeView.state

import java.io.File

import akka.actor.ActorRef
import io.iohk.iodb.Store
import org.ergoplatform.{ErgoBox, Height, Outputs, Self}
import org.ergoplatform.ErgoBox.{R3, R4}
import org.ergoplatform.modifiers.ErgoPersistentModifier
import org.ergoplatform.settings.{Algos, ChainSettings, ErgoSettings, NodeConfigurationSettings}
import scorex.core.VersionTag
import scorex.core.transaction.state.MinimalState
import scorex.core.utils.ScorexLogging
import scorex.crypto.authds.ADDigest
import scorex.crypto.encode.Base16
import sigmastate.{SLong, _}
import sigmastate.Values.{ByteConstant, IntConstant, LongConstant, TrueLeaf, Value}
import sigmastate.utxo.{ByIndex, ExtractAmount, ExtractRegisterAs, ExtractScriptBytes}

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

  lazy val genesisEmissionBox: ErgoBox = {
    // TODO check that this corresponds to ChainSettings.blockInterval
    val fixedRate = LongConstant(7500000000L)
    val fixedRatePeriod = LongConstant(460800)
    val rewardReductionPeriod = LongConstant(64800)
    val decreasingEpochs = LongConstant(25)
    val blocksTotal = LongConstant(2102400)

    val register = R3
    val red = Modulo(Multiply(fixedRate, Modulo(Minus(Height, fixedRatePeriod), rewardReductionPeriod)), decreasingEpochs)
    val coinsToIssue = If(LE(Height, fixedRatePeriod), fixedRate, Minus(fixedRate, red))
    val out = ByIndex(Outputs, 0)
    val sameScriptRule = EQ(ExtractScriptBytes(Self), ExtractScriptBytes(out))
    val heightCorrect = EQ(ExtractRegisterAs[SLong.type](out, register), Height)
    val heightIncreased = GT(ExtractRegisterAs[SLong.type](out, register), ExtractRegisterAs[SLong.type](Self, register))
    val correctCoinsConsumed = EQ(coinsToIssue, Minus(ExtractAmount(Self), ExtractAmount(out)))
    val prop = OR(AND(sameScriptRule, correctCoinsConsumed, heightIncreased, heightCorrect), EQ(Height, blocksTotal))
    ErgoBox(9773992500000000L, prop, Map(R4 -> IntConstant(-1)))
  }

  def generateGenesisUtxoState(stateDir: File, nodeViewHolderRef: Option[ActorRef]): (UtxoState, BoxHolder) = {
    log.info("Generating genesis UTXO state")
    val bh = BoxHolder(Seq(genesisEmissionBox))

    UtxoState.fromBoxHolder(bh, stateDir, nodeViewHolderRef).ensuring(us => {
      log.info(s"Genesis UTXO state generated with hex digest ${Base16.encode(us.rootHash)}")
      us.rootHash.sameElements(afterGenesisStateDigest) && us.version.sameElements(genesisStateVersion)
    }) -> bh
  }

  def generateGenesisDigestState(stateDir: File, settings: NodeConfigurationSettings): DigestState = {
    DigestState.create(Some(genesisStateVersion), Some(afterGenesisStateDigest), stateDir, settings)
  }

  val preGenesisStateDigest: ADDigest = ADDigest @@ Array.fill(32)(0: Byte)
  //33 bytes in Base16 encoding
  val afterGenesisStateDigestHex: String = "78b130095239561ecf5449a7794c0615326d1fd007cc79dcc286e46e4beb1d3f01"
  //TODO rework try.get
  val afterGenesisStateDigest: ADDigest = ADDigest @@ Base16.decode(afterGenesisStateDigestHex).get

  lazy val genesisStateVersion: VersionTag = VersionTag @@ Algos.hash(afterGenesisStateDigest.tail)

  def readOrGenerate(settings: ErgoSettings, nodeViewHolderRef: Option[ActorRef]): ErgoState[_] = {
    val dir = stateDir(settings)
    dir.mkdirs()

    settings.nodeSettings.stateType match {
      case StateType.Digest => DigestState.create(None, None, dir, settings.nodeSettings)
      case StateType.Utxo if dir.listFiles().nonEmpty => UtxoState.create(dir, nodeViewHolderRef)
      case _ => ErgoState.generateGenesisUtxoState(dir, nodeViewHolderRef)._1
    }
  }
}
