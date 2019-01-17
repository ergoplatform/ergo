package org.ergoplatform.nodeView.state

import java.io.File

import org.ergoplatform.ErgoBox.R4
import org.ergoplatform._
import org.ergoplatform.mining.emission.EmissionRules
import org.ergoplatform.mining.group
import org.ergoplatform.modifiers.ErgoPersistentModifier
import org.ergoplatform.modifiers.mempool.ErgoTransaction
import org.ergoplatform.modifiers.state.{Insertion, Removal, StateChanges}
import org.ergoplatform.nodeView.history.ErgoHistory
import org.ergoplatform.settings.ErgoSettings
import scorex.core.transaction.state.MinimalState
import scorex.core.{VersionTag, bytesToVersion}
import scorex.crypto.authds.{ADDigest, ADKey}
import scorex.util.encode.Base16
import scorex.util.{ModifierId, ScorexLogging, bytesToId}
import sigmastate.SCollection.SByteArray
import sigmastate.Values.{ByteArrayConstant, IntArrayConstant, IntConstant, SigmaPropValue, Value}
import sigmastate.basics.DLogProtocol.{DLogProverInput, ProveDlog}
import sigmastate.eval.RuntimeIRContext
import sigmastate.lang.Terms._
import sigmastate.serialization.{ErgoTreeSerializer, ValueSerializer}
import sigmastate.utxo._
import sigmastate.{Values, _}

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

  private def boxCreationHeight(box: Value[SBox.type]): Value[SInt.type] =
    SelectField(ExtractCreationInfo(box), 1).asIntValue

  /**
    * Genesis box that contains coins in the system to be collected by miners.
    * Box is protected by the script that allows to take part of them every block.
    */
  private def genesisFoundersBox(emission: EmissionRules): ErgoBox = {
    // todo move to config
    val secret = new DLogProverInput(BigInt(1).bigInteger)
    val protection = ByteArrayConstant(ValueSerializer.serialize(secret.publicImage))
    val value = emission.foundersCoinsTotal
    val prop = ErgoScriptPredef.foundationScript(emission.settings)
    ErgoBox(value, prop, ErgoHistory.EmptyHistoryHeight, Seq(), Map(R4 -> protection))
  }

  /**
    * Genesis box that contains coins in the system to be collected by miners.
    * Box is protected by the script that allows to take part of them every block.
    */
  private def genesisEmissionBox(emission: EmissionRules): ErgoBox = {
    val value = emission.minersCoinsTotal
    val prop = ErgoScriptPredef.emissionBoxProp(emission.settings)
    ErgoBox(value, prop, ErgoHistory.EmptyHistoryHeight, Seq(), Map())
  }

  /**
    * All boxes of genesis state.
    * Emission box is always the first.
    */
  def genesisBoxes(emission: EmissionRules): Seq[ErgoBox] = {
    Seq(genesisEmissionBox(emission), genesisFoundersBox(emission))
  }

  def generateGenesisUtxoState(stateDir: File,
                               constants: StateConstants): (UtxoState, BoxHolder) = {

    log.info("Generating genesis UTXO state")
    val boxes = genesisBoxes(constants.emission)
    val bh = BoxHolder(boxes)

    UtxoState.fromBoxHolder(bh, boxes.headOption, stateDir, constants).ensuring(us => {
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

}
