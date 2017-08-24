package org.ergoplatform.nodeView.state

import java.io.File
import org.ergoplatform.modifiers.ErgoPersistentModifier
import org.ergoplatform.modifiers.mempool.AnyoneCanSpendTransaction
import org.ergoplatform.modifiers.mempool.proposition.{AnyoneCanSpendNoncedBox, AnyoneCanSpendNoncedBoxSerializer, AnyoneCanSpendProposition}
import org.ergoplatform.nodeView.state.ErgoState.Digest
import org.ergoplatform.settings.ErgoSettings
import scorex.core.transaction.state.MinimalState.VersionTag
import scorex.core.transaction.state.{BoxStateChanges, Insertion, MinimalState, Removal}
import scorex.core.utils.ScorexLogging
import scorex.crypto.encode.Base16

import scala.util.Try


/**
  * Implementation of minimal state concept in Scorex. Minimal state (or just state from now) is some data structure
  * enough to validate a new blockchain element(e.g. block).
  * State in Ergo could be UTXO, like in Bitcoin or just a single digest. If the state is about UTXO, transaction set
  * of a block could be verified with no help of additional data. If the state is about just a digest, then proofs for
  * transformations of UTXO set presented in form of authenticated dynamic dictionary are needed to check validity of
  * a transaction set (see https://eprint.iacr.org/2016/994 for details).
  */
trait ErgoState[IState <: MinimalState[AnyoneCanSpendProposition.type,
  AnyoneCanSpendNoncedBox,
  AnyoneCanSpendTransaction,
  ErgoPersistentModifier,
  IState]] extends MinimalState[AnyoneCanSpendProposition.type,
  AnyoneCanSpendNoncedBox,
  AnyoneCanSpendTransaction,
  ErgoPersistentModifier,
  IState] with ScorexLogging {

  self: IState =>

  //TODO: kushti: AVL+ tree root, not Merkle
  def rootHash(): Digest

  //TODO implement correctly
  def stateHeight: Int = 0

  /**
    * Extract ordered sequence of operations on UTXO set from set of transactions
    */
  def boxChanges(txs: Seq[AnyoneCanSpendTransaction]): BoxStateChanges[AnyoneCanSpendProposition.type, AnyoneCanSpendNoncedBox] =
    BoxStateChanges[AnyoneCanSpendProposition.type, AnyoneCanSpendNoncedBox](txs.flatMap { tx =>
      tx.boxIdsToOpen.map(id => Removal[AnyoneCanSpendProposition.type, AnyoneCanSpendNoncedBox](id)) ++
        tx.newBoxes.map(b => Insertion[AnyoneCanSpendProposition.type, AnyoneCanSpendNoncedBox](b))
    })

  override def version: VersionTag

  override def validate(mod: ErgoPersistentModifier): Try[Unit]

  override def applyModifier(mod: ErgoPersistentModifier): Try[IState]

  override def rollbackTo(version: VersionTag): Try[IState]

  def rollbackVersions: Iterable[Digest]

  override type NVCT = this.type
}

object ErgoState extends ScorexLogging {

  type Digest = Array[Byte]

  val BoxSize = AnyoneCanSpendNoncedBoxSerializer.Length

  def generateGenesisUtxoState(stateDir: File): (UtxoState, BoxHolder) = {
    log.info("Generating genesis UTXO state")
    lazy val genesisSeed = Long.MaxValue
    lazy val rndGen = new scala.util.Random(genesisSeed)
    lazy val initialBoxesNumber = 10000

    lazy val initialBoxes: Seq[AnyoneCanSpendNoncedBox] =
      (1 to initialBoxesNumber).map(_ => AnyoneCanSpendNoncedBox(nonce = rndGen.nextLong(), value = 10000))

    val bh = BoxHolder(initialBoxes)

    UtxoState.fromBoxHolder(bh, stateDir).ensuring(us => {
      log.info("Genesis UTXO state generated")
      us.rootHash.sameElements(afterGenesisStateDigest)
    }) -> bh
  }

  def generateGenesisDigestState(stateDir: File): DigestState = {
    DigestState.create(afterGenesisStateDigest, stateDir).get //todo: .get
  }

  val preGenesisStateDigest: Digest = Array.fill(32)(0: Byte)
  val afterGenesisStateDigestHex: String = "f2343e160d4e42a83a87ea1a2f56b6fa2046ab8146c5e61727c297be578da0f510"
  val afterGenesisStateDigest: Digest = Base16.decode(afterGenesisStateDigestHex)

  def readOrGenerate(settings: ErgoSettings): Option[ErgoState[_]] = {
    val stateDir = new File(s"${settings.dataDir}/state")
    stateDir.mkdirs()

    if (stateDir.listFiles().isEmpty) {
      None
    } else {
      if (settings.ADState) DigestState.create(None, stateDir).toOption else Some(new UtxoState(stateDir))
    }
  }
}


/**
  * Tool to print new target digest in case of initial utxo state re-generation
  */
object DigestPrinter extends App {
  println(Base16.encode(ErgoState.generateGenesisUtxoState(new File("/tmp/ergo11/").ensuring(_.mkdirs()))._1.rootHash))
}