package org.ergoplatform.bench

import java.io.File

import akka.actor.{ActorRef, ActorSystem}
import org.ergoplatform.bench.misc.TempDir
import org.ergoplatform.bench.misc.Utils.{elapsedTimeOf, _}
import org.ergoplatform.modifiers.ErgoFullBlock
import org.ergoplatform.modifiers.history.{BlockTransactions, Extension, Header}
import org.ergoplatform.nodeView.state._
import org.ergoplatform.settings.ErgoSettings
import scorex.core.idToVersion
import scorex.util.ScorexLogging

import scala.concurrent.ExecutionContextExecutor
import scala.language.postfixOps

object StateSnapshotTakingBench extends ScorexLogging {

  implicit val system: ActorSystem = ActorSystem("bench")
  implicit val ec: ExecutionContextExecutor = system.dispatcher

  val targetDirectory = "target/bench"

  def main(args: Array[String]): Unit = {
    new File(targetDirectory).mkdirs()
    val threshold = args.headOption.getOrElse("1000").toInt
    val fileUrl = args.lift(1).get
    val userDir = TempDir.createTempDir

    log.info(s"User dir is $userDir")
    log.info("Starting benchmark.")

    val settings = ErgoSettings.read(None)
    val nodeSettings = settings.nodeSettings.copy(stateType = StateType.Utxo, snapshotCreationInterval = threshold - 1)

    lazy val ergoSettings: ErgoSettings = settings
      .copy(directory = userDir.getAbsolutePath, nodeSettings = nodeSettings)

    log.info(s"Setting to be used:")
    log.info(s"$ergoSettings")

    val modifiers = readModifiers(fileUrl, threshold)

    val blocks = modifiers.foldLeft(Seq.empty[ErgoFullBlock]) {
      case (acc, header: Header) =>
        modifiers
          .collectFirst { case bt: BlockTransactions if bt.headerId == header.id => bt }
          .map(bt => acc :+ ErgoFullBlock(header, bt, Extension(header.id, Seq.empty, Seq.empty), None))
          .getOrElse(acc)
      case (acc, _) =>
        acc
    }

    var state = createUtxoState(settings, None)._1

    blocks.foreach(b => state = state.applyModifier(b).get)

    log.info(s"State is at height ${state.stateContext.currentHeight}")

    val destinationBlock = blocks.dropRight(10).last

    state = state.rollbackTo(idToVersion(destinationBlock.id)).get

    log.info(s"State height after rollback is ${state.stateContext.currentHeight}")

    val result = elapsedTimeOf {
      state.takeSnapshot
    }

    log.info(s"Elapsed time of taking state snapshot at height ${destinationBlock.header.height} is $result millis")

    System.exit(0)
  }

  private def createUtxoState(settings: ErgoSettings,
                              nodeViewHolderRef: Option[ActorRef] = None): (UtxoState, BoxHolder) = {
    val constants = StateConstants(nodeViewHolderRef, settings)
    ErgoState.generateGenesisUtxoState(TempDir.createTempDir, constants, settings)
  }

}

