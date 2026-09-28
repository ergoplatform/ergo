package org.ergoplatform.tools

import com.google.common.primitives.Ints
import org.ergoplatform.modifiers.history.BlockTransactionsSerializer
import org.ergoplatform.modifiers.history.header.HeaderSerializer
import org.ergoplatform.settings.Algos
import scorex.db.LDBFactory
import scorex.util.{ModifierId, bytesToId, idToBytes}

import scala.util.{Failure, Success}

/**
  * Scans the whole blockchain stored in a local node database block-by-block and checks
  * that every transaction has no duplicated data inputs
  * (consensus rule `txDataInputsUnique`, id 110, see ValidationRules).
  *
  * Usage:
  *   sbt "Test/runMain org.ergoplatform.tools.DuplicateDataInputsChecker /path/to/ergo/data/dir"
  *
  * where /path/to/ergo/data/dir is the node data directory containing the `history` folder
  * (e.g. ~/.ergo). Note: stop the node first (or point the tool at a copy of the database),
  * as a LevelDB database can not be opened by two processes at the same time.
  *
  * The tool walks the best (main) chain by height, and also checks fork blocks stored
  * at the same heights. Blocks whose transactions section is not stored locally
  * (e.g. on a pruning node) are reported as skipped.
  *
  * Exit code is 0 if no violations found in the best chain, 1 otherwise.
  */
object DuplicateDataInputsChecker {

  case class Violation(height: Int,
                       blockId: ModifierId,
                       txId: ModifierId,
                       duplicatedDataInputs: Seq[ModifierId],
                       inBestChain: Boolean)

  case class CheckReport(chainHeight: Int,
                         blocksChecked: Long,
                         forkBlocksChecked: Long,
                         missingSections: Long,
                         unparsedBlocks: Long,
                         txsChecked: Long,
                         txsWithDataInputs: Long,
                         violations: Seq[Violation]) {
    def bestChainViolations: Seq[Violation] = violations.filter(_.inBestChain)
  }

  // same as HeadersProcessor.heightIdsKey
  private def heightIdsKey(height: Int): Array[Byte] = Algos.hash(Ints.toByteArray(height))

  def check(dir: String, progressEvery: Int = 50000): CheckReport = {
    val indexStore = LDBFactory.createKvDb(s"$dir/history/index")
    val objectsStore = LDBFactory.createKvDb(s"$dir/history/objects")

    var violations = Seq.empty[Violation]
    var blocksChecked = 0L
    var forkBlocksChecked = 0L
    var txsChecked = 0L
    var txsWithDataInputs = 0L
    var missingSections = 0L
    var unparsedBlocks = 0L

    var height = 1
    var continue = true

    while (continue) {
      indexStore.get(heightIdsKey(height)) match {
        case Some(idsBytes) =>
          // first header id at a height is always from the best headers chain,
          // ids after it (if any) are from forks
          val headerIds = idsBytes.grouped(32).map(bytesToId).toSeq
          headerIds.zipWithIndex.foreach { case (headerId, idx) =>
            val inBestChain = idx == 0
            objectsStore.get(idToBytes(headerId)) match {
              case Some(headerRecord) =>
                // first byte of a stored record is modifier type id, tail is the modifier itself
                val header = HeaderSerializer.parseBytes(headerRecord.tail)
                objectsStore.get(idToBytes(header.transactionsId)) match {
                  case Some(btRecord) =>
                    BlockTransactionsSerializer.parseBytesTry(btRecord.tail) match {
                      case Success(bt) =>
                        blocksChecked += 1
                        if (!inBestChain) forkBlocksChecked += 1
                        bt.txs.foreach { tx =>
                          txsChecked += 1
                          if (tx.dataInputs.nonEmpty) txsWithDataInputs += 1
                          if (tx.dataInputs.distinct.size != tx.dataInputs.size) {
                            val duplicated = tx.dataInputs
                              .groupBy(di => bytesToId(di.boxId))
                              .collect { case (boxId, occurrences) if occurrences.size > 1 => boxId }
                              .toSeq
                            val violation = Violation(height, bt.headerId, tx.id, duplicated, inBestChain)
                            violations = violations :+ violation
                            println(s"Violation: height $height, block ${Algos.encode(bt.headerId)}, " +
                              s"tx ${Algos.encode(tx.id)}, duplicated data inputs " +
                              s"${duplicated.map(Algos.encode).mkString(", ")}" +
                              (if (!inBestChain) " (fork block)" else ""))
                          }
                        }
                      case Failure(e) =>
                        unparsedBlocks += 1
                        println(s"Failed to parse block transactions of block $headerId " +
                          s"at height $height: ${e.getMessage}")
                    }
                  case None =>
                    missingSections += 1
                    println(s"Skipping block $headerId at height $height: " +
                      s"transactions section is not stored locally (pruned?)")
                }
              case None =>
                println(s"Unexpected: header $headerId at height $height is in the index " +
                  s"but not in the objects database")
            }
          }
          height += 1
          if (progressEvery > 0 && height % progressEvery == 0) {
            println(s"Progress: height $height, blocks checked: $blocksChecked, " +
              s"transactions checked: $txsChecked, violations so far: ${violations.size}")
          }
        case None =>
          continue = false
      }
    }

    indexStore.close()
    objectsStore.close()

    CheckReport(
      chainHeight = height - 1,
      blocksChecked = blocksChecked,
      forkBlocksChecked = forkBlocksChecked,
      missingSections = missingSections,
      unparsedBlocks = unparsedBlocks,
      txsChecked = txsChecked,
      txsWithDataInputs = txsWithDataInputs,
      violations = violations
    )
  }

  def printReport(report: CheckReport): Unit = {
    println("=====================")
    println(s"Chain height scanned: ${report.chainHeight}")
    println(s"Blocks checked: ${report.blocksChecked} (of them fork blocks: ${report.forkBlocksChecked})")
    println(s"Blocks skipped (transactions section not stored): ${report.missingSections}")
    println(s"Blocks failed to parse: ${report.unparsedBlocks}")
    println(s"Transactions checked: ${report.txsChecked} " +
      s"(of them with data inputs: ${report.txsWithDataInputs})")
    println(s"Total violations: ${report.violations.size} " +
      s"(in the best chain: ${report.bestChainViolations.size})")
    if (report.violations.nonEmpty) {
      val heights = report.violations.map(_.height)
      println(s"Violation heights: from ${heights.min} to ${heights.max}")
    }

    report.bestChainViolations.foreach { v =>
      println(s"  height ${v.height}, block ${Algos.encode(v.blockId)}, tx ${Algos.encode(v.txId)}, " +
        s"duplicated data inputs ${v.duplicatedDataInputs.map(Algos.encode).mkString(", ")}")
    }
  }

  def main(args: Array[String]): Unit = {
    if (args.isEmpty) {
      println("Usage: DuplicateDataInputsChecker <path to ergo data directory, e.g. ~/.ergo>")
      println("Note: stop the node first (or point the tool at a copy of the database), " +
        "a LevelDB database can not be opened by two processes at the same time")
      sys.exit(1)
    }

    val report = check(args(0))
    printReport(report)

    if (report.bestChainViolations.nonEmpty) {
      sys.exit(1)
    } else if (report.missingSections > 0 || report.unparsedBlocks > 0) {
      println("WARNING: some blocks were not checked, see messages above")
    } else {
      println("OK: no duplicated data inputs found in the whole blockchain")
    }
  }
}
