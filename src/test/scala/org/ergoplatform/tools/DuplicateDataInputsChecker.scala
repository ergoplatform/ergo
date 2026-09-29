package org.ergoplatform.tools

import com.google.common.primitives.Ints
import org.ergoplatform.modifiers.history.BlockTransactionsSerializer
import org.ergoplatform.modifiers.history.header.HeaderSerializer
import org.ergoplatform.settings.Algos
import scorex.db.LDBFactory
import scorex.util.{ModifierId, bytesToId, idToBytes}
import sigma.ast._

import scala.util.{Failure, Success}

/**
  * Scans the whole blockchain stored in a local node database block-by-block and checks
  * that no historical data violates the tightened consensus rules:
  *
  *  - no more than one pair of data inputs with the same box id in any transaction
  *    (consensus rule `txDataInputsUnique`, id 110, see ValidationRules);
  *  - no SUnit types in box registers and context extension values
  *    (tightened sigma-state rule #1019 CheckV6Type, sertests branch of
  *    sigmastate-interpreter);
  *  - no collections with zero-width element types (SUnit and composites built only from
  *    it) in box registers, context extension values and ErgoTree constants
  *    (new sigma-state rule #1020 CheckZeroWidthCollection);
  *  - no type descriptors nested deeper than MaxTypeDepth in box registers, context
  *    extension values and ErgoTree constants (SigmaConstants.MaxTypeDepth limit in
  *    TypeSerializer).
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
  * Known limitation: type descriptor depth is computed for the canonical encoding of the
  * parsed type; a pathological non-canonical encoding of the same type in historical
  * bytes could nest deeper. Also, only the segregated constant pool of ErgoTrees is
  * checked (practically all on-chain trees are constant-segregated).
  *
  * Exit code is 0 if no violations found in the best chain, 1 otherwise.
  */
object DuplicateDataInputsChecker {

  case class Violation(height: Int,
                       blockId: ModifierId,
                       txId: ModifierId,
                       overReferencedBoxes: Seq[ModifierId],
                       inBestChain: Boolean)

  case class TypeViolation(height: Int,
                           blockId: ModifierId,
                           txId: ModifierId,
                           location: String,
                           rule: String,
                           tpe: String,
                           inBestChain: Boolean)

  case class CheckReport(chainHeight: Int,
                         blocksChecked: Long,
                         forkBlocksChecked: Long,
                         missingSections: Long,
                         unparsedBlocks: Long,
                         txsChecked: Long,
                         txsWithDataInputs: Long,
                         violations: Seq[Violation],
                         typeViolations: Seq[TypeViolation]) {
    def bestChainViolations: Seq[Violation] = violations.filter(_.inBestChain)
    def bestChainTypeViolations: Seq[TypeViolation] = typeViolations.filter(_.inBestChain)
  }

  /** Max nesting depth of a type descriptor allowed by the type deserializer.
    * Mirrors SigmaConstants.MaxTypeDepth from the sertests branch of sigmastate-interpreter
    * (not present in sigma-state 6.0.6 this codebase is compiled against). */
  val MaxTypeDepth: Int = 8

  // same as HeadersProcessor.heightIdsKey
  private def heightIdsKey(height: Int): Array[Byte] = Algos.hash(Ints.toByteArray(height))

  /** Zero-width types occupy no bytes in serialization: SUnit itself, collections of
    * zero-width elements, and tuples whose items are all zero-width.
    * Mirrors CoreDataSerializer.isZeroWidth from the sertests branch. */
  private def isZeroWidth(tpe: SType): Boolean = tpe match {
    case SUnit => true
    case t: STuple => t.items.forall(isZeroWidth)
    case tc: SCollectionType[_] => isZeroWidth(tc.elemType)
    case _ => false
  }

  /** Whether the type descriptor contains SUnit anywhere.
    * Mirrors the CheckV6Type tightening (SUnit added to forbidden types) on the
    * sertests branch. */
  private def containsUnit(tpe: SType): Boolean = tpe match {
    case SUnit => true
    case t: STuple => t.items.exists(containsUnit)
    case tc: SCollectionType[_] => containsUnit(tc.elemType)
    case so: SOption[_] => containsUnit(so.elemType)
    case sf: SFunc => sf.tDom.exists(containsUnit) || containsUnit(sf.tRange)
    case _ => false
  }

  /** Whether the type descriptor contains a collection with zero-width element type
    * anywhere. Mirrors the new CheckZeroWidthCollection rule on the sertests branch
    * (which fires on collection deserialization). */
  private def containsZeroWidthCollection(tpe: SType): Boolean = tpe match {
    case t: STuple => t.items.exists(containsZeroWidthCollection)
    case tc: SCollectionType[_] =>
      isZeroWidth(tc.elemType) || containsZeroWidthCollection(tc.elemType)
    case so: SOption[_] => containsZeroWidthCollection(so.elemType)
    case sf: SFunc => sf.tDom.exists(containsZeroWidthCollection) || containsZeroWidthCollection(sf.tRange)
    case _ => false
  }

  private def isEmbeddable(tpe: SType): Boolean = tpe.isInstanceOf[SPrimType]

  /** Nesting depth the type deserializer would reach when parsing the canonical
    * serialization of the given type. Mirrors TypeSerializer.deserialize depth counting
    * (depth is incremented per nested deserialize call; embeddable shortcut encodings
    * do not recurse). */
  private def descriptorDepth(tpe: SType): Int = tpe match {
    case t: STuple =>
      if (t.items.length == 2 && t.items.distinct.size == 1 && t.items.forall(isEmbeddable)) {
        0 // symmetric pair of embeddable types is a single byte
      } else {
        1 + t.items.map(descriptorDepth).max
      }
    case tc: SCollectionType[_] =>
      if (isEmbeddable(tc.elemType)) {
        0 // collection of embeddable type is a single byte
      } else {
        tc.elemType match {
          case SCollectionType(inner) if isEmbeddable(inner) =>
            0 // collection of collection of embeddable type is two bytes, no recursion
          case elem => 1 + descriptorDepth(elem)
        }
      }
    case so: SOption[_] =>
      if (isEmbeddable(so.elemType)) {
        0 // option of embeddable type is a single byte
      } else {
        so.elemType match {
          case SCollectionType(inner) if isEmbeddable(inner) =>
            0 // option of collection of embeddable type is two bytes, no recursion
          case elem => 1 + descriptorDepth(elem)
        }
      }
    case sf: SFunc => 1 + (sf.tDom.map(descriptorDepth) :+ descriptorDepth(sf.tRange)).max
    case _ => 0
  }

  /** Rules applied to box registers and context extension values (CheckV6Type is
    * register/extension-specific, so SUnit is checked only here). */
  private def registerOrExtensionTypeViolations(tpe: SType): Seq[String] = {
    val unitViolation =
      if (containsUnit(tpe)) Seq("type contains SUnit (tightened rule #1019 CheckV6Type)") else Seq.empty
    unitViolation ++ treeConstantTypeViolations(tpe)
  }

  /** Rules applied to any deserialized type, including ErgoTree constants
    * (SUnit is legitimate inside scripts, so not checked here). */
  private def treeConstantTypeViolations(tpe: SType): Seq[String] = {
    val zeroWidthViolation =
      if (containsZeroWidthCollection(tpe)) {
        Seq("collection with zero-width element type (new rule #1020 CheckZeroWidthCollection)")
      } else {
        Seq.empty
      }
    val depth = descriptorDepth(tpe)
    val depthViolation =
      if (depth > MaxTypeDepth) {
        Seq(s"type descriptor nesting depth $depth exceeds MaxTypeDepth = $MaxTypeDepth")
      } else {
        Seq.empty
      }
    zeroWidthViolation ++ depthViolation
  }

  def check(dir: String, progressEvery: Int = 50000): CheckReport = {
    val indexStore = LDBFactory.createKvDb(s"$dir/history/index")
    val objectsStore = LDBFactory.createKvDb(s"$dir/history/objects")

    var violations = Seq.empty[Violation]
    var typeViolations = Seq.empty[TypeViolation]
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
                          if (tx.dataInputs.size - tx.dataInputs.distinct.size > 1) {
                            // more than one pair of data inputs with the same box id
                            val overReferenced = tx.dataInputs
                              .groupBy(di => bytesToId(di.boxId))
                              .collect { case (boxId, occurrences) if occurrences.length > 1 => boxId }
                              .toSeq
                            val violation = Violation(height, bt.headerId, tx.id, overReferenced, inBestChain)
                            violations = violations :+ violation
                            println(s"Violation: height $height, block ${Algos.encode(bt.headerId)}, " +
                              s"tx ${Algos.encode(tx.id)}, more than one pair of data inputs " +
                              s"with the same box id: ${overReferenced.map(Algos.encode).mkString(", ")}" +
                              (if (!inBestChain) " (fork block)" else ""))
                          }

                          // checks for compliance with the tightened sigma-state
                          // deserialization rules (sertests branch, sigma-state > 6.0.6)
                          tx.inputs.zipWithIndex.foreach { case (input, inputIndex) =>
                            input.spendingProof.extension.values.foreach { case (varId, v) =>
                              registerOrExtensionTypeViolations(v.tpe).foreach { rule =>
                                val tv = TypeViolation(height, bt.headerId, tx.id,
                                  s"input #$inputIndex context extension var $varId", rule,
                                  v.tpe.toString, inBestChain)
                                typeViolations = typeViolations :+ tv
                                println(s"Type violation: height $height, " +
                                  s"block ${Algos.encode(bt.headerId)}, tx ${Algos.encode(tx.id)}, " +
                                  s"${tv.location}, $rule, type ${tv.tpe}" +
                                  (if (!inBestChain) " (fork block)" else ""))
                              }
                            }
                          }
                          tx.outputCandidates.zipWithIndex.foreach { case (out, outIndex) =>
                            out.additionalRegisters.foreach { case (regId, v) =>
                              registerOrExtensionTypeViolations(v.tpe).foreach { rule =>
                                val tv = TypeViolation(height, bt.headerId, tx.id,
                                  s"output #$outIndex register $regId", rule,
                                  v.tpe.toString, inBestChain)
                                typeViolations = typeViolations :+ tv
                                println(s"Type violation: height $height, " +
                                  s"block ${Algos.encode(bt.headerId)}, tx ${Algos.encode(tx.id)}, " +
                                  s"${tv.location}, $rule, type ${tv.tpe}" +
                                  (if (!inBestChain) " (fork block)" else ""))
                              }
                            }
                            out.ergoTree.constants.zipWithIndex.foreach { case (c, constIndex) =>
                              treeConstantTypeViolations(c.tpe).foreach { rule =>
                                val tv = TypeViolation(height, bt.headerId, tx.id,
                                  s"output #$outIndex ergoTree constant #$constIndex", rule,
                                  c.tpe.toString, inBestChain)
                                typeViolations = typeViolations :+ tv
                                println(s"Type violation: height $height, " +
                                  s"block ${Algos.encode(bt.headerId)}, tx ${Algos.encode(tx.id)}, " +
                                  s"${tv.location}, $rule, type ${tv.tpe}" +
                                  (if (!inBestChain) " (fork block)" else ""))
                              }
                            }
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
              s"transactions checked: $txsChecked, violations so far: " +
              s"${violations.size + typeViolations.size}")
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
      violations = violations,
      typeViolations = typeViolations
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
    println(s"Duplicated data inputs pairs violations: ${report.violations.size} " +
      s"(in the best chain: ${report.bestChainViolations.size})")
    if (report.violations.nonEmpty) {
      val heights = report.violations.map(_.height)
      println(s"Duplicated data inputs pairs violation heights: from ${heights.min} to ${heights.max}")
    }
    println(s"Type violations (tightened sigma-state rules): ${report.typeViolations.size} " +
      s"(in the best chain: ${report.bestChainTypeViolations.size})")
    if (report.typeViolations.nonEmpty) {
      val heights = report.typeViolations.map(_.height)
      println(s"Type violation heights: from ${heights.min} to ${heights.max}")
    }

    report.bestChainViolations.foreach { v =>
      println(s"  height ${v.height}, block ${Algos.encode(v.blockId)}, tx ${Algos.encode(v.txId)}, " +
        s"boxes in duplicated data input pairs: " +
        s"${v.overReferencedBoxes.map(Algos.encode).mkString(", ")}")
    }
    report.bestChainTypeViolations.foreach { v =>
      println(s"  height ${v.height}, block ${Algos.encode(v.blockId)}, tx ${Algos.encode(v.txId)}, " +
        s"${v.location}, ${v.rule}, type ${v.tpe}")
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

    if (report.bestChainViolations.nonEmpty || report.bestChainTypeViolations.nonEmpty) {
      sys.exit(1)
    } else if (report.missingSections > 0 || report.unparsedBlocks > 0) {
      println("WARNING: some blocks were not checked, see messages above")
    } else {
      println("OK: no violations of the tightened rules found in the whole blockchain")
    }
  }
}
