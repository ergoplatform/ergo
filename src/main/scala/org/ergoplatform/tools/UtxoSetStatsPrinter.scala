package org.ergoplatform.tools

import java.io.File

import scorex.crypto.authds.avltree.batch.VersionedLDBAVLStorage
import scorex.db.LDBVersionedStore

import scala.util.{Failure, Success}

/**
  * Standalone utility printing statistics about a node's UTXO set and the database backing its
  * authenticating AVL+ tree: record/box counts and sizes for both the physical state store and the
  * current UTXO set. Useful for economic modelling and planning storage optimizations.
  *
  * Run with the node stopped (LevelDB locks the state directory):
  * {{{
  *   sbt "runMain org.ergoplatform.tools.UtxoSetStatsPrinter <path-to-state-dir>"
  * }}}
  * where `<path-to-state-dir>` is the node's `state` directory (the one containing the
  * `ldb_main` / `ldb_undo` subdirectories).
  */
object UtxoSetStatsPrinter extends App {

  if (args.isEmpty) {
    println("Usage: UtxoSetStatsPrinter <path-to-state-dir>")
    sys.exit(1)
  }

  val dir = new File(args(0))
  if (!dir.isDirectory) {
    println(s"State directory not found: ${dir.getAbsolutePath}")
    sys.exit(1)
  }

  def mib(bytes: Long): String = f"${bytes.toDouble / (1024 * 1024)}%.2f MiB"

  val store = new LDBVersionedStore(dir, initialKeepVersions = 0)
  try {
    new VersionedLDBAVLStorage(store).collectStats match {
      case Success(s) =>
        println(s"UTXO set statistics for ${dir.getAbsolutePath}")
        println()
        println("Physical state store (all records in the database):")
        println(s"  total records:        ${s.totalRecords}")
        println(s"  total key bytes:      ${s.totalKeyBytes} (${mib(s.totalKeyBytes)})")
        println(s"  total value bytes:    ${s.totalValueBytes} (${mib(s.totalValueBytes)})")
        println(s"  leaf records:         ${s.leafRecords}")
        println(s"    box value bytes:    ${s.leafValueBytes} (${mib(s.leafValueBytes)})")
        println(s"    leaf record bytes:  ${s.leafRecordBytes} (${mib(s.leafRecordBytes)})")
        println(s"  internal records:     ${s.internalRecords}")
        println(s"    record bytes:       ${s.internalRecordBytes} (${mib(s.internalRecordBytes)})")
        println(s"  other records:        ${s.otherRecords}")
        println(s"    record bytes:       ${s.otherRecordBytes} (${mib(s.otherRecordBytes)})")
        println()
        println("Live UTXO set (current AVL+ tree):")
        println(s"  boxes:                ${s.liveBoxes}")
        println(s"  box value bytes:      ${s.liveBoxValueBytes} (${mib(s.liveBoxValueBytes)})")
        println(s"  internal nodes:       ${s.liveInternalNodes}")
        println(s"  tree height:          ${s.treeHeight}")
      case Failure(t) =>
        println(s"Failed to collect UTXO set statistics: ${t.getMessage}")
        t.printStackTrace()
    }
  } finally {
    store.close()
  }
}
