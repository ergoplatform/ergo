package org.ergoplatform.utils

import sigma.VersionContext
import sigma.ast.{ErgoTree, SBoolean, SSigmaProp, Value}
import sigma.compiler.{CompilerResult, SigmaCompiler}
import sigma.compiler.ir.CompiletimeIRContext

import scala.util.{Failure, Success, Try}

/**
  * Common compilation related utils
  */
trait ErgoCompilerHelpers {

  private def compileSource(source: String, scriptVersion: Byte, treeVersion: Byte): ErgoTree = {
    VersionContext.withVersions(scriptVersion, treeVersion) {
      val compiler = new SigmaCompiler(16.toByte)
      val ergoTreeHeader = ErgoTree.defaultHeaderWithVersion(treeVersion)
      val ergoTree = Try(compiler.compile(Map.empty, source)(new CompiletimeIRContext)).flatMap {
        case CompilerResult(_, _, _, script: Value[SSigmaProp.type@unchecked]) if script.tpe == SSigmaProp =>
          Success(ErgoTree.fromProposition(ergoTreeHeader, script))
        case CompilerResult(_, _, _, script: Value[SBoolean.type@unchecked]) if script.tpe == SBoolean =>
          Success(ErgoTree.fromProposition(ergoTreeHeader, script.toSigmaProp))
        case other =>
          Failure(new Exception(s"Source compilation result is of type ${other.buildTree.tpe}, but `SBoolean` expected"))
      }.get
      ergoTree
    }
  }

  /**
    * Compile provided Ergoscript code in `source` with version 3 (block version 4) ErgoTree protocol activated,
    * generates tree of provided `treeVersion`
    */
  def compileSourceV5(source: String, treeVersion: Byte): ErgoTree = compileSource(source, 2, treeVersion)


  /**
    * Compile provided Ergoscript code in `source` with version 2 (block version 3) ErgoTree protocol activated,
    * generates tree of provided `treeVersion`
    */
  def compileSourceV6(source: String, treeVersion: Byte): ErgoTree = compileSource(source, 3, treeVersion)

}
