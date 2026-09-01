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

  /**
    * Compiles ErgoScript source code into an ErgoTree.
    *
    * @param source The ErgoScript source code string to compile
    * @param scriptVersion The version of the script compiler to use
    * @param treeVersion The version of the ErgoTree to generate
    * @return The compiled ErgoTree representing the source code
    * @throws Exception if compilation fails or if the result is not of expected type (SBoolean or SSigmaProp)
    */
  def compileSource(source: String, scriptVersion: Byte, treeVersion: Byte): ErgoTree = {
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

  def compileSourceV5(source: String, treeVersion: Byte): ErgoTree = compileSource(source, 2, treeVersion)
  def compileSourceV6(source: String, treeVersion: Byte): ErgoTree = compileSource(source, 3, treeVersion)

}
