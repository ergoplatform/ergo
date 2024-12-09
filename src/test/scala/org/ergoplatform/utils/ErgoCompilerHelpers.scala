package org.ergoplatform.utils

import sigma.VersionContext
import sigma.ast.{ErgoTree, SBoolean, SSigmaProp, Value}
import sigma.compiler.{CompilerResult, SigmaCompiler}
import sigma.compiler.ir.CompiletimeIRContext

import scala.util.{Failure, Success, Try}

trait ErgoCompilerHelpers {

  private def compileSource(source: String, scriptVersion: Byte) = {
    VersionContext.withScriptVersion(scriptVersion) {
      val compiler = new SigmaCompiler(16.toByte)
      val ergoTreeHeader = ErgoTree.defaultHeaderWithVersion(1.toByte)
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

  def compileSourceV5(source: String): ErgoTree = compileSource(source, 2)
  def compileSourceV6(source: String): ErgoTree = compileSource(source, 3)

}
