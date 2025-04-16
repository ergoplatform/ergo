package org.ergoplatform.http.api.requests


/**
  * Represents a request to compile a source script into an ErgoTree.
  *
  * @param source      The ErgoScript source code to compile.
  * @param treeVersion The version of the ErgoTree (defaults to 0).
  */
case class CompileRequest(source: String, treeVersion: Byte)
