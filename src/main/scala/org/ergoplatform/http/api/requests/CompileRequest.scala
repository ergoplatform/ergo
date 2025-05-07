package org.ergoplatform.http.api.requests


/**
  * Represents a request to compile a source script into an ErgoTree.
  *
  * @param source      ErgoScript source code to compile
  * @param treeVersion Version of target ErgoTree
  */
case class CompileRequest(source: String, treeVersion: Byte)
