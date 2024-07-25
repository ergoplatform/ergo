package org.ergoplatform.wallet.protocol.context

import sigmastate.interpreter.ContextExtension

/**
  * Part of execution context regarding a box to be spent.
  * It includes index of the box in inputs and also context extension
  * (additional key-value pairs provided during the spending)
  *
  * @param selfIndex - index of the box in spending transaction inputs
  * @param extension - input-provided context extension
  */
case class InputContext(selfIndex: Short, extension: ContextExtension)
