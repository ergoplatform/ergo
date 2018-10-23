package org.ergoplatform.utils

import org.ergoplatform.utils.fixtures.NodeViewFixture

case class TestCase(name: String)(test: NodeViewFixture => Unit) {
  def run(c: NodeViewTestConfig): Unit = new NodeViewFixture(c.toSettings).apply(test)
}
