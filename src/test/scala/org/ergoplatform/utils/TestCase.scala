package org.ergoplatform.utils

import org.ergoplatform.settings.Parameters
import org.ergoplatform.utils.fixtures.NodeViewFixture

case class TestCase(name: String)(test: NodeViewFixture => Unit) {
  def run(parameters: Parameters, c: NodeViewTestConfig): Unit =
    new NodeViewFixture(c.toSettings, parameters).apply(test)
}
