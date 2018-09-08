package org.ergoplatform.utils

case class TestCase(name: String)(test: NodeViewFixture => Unit) {
  def run(c: NodeViewTestConfig): Unit = new NodeViewFixture(c.toSettings).apply(test)
}
