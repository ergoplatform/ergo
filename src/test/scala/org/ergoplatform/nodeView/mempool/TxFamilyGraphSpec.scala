package org.ergoplatform.nodeView.mempool

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import scorex.util.ModifierId

class TxFamilyGraphSpec extends AnyFlatSpec with Matchers {

  private def id(s: String): ModifierId = ModifierId @@ s

  private val tA = id("a")
  private val tB = id("b")
  private val tC = id("c")
  private val tD = id("d")

  "TxFamilyGraph.empty" should "have no parents or children" in {
    TxFamilyGraph.empty.parents shouldBe empty
    TxFamilyGraph.empty.children shouldBe empty
    TxFamilyGraph.empty.parentsOf(tA) shouldBe empty
    TxFamilyGraph.empty.childrenOf(tA) shouldBe empty
  }

  "addTx" should "register a root tx with no parents without populating either map" in {
    val g = TxFamilyGraph.empty.addTx(tA, Set.empty)
    g.parents shouldBe empty
    g.children shouldBe empty
    g.parentsOf(tA) shouldBe empty
  }

  it should "wire parent and back-edge for a single parent" in {
    val g = TxFamilyGraph.empty.addTx(tB, Set(tA))
    g.parentsOf(tB) shouldBe Set(tA)
    g.childrenOf(tA) shouldBe Set(tB)
  }

  it should "wire all parents and back-edges for multiple parents (diamond)" in {
    // tA, tB are roots; tC spends both; tD spends tC
    val g = TxFamilyGraph.empty
      .addTx(tC, Set(tA, tB))
      .addTx(tD, Set(tC))
    g.parentsOf(tC) shouldBe Set(tA, tB)
    g.childrenOf(tA) shouldBe Set(tC)
    g.childrenOf(tB) shouldBe Set(tC)
    g.parentsOf(tD) shouldBe Set(tC)
    g.childrenOf(tC) shouldBe Set(tD)
  }

  it should "be idempotent when called twice with the same parents" in {
    val g1 = TxFamilyGraph.empty.addTx(tB, Set(tA))
    val g2 = g1.addTx(tB, Set(tA))
    g2 shouldBe g1
  }

  it should "reconcile back-edges when called again with a different parent set" in {
    val g1 = TxFamilyGraph.empty.addTx(tC, Set(tA, tB))
    val g2 = g1.addTx(tC, Set(tB)) // tA is no longer a parent of tC
    g2.parentsOf(tC) shouldBe Set(tB)
    g2.childrenOf(tA) shouldBe empty
    g2.childrenOf(tB) shouldBe Set(tC)
    g2.children.keySet should not contain tA // pruned
  }

  "removeTx" should "be a no-op for an unknown id" in {
    val g = TxFamilyGraph.empty.addTx(tB, Set(tA))
    g.removeTx(tD) shouldBe g
  }

  it should "remove a leaf and clean the parent's back-edge" in {
    val g = TxFamilyGraph.empty.addTx(tB, Set(tA))
    val g2 = g.removeTx(tB)
    g2.parentsOf(tB) shouldBe empty
    g2.childrenOf(tA) shouldBe empty
    g2.parents.keySet should not contain tB
    g2.children.keySet should not contain tA // pruned
  }

  it should "remove a middle node, dropping its parents' back-edges and trimming children's parents" in {
    // chain tA -> tB -> tC
    val g = TxFamilyGraph.empty
      .addTx(tB, Set(tA))
      .addTx(tC, Set(tB))
    val g2 = g.removeTx(tB)
    g2.parents.keySet should not contain tB
    g2.children.keySet should not contain tB
    g2.childrenOf(tA) shouldBe empty // tA -> tB edge gone
    g2.parentsOf(tC) shouldBe empty // tB -> tC edge gone
  }

  it should "remove a root and trim parents of its direct children" in {
    val g = TxFamilyGraph.empty
      .addTx(tB, Set(tA))
      .addTx(tC, Set(tA))
    val g2 = g.removeTx(tA)
    g2.parentsOf(tB) shouldBe empty
    g2.parentsOf(tC) shouldBe empty
    g2.children.keySet should not contain tA
  }

  "ancestorsOf" should "return transitive parents and skip the start id itself" in {
    // chain tA -> tB -> tC -> tD
    val g = TxFamilyGraph.empty
      .addTx(tB, Set(tA))
      .addTx(tC, Set(tB))
      .addTx(tD, Set(tC))
    g.ancestorsOf(tD) shouldBe Set(tA, tB, tC)
    g.ancestorsOf(tA) shouldBe empty
  }

  it should "handle a diamond without duplicating shared ancestors" in {
    // tA -> tB, tA -> tC, tB -> tD, tC -> tD
    val g = TxFamilyGraph.empty
      .addTx(tB, Set(tA))
      .addTx(tC, Set(tA))
      .addTx(tD, Set(tB, tC))
    g.ancestorsOf(tD) shouldBe Set(tA, tB, tC)
  }

  "descendantsOf" should "return transitive children and skip the start id itself" in {
    val g = TxFamilyGraph.empty
      .addTx(tB, Set(tA))
      .addTx(tC, Set(tB))
      .addTx(tD, Set(tC))
    g.descendantsOf(tA) shouldBe Set(tB, tC, tD)
    g.descendantsOf(tD) shouldBe empty
  }

  it should "handle a diamond without duplicating shared descendants" in {
    val g = TxFamilyGraph.empty
      .addTx(tB, Set(tA))
      .addTx(tC, Set(tA))
      .addTx(tD, Set(tB, tC))
    g.descendantsOf(tA) shouldBe Set(tB, tC, tD)
  }

  "the graph" should "stay empty after add followed by remove of the same tx" in {
    val g = TxFamilyGraph.empty.addTx(tB, Set(tA)).removeTx(tB)
    g.parents shouldBe empty
    g.children shouldBe empty
  }
}
