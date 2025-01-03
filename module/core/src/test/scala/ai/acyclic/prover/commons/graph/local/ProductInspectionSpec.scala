package ai.acyclic.prover.commons.graph.local

import ai.acyclic.prover.commons.graph.viz.LinkedHierarchy
import ai.acyclic.prover.commons.testlib.BaseSpec

class ProductInspectionSpec extends BaseSpec {

  import ProductInspectionSpec.*

  lazy val viz = LinkedHierarchy.Default

  describe("standard") {

    lazy val ii = ProductInspection

    it("tree") {

      viz
        .showNode(ii.inspect(tree0))
        .toString
        .shouldBe(
          """
          |+ C
          |!-+ B
          |: !-+ A
          |: : !-- 1
          |: : !-- a
          |: !-+ A
          |:   !-- 2
          |:   !-- b
          |!-- None
          |""".stripMargin
        )
    }

    it("multi-tree") {

      viz
        .showNode(ii.inspect(multiTree))
        .toString
        .shouldBe(
          """
          |+ C
          |!-+ B
          |: !-+ A ............................................................................... [0]
          |: : !-- 1
          |: : !-- a
          |: !-- A ... (see [0])
          |!-- None
          |""".stripMargin
        )
    }

    it("diamond") {

      viz
        .showNode(ii.inspect(diamond))
        .toString
        .shouldBe(
          """
            |
            |+ C
            |!-+ B
            |: !-+ A
            |: : !-- 1
            |: : !-- a ............................................................................... [0]
            |: !-+ A
            |:   !-- 2
            |:   !-- a ... (see [0])
            |!-- None
            |""".stripMargin
        )
    }

  }

  describe("named") {

    lazy val ii = ProductInspection.Named

    it("tree") {

      viz
        .showNode(ii.inspect(tree0))
        .toString
        .shouldBe(
          """
            |+ C
            |!-⟦ b ⟧+ B
            |:      !-⟦ a1 ⟧+ A
            |:      :       !-⟦ x ⟧- 1
            |:      :       !-⟦ y ⟧- a
            |:      !-⟦ a2 ⟧+ A
            |:              !-⟦ x ⟧- 2
            |:              !-⟦ y ⟧- b
            |!-⟦ c ⟧- None
            |""".stripMargin
        )
    }

    it("multi-tree") {

      viz
        .showNode(ii.inspect(multiTree))
        .toString
        .shouldBe(
          """
          |+ C
          |!-⟦ b ⟧+ B
          |:      !-⟦ a1 ⟧+ A ............................................................................... [0]
          |:      :       !-⟦ x ⟧- 1
          |:      :       !-⟦ y ⟧- a
          |:      !-⟦ a2 ⟧- A ... (see [0])
          |!-⟦ c ⟧- None
          |""".stripMargin
        )
    }
  }

}

object ProductInspectionSpec {

  case class A(x: Int, y: String)

  case class B(a1: A, a2: A)

  case class C(b: B, var c: Option[Double] = None)

  lazy val tree0 = C(B(A(1, "a"), A(2, "b")))

//  lazy val tree1 = C(B(A(1, "a"), A(2, "b")), Some(C(B(A(3, "c"), A(4, "d")))))

  lazy val multiTree = C(B(A(1, "a"), A(1, "a")))

  lazy val diamond = C(B(A(1, "a"), A(2, "a")))

}
