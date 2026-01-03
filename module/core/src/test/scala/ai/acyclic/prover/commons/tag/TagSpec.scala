package ai.acyclic.prover.commons.tag

import ai.acyclic.prover.commons.testlib.BaseSpec

class TagSpec extends BaseSpec {

  trait Ex {

    def fn(v: Int): Int
  }

  val ex: Ex = { v => v + 1 }

  trait T1 extends Tag {}
  trait T2 extends Tag {}
  trait T3 extends Tag {}
  trait T4 extends T3

  val et1: Ex <> T1 = Tag(ex).<>[T1]
  val et12: (Ex <> T1) <> T2 = Tag(et1).<>[T2]
  val et123: ((Ex <> T1) <> T2) <> T3 = Tag(et12).<>[T3]

  it("Tag arg is covariant") {

    implicitly[(Ex <> T4) <:< (Ex <> T3)]
  }

  describe("can enable at runtime") {

    it("by type casting") {

      assert(ex.asInstanceOf[Ex <> T1] == ex)
      assert(ex.asInstanceOf[Ex <> T1 <> T2] == ex)
    }

    it("by annotation") {

      assert(et1 == ex)
    }
  }

  describe("can revoke all") {

    it("by upcasting") {

      et1: Ex
      et12: Ex
      et12: Ex <> T1
      et12: Ex <> T2

      et12: (Ex <> T2) <> T1
      et123: ((Ex <> T3) <> T2) <> T1
      et123: ((Ex <> T2) <> T3) <> T1
    }

    it("by function") {

      {
        val revoked = Tag.revokeAll(et1)
        assert(revoked == et1)
        shouldNotCompile("revoked: Ex ^: Subject.Cap1")
      }

      {
        val revoked = Tag.revokeAll(et12)
        // buggy compiler! circumventing

        assert(revoked == et12)
        shouldNotCompile("revoked: Ex ^: Subject.Cap1")
      }
    }
  }

  it("equivalence of type regardless of being left/right associative") {

    implicitly[((Ex <> T1) <> T2) =:= ((Ex <> T1) <> T2)]

    et12
//    val d2: (Ex <> Cap1) <> Cap2 = d1
  }

  it("can swap") {

    val ex21: (Ex <> T2) <> T1 = et12

    assert(ex21 == et12)
  }

  describe("can revoke some") {

    it("by upcasting") {

      {
        val revoked: Ex <> T1 = et12
        assert(revoked == et12)
        shouldNotCompile("revoked: Ex ^: Subject.Cap2")
      }

      {
        val revoked: Ex <> T2 = et12
        assert(revoked == et12)
        shouldNotCompile("revoked: Ex ^: Subject.Cap1")
      }
    }

    it("by function") {

      {
        object revoke extends Tag.revoke[T1]

        val revoked = revoke(et12)(revoke.last[Ex <> T2, T1]) // fuck scala
        assert(revoked == et12)
        val _: Ex <> T2 = revoked
        shouldNotCompile("revoked: Ex <> Subject.Cap1")
      }

      {
        val revoke = Tag.revoke[T2]
        val revoked = revoke(et12)(revoke.last) // fuck scala
        assert(revoked == et12)
        val _: Ex <> T1 = revoked
        shouldNotCompile("revoked: Ex <> Subject.Cap2")
      }
    }
  }

}

object TagSpec {}
