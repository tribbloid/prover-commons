package ai.acyclic.prover.commons.finset

import ai.acyclic.prover.commons.testlib.BaseSpec

class RightNestedSpec extends BaseSpec {

  object Fixture extends RightNested {
    type VBound = Any

    sealed trait Fin
    protected case object _Empty extends Fin

    case class Cons[+HEAD <: VBound, +TAIL <: Fin](head: HEAD, tail: TAIL) extends Fin
    type ><:[+HEAD <: VBound, +TAIL <: Fin] = Cons[HEAD, TAIL]

    override def cons[HEAD <: VBound, TAIL <: Fin](head: HEAD, tail: TAIL): HEAD ><: TAIL = Cons(head, tail)

    override def deCons[HEAD <: VBound, TAIL <: Fin](cons: HEAD ><: TAIL): (HEAD, TAIL) = (cons.head, cons.tail)
  }
  import Fixture.*

  describe("RightNested") {
    it("should construct correctly using cons") {
      val t: Int ><: String ><: Empty = cons(1, cons("a", Empty))
      val (h1, t1) = deCons(t)
      assert(h1 == 1)

      val (h2, t2) = deCons(t1)
      assert(h2 == "a")
      assert(t2 == Empty)
    }

    it("should construct correctly using operators") {
      val t: Int ><: String ><: Empty = 1 ><: "a" ><: Empty
      val (h1, t1) = deCons(t)
      assert(h1 == 1)

      val (h2, t2) = deCons(t1)
      assert(h2 == "a")
      assert(t2 == Empty)
    }

    it("should have correct associativity") {
      // 1 ><: "a" ><: Empty should be parsed as 1 ><: ("a" ><: Empty)
      // because operator ends in :, it is right-associative
      val t1 = 1 ><: "a" ><: Empty
      val t2 = 1 ><: ("a" ><: Empty)
      assert(t1 == t2)
    }
  }
}
