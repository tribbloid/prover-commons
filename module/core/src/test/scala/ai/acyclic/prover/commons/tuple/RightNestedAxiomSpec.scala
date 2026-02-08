package ai.acyclic.prover.commons.tuple

import ai.acyclic.prover.commons.testlib.BaseSpec

class RightNestedAxiomSpec extends BaseSpec {

  object Fixture extends RightNestedAxiom {
    type VBound = Any

    sealed trait Inductive
    protected case object _0 extends Inductive

    case class Cons[+HEAD <: VBound, +TAIL <: Inductive](head: HEAD, tail: TAIL) extends Inductive
    type ><:[+HEAD <: VBound, +TAIL <: Inductive] = Cons[HEAD, TAIL]

    def cons[HEAD <: VBound, TAIL <: Inductive](head: HEAD, tail: TAIL): HEAD ><: TAIL = Cons(head, tail)

    override def deCons[HEAD <: VBound, TAIL <: Inductive](cons: HEAD ><: TAIL): (HEAD, TAIL) = (cons.head, cons.tail)
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
  }
}
