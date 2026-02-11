package ai.acyclic.prover.commons.tuple

import ai.acyclic.prover.commons.testlib.BaseSpec

class RightAssociatedSpec extends BaseSpec {

  object Fixture extends RightAssociated {
    type VBound = Any

    sealed trait Prod
    protected case object _1 extends Prod

    case class Cons[+HEAD <: VBound, +TAIL <: Prod](head: HEAD, tail: TAIL) extends Prod
    type ><:[+HEAD <: VBound, +TAIL <: Prod] = Cons[HEAD, TAIL]

    def cons[HEAD <: VBound, TAIL <: Prod](head: HEAD, tail: TAIL): HEAD ><: TAIL = Cons(head, tail)

    override def deCons[HEAD <: VBound, TAIL <: Prod](cons: HEAD ><: TAIL): (HEAD, TAIL) = (cons.head, cons.tail)
  }
  import Fixture.*

  describe("RightNested") {
    it("should construct correctly using cons") {
      val t: Int ><: String ><: Eye = cons(1, cons("a", Eye))
      val (h1, t1) = deCons(t)
      assert(h1 == 1)

      val (h2, t2) = deCons(t1)
      assert(h2 == "a")
      assert(t2 == Eye)
    }
  }
}
