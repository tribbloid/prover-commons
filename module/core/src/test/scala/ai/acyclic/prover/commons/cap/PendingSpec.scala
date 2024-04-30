package ai.acyclic.prover.commons.cap

import ai.acyclic.prover.commons.cap.Pending.{<<, OrNull}
import ai.acyclic.prover.commons.testlib.BaseSpec

class PendingSpec extends BaseSpec {

  describe("OrNull") {

    it("can execute as the only effect") {

      {
        val v = "abc" <<: OrNull
        val vv = v
        assert(vv.asOption.contains("abc"))
      }

      {
        val v: String << OrNull = null <<: OrNull
        assert(v.asOption.isEmpty)
      }
    }
  }
}

object PendingSpec {

  import ai.acyclic.prover.commons.cap.Pending._

  trait IO1 {}

  object IO1 {}

  trait IO2 {}

  trait IO3 {}

  trait Ex {

    def fn(v: Int): Int
  }

  //  trait Ext[C <: Subject.Capability] extends Ex {}

  val ex0: Ex = { v => v + 1 }

  val ex1: Ex << IO1 = ex0

//  val ex12: Ex << IO1 << IO2 = ex1 // only works in Scala 3
  val ex12: Ex << (IO1 with IO2) = ex1

  val ex123: Ex << (IO1 with IO2 with IO3) = ex12
}
