package ai.acyclic.prover.commons.congr

import org.scalatest.funspec.AnyFunSpec

class CongruentSpec extends AnyFunSpec {}

object CongruentSpec {

  {
    // given type congruence
    class T1 extends Congruent[T1, T1] {
      type S

      type SS
    }

    def getT1[T <: T1]: T = ???

    {
      val t1 = getT1[T1]
      val t2 = getT1[T1]

      given (t1.type === t2.type) = ???

      // sanity check
      summon[t1.S =:= t2.S]
      summon[t1.SS =:= t2.SS]
    }

  }

  {
    // given singleton types and equality
    case class S2() {

      type S
    }
    object S2 extends CongruentSupport
    val sx = new S2()
    val sy = sx

    summon[sx.type <:< Singleton]
    summon[sy.type <:< Singleton]

    given (sx.type =:= sy.type) = ???

//    import Congruent.given
    summon[sx.S =:= sy.S]
  }
}
