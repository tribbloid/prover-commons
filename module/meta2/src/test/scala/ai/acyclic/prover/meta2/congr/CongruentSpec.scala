package ai.acyclic.prover.meta2.congr

import org.scalatest.funspec.AnyFunSpec

class CongruentSpec extends AnyFunSpec {}

object CongruentSpec {

  {
    // given type congruence
    import ai.acyclic.prover.meta2.congr.Congruent.*
    class T1 extends Congruent[T1, T1] {
      type S

      type SS
    }

    def getT1[T <: T1]: T = ???

    {
      val t1 = getT1[T1]
      val t2 = getT1[T1]

      implicit val lemma: Congruent[t1.type, t2.type] = ???

      // sanity check
      implicitly[t1.S =:= t2.S]
      implicitly[t1.SS =:= t2.SS]
    }

  }

  {
    // given singleton types and equality
    import ai.acyclic.prover.meta2.congr.Congruent.*
    case class S2() {

      type S
    }
    object S2 extends CongruentSupport
    val sx = new S2()
    val sy = sx

    implicitly[sx.type <:< Singleton]
    implicitly[sy.type <:< Singleton]

    implicit val lemma: Congruent[sx.type, sy.type] = ???

//    import Congruent.given
    implicitly[sx.S =:= sy.S]
  }
}
