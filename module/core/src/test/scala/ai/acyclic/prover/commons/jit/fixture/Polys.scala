package ai.acyclic.prover.commons.jit.fixture

import ai.acyclic.prover.commons.jit.hom.Poly

object Polys {

  object _poly extends Poly {

    implicit lazy val int: Int /=> Int = {

      at[Int](v => v + 1)
    }

    implicit lazy val str: String /=> String = {
      at[String].to[String](v => v + "1")
    }

    case class __sanity[I, O]() {

      object s1 {
        def useIO(l: _poly.Case[Int, String]) = {
          l.apply(1)
        }

        def useI(l: _poly.Case.At[Int]) = {
          l.apply(1)
        }
      }

      object s2 {
        def useIO(l: _poly.Case[I, O]) = {
          val v: I = ???
          l.apply(v)
        }

        def useI(l: _poly.Case.At[I]) = {
          val v: I = ???
          l.apply(v)
        }
      }
    }
  }
}
