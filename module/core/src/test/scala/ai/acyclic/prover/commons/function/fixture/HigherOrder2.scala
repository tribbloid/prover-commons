package ai.acyclic.prover.commons.function.fixture

import ai.acyclic.prover.commons.function.Hom.:=>

object HigherOrder2 {

  import Fns._

  { // 2-cat
    {
      fn1.andThen {

        :=>.at[Long :=> Seq[Double]] { _fn2 =>
          :=>.at[Seq[Long]] { o1 =>
            o1.flatMap(_fn2)
          }
        }
          .apply(fn2)
      }
    }

    {

      fn1.andThen {

        val result = for (ff <- fn2.^) yield {

          :=>.at[Seq[Long]] { v =>
            v.flatMap(ff)
          }
        }

        result
      }
    }

    {
      for (
        o1 <- fn1.out;
        ff <- fn2.^
      ) yield {

        o1.flatMap(ff)
      }
    }

    {
      for (
        ff <- fn2.^;
        o1 <- fn1.out
      ) yield {

        o1.flatMap(ff)
      }
    }
  }
}
