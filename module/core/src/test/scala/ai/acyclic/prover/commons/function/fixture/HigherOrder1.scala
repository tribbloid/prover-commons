package ai.acyclic.prover.commons.function.fixture

import ai.acyclic.prover.commons.function.Hom.:=>

object HigherOrder1 {

  import Fns._

  { // 1-cat
    {
      :=>.at[Long :=> Seq[Double]] { ff =>
        :=>.at[Seq[Long]] { o1 =>
          o1.flatMap(ff)
        }
      }
        .apply(fn2)
    }

    {
      for (ff <- fn2.^) yield {

        :=>.at[Seq[Long]] { v =>
          v.flatMap(ff)
        }
      }
    }
  }

  { // currying

    for (
      o1 <- fn1.out;
      o2 <- fn2.out
    ) yield {

      o1.zip(o2)
    }
  }

  lazy val flatMapOthers = {

//    val fz: (Long :=> Seq[Double]) :=> (Int :=> Seq[Double]) =
//      Hom.at[Long :=> Seq[Double]].apply { inner =>
//        val result =
//          for (seq <- fn1) yield {
//
//            seq.flatMap {
//              //          HomSystem.FnOps[Int, Seq[Double]](inner)
//              inner
//            }
//          }
//        result
//      }
//
//    val result = fz.^.apply(fn2.^) // should yield FnImpl directly
//    result

    // ideally
    val result: Int :=> Seq[Any] =
      for (
        seq <- fn1.out;
        _fn2 <- fn2.^
        // fn2 has to exist here, or it will be hidden behind function definition
      ) yield {

        val result = seq.flatMap(_fn2)
        result
      }

    result // signature looks good
  }

  // ideally
//
//  val result = {
//
//    fn1.andThen {
//
//      Hom.at[Seq[Int]] { v1 => }
//    }
//
//    for (seq <- fn1) yield {
//
//      fn2.^(seq)
//      seq.flatMap(fn2)
//    }
//  }

}
