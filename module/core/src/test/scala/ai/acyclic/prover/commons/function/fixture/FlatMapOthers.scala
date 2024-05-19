package ai.acyclic.prover.commons.function.fixture

import ai.acyclic.prover.commons.function.Hom
import ai.acyclic.prover.commons.function.Hom.:=>
import ai.acyclic.prover.commons.function.hom.HomSystem

trait FlatMapOthers extends Fns {

  { // 1-cat
    val case1: Seq[Long] :=> Seq[Double] = {
      Hom
        .at[Long :=> Seq[Double]] { ff =>
          Hom.at[Seq[Long]] { o1 =>
            o1.flatMap(ff)
          }
        }
        .apply(fn2)
    }

    val case2: Seq[Long] :=> Seq[Double] = {
      val result =
        for (ff <- fn2) yield {

          Hom.at[Seq[Long]] { v =>
            v.flatMap(ff)
          }
        }

      result
    }
  }

  { // 2-cat
    val case1: Int :=> Seq[Double] = {
      fn1.andThen {

        Hom
          .at[Long :=> Seq[Double]] { _fn2 =>
            Hom.at[Seq[Long]] { o1 =>
              o1.flatMap(_fn2)
            }
          }
          .apply(fn2)
      }
    }

    val case2: Int :=> Seq[Double] = {

      fn1.andThen {

        val result = for (ff <- fn2) yield {

          Hom.at[Seq[Long]] { v =>
            v.flatMap(ff)
          }
        }

        result
      }
    }

    val case3 = {

      for (
        o1 <- fn1.out;
        ff <- fn2
      ) yield {

        o1.flatMap(ff)

      }
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

//    { // ideally
//      val result: HomSystem.FnImpl[fn1.In, Seq[Double]] =
//        for (
//          seq <- fn1;
//          _fn2 <- fn2.^
//          // fn2 has to exist here, or it will be hidden behind function definition
//        ) yield {
//
//          val result = seq.flatMap(_fn2)
//          result
//        }
//
//      result // signature looks good
//    }
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
