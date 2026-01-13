package ai.acyclic.prover.commons.jit.tracing

import ai.acyclic.prover.commons.debug.SrcDefinition
import ai.acyclic.prover.commons.jit.hom.Hom.:=>

object __TracingDesign {

  object Trace {

    def id[T](
        implicit
        srcDefinition: SrcDefinition
    ): Id[T] = Id[T]()

    def fromFn[I, O](
        fn: I :=> O
    ) = Const(fn)
  }

  /**
    * the primary use case is to use for-comprehension to construct a computation graph
    *
    * tracing is always best-effort: each function argument can be executed to get its internal, or at least part of it
    * (where it throws an exception and yield an incomplete tracing result)
    */

  object Examples {

    // produce x => x
    val id: Constructor[String, String] =
      for (x <- Trace.id[String]) yield {
        x
      }

    // produce x => x + "1"
    val t1: Constructor[String, String] =
      for (x <- Trace.id[String]) yield {
        x + "1"
      }

    // produce x => x + "1"
    val t1_2: Constructor[String, String] = {
      for {
        x <- Trace.id[String]
        a = "1"
      } yield {
        x + a
      }
    }
    val t1_2_desugared: Constructor[String, String] = {
      val v1 = Trace
        .id[String]
        .map { x =>
          val a = "1"
          (x, a)
        }

      v1
        .map {
          case (x, a) =>
            x + a
        }
    }

    // produce x => [y](t1(x))(x + "1") + "2"
    val t1_chained: Constructor[String, String] =
      for (
        x <- Trace.id[String];
        y = t1.apply(x)
      ) yield {
        val result: String = y + "2"
        result
      }
    // ditto
    val t1_chained_2: Constructor[String, String] =
      for (x <- Trace.id[String]) yield {
        val y = t1.apply(x)
        val result: String = y + "2"
        result
      }

    // produce (x, y) => x + y
    val t2: Constructor[(Int, Int), Int] =
      for (
        x <- Trace.id[Int];
        y <- Trace.id[Int]
      ) yield {
        x + y
      }

    // produce x => x + ((t1(y))(y + "1")) + "2"
    // different from t1_chained/t1_chained_2, y is introduced as another variable
    val t2_chained: Constructor[(String, String), String] = {
      for (
        x: Var[String] <- Trace.id[String];
        y: Var[String] <- t1
      ) yield {
        val result: String = x + y + "2"
        result
      }
    }
    // ditto
    val t2_chained_desugared: Constructor[(String, String), String] = {
      Trace.id[String].flatMap { (_: Var[String]) =>
        t1.map { (y: Var[String]) =>
          val result: String = y + "2"
          result
        }
      }
    }

    // produce x => x + {y <-}([expr1](t1(x))(x + "1")) + "2"
    val t2_moreChained: Constructor[(String, String), String] = {
      for (
        x: Var[String] <- Trace.id[String];
        expr1: Constructor[String, String] = t1;
        y: Var[String] <- expr1;
        result = x + y + "2"
      ) yield {
        result
      }
    }
  }
}
