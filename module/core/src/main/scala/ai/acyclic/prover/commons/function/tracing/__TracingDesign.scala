package ai.acyclic.prover.commons.function.tracing

import ai.acyclic.prover.commons.debug.SrcDefinition
import ai.acyclic.prover.commons.function.hom.Hom

object __TracingDesign {

  object Trace {

    def var1[T](
        implicit
        srcDefinition: SrcDefinition
    ): Var[T] = Var[T](srcDefinition)

    def fromFn[I, O](
        fn: Hom.Fn[I, O]
    ): Expr[I, O] = Expr._1[I, O](fn)
  }

  /**
    * the primary use case is to use for-comprehension to construct a computation graph
    *
    * tracing is always best-effort: each function argument can be executed to get its internal, or at least part of it
    * (where it throws an exception and yield an incomplete tracing result)
    */

  object Examples {

    // produce x => x
    val id: Tracer[String, String] =
      for (x <- Trace.var1[String]) yield {
        x
      }

    // produce x => x + "1"
    val t1: Tracer[String, String] =
      for (x <- Trace.var1[String]) yield {
        x + "1"
      }

    // produce x => x + "1"
    val t1_2: Tracer[String, String] = {
      for {
        x <- Trace.var1[String]
        a = "1"
      } yield {
        x + a
      }
    }
    val t1_2_desugared: Tracer[String, String] = {
      val v1 = Trace
        .var1[String]
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
    val t1_chained: Tracer[String, String] =
      for (
        x <- Trace.var1[String];
        y = t1.apply(x)
      ) yield {
        val result: String = y + "2"
        result
      }
    // ditto
    val t1_chained_2: Tracer[String, String] =
      for (x <- Trace.var1[String]) yield {
        val y = t1.apply(x)
        val result: String = y + "2"
        result
      }

    // produce (x, y) => x + y
    val t2: Tracer[(Int, Int), Int] =
      for (
        x <- Trace.var1[Int];
        y <- Trace.var1[Int]
      ) yield {
        x + y
      }

    // produce x => x + ((t1(y))(y + "1")) + "2"
    // different from t1_chained/t1_chained_2, y is introduced as another variable
    val t2_chained: Tracer[(String, String), String] = {
      for (
        x: Var[String] <- Trace.var1[String];
        y: Var[String] <- t1
      ) yield {
        val result: String = x + y + "2"
        result
      }
    }
    // ditto
    val t2_chained_desugared: Tracer[(String, String), String] = {
      Trace.var1[String].flatMap { (x: Var[String]) =>
        t1.map { (y: Var[String]) =>
          val result: String = y + "2"
          result
        }
      }
    }

    // TODO: should NOT produce x => x + {y <-}((t1(x))(x + "1")) + "2"
    val t2_forbidden: Tracer[(String, String), String] = {
      for (
        x: Var[String] <- Trace.var1[String];
        y: Var[String] <- t1.apply(x) // t1(x) should results in something that forbid further for-comprehension
      ) yield {
        val result: String = x + y + "2"
        result
      }
    }

    // produce x => x + {y <-}([expr1](t1(x))(x + "1")) + "2"
    val t2_moreChained: Tracer[(String, String), String] = {
      for (
        x: Var[String] <- Trace.var1[String];
        expr1 = t1.apply(x);
        y: Var[String] <- expr1;
        result = x + y + "2"
      ) yield {
        result
      }
    }
    // ditto
    val t2_moreChained_desugared: Tracer[(String, String), String] = {
      val v1 = Trace
        .var1[String]
        .map { x: Var[String] =>
          val expr1 = t1.apply(x)
          (x, expr1)
        }

      v1
        .flatMap {
          case (_, expr1) =>
            val v1 = expr1.map { y =>
              val result = y + "2"
              (y, result)
            }

            val result = v1.map {
              case (_, result) =>
                result
            }

            result
        }
    }
  }
}
