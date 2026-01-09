package ai.acyclic.prover.commons.function.tracing

import ai.acyclic.prover.commons.debug.SrcDefinition
import ai.acyclic.prover.commons.function.hom.Hom
import scala.language.implicitConversions

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
      Trace
        .var1[String]
        .map { x =>
          val a = "1"
          (x, a)
        }
        .map {
          case (x, a) =>
            x + a
        }
    }

    // produce x => [t1](x + "1") + "2"
    val t1_chained: Tracer[String, String] =
      for (x <- Trace.var1[String]) yield {
        val y = t1.apply(x)
        val result: String = y + "2"
        result
      }

    // ditto
    val t1_chained_2: Tracer[String, String] =
      for (
        x <- Trace.var1[String];
        y = t1.apply(x)
      ) yield {
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

    // produce x => [t1](y + "1") + "2"
    // different from t1_chained/t1_chained_2, y is introduced as another variable
    val t2_chained: Tracer[(String, String), String] = {
      for (
        x <- Trace.var1[String];
        y <- t1.apply(x)
      ) yield {
        val result: String = y + "2"
        result
      }
    }

    val t2_moreChained: Tracer[String, String] = {
      for (
        x: Var[String] <- Trace.var1[String];
        c1: Tracer[String, String] = t1.apply(x); // TODO: why is it broken
        y <- c1;
        result = y + "2"
      ) yield {
        result
      }
    }

  }
}
