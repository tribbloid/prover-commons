package ai.acyclic.prover.commons.function.tracing

import ai.acyclic.prover.commons.debug.SrcDefinition
import ai.acyclic.prover.commons.function.hom.Hom

object __TracingDesign {

  object TraceDummy {

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

    // produce x => x + "1"
    val t1: Tracer[String, String] =
      for (x <- TraceDummy.var1[String]) yield {
        //        Tracer._get[String].apply(x) + "!"

        x + "1"
      }

    // produce (x, y) => x + y
    val t2: Tracer[(Int, Int), Int] =
      for (
        x <- TraceDummy.var1[Int];
        y <- TraceDummy.var1[Int]
      ) yield {
        x + y
      }

    // produce x => [t1](x + "1") + "2"
    val t31: Tracer[(String, Unit), String] = {
      for (
        x: Var[String] <- TraceDummy.var1[String];
        y: Var[String] <- t1.apply(x)
      ) yield {
        val result: String = y + "2"
        result
      }
    }

    {
      // TODO: convert t31 definition to dual form
    }

    // ditto
    val t32: Tracer[(String, Unit), String] = {
      for (
        x: Var[String] <- TraceDummy.var1[String];
        c1: Concrete[String] = t1.apply(x); // TODO: why is it broken
        y: Var[String] <- c1;
        result = y + "2"
      ) yield {
        result
      }
    }

    {
      // TODO: convert t32 definition to dual form
    }

    // produce x => [t1](x + "1") + "2"
    //    val t4: Tracer[(String, Unit), String] = {
    //      for (
    //        x <- Tracing.var1[String];
    //        y <- Tracing.var1[Int];
    //        c1 = t1.apply(x);
    //        z <- c1
    //      ) yield {
    //        y + "2"
    //      }
    //    }

  }
}
