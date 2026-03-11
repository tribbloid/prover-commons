package ai.acyclic.prover.commons.jit

import ai.acyclic.prover.commons.jit.__FusingRequirements.Expr.~>
import zio.Zippable

/**
  * contains several proposals of intermediate representation (IR) that breaks a forward execution into a graph of
  * subtasks.
  *
  * The 2 most promising proposals are:
  *
  *   1. rewrite into dependency graph directly (MeshCI)
  *   2. first rewrite into linear sequent (cut-elimination) combinators, then convert it into dependency graph. This
  *      can immediately support linear or affine type systems, but not as intuitive
  *
  * I like the sequent combinator, but converting from for-comprehension to it still takes macro.
  */
object __FusingRequirements {

  {
    // associativity of map
    val v1: Option[String] = ???

    val k0 =
      for (
        x <- v1;
        y <- Option(x + "1")
      ) yield x + y

    val k1 = v1
      .flatMap { x =>
        Option(x).zip(Option(x + "1"))
      }
      .map {
        case (x, y) => // more granular
          x + y
      }

    val k2 = v1.flatMap { x => // much shorter
      Option(x + "1")
        .map { y =>
          x + y
        }
    }
  }

  /**
    * this requirements only affects IR DSL, it may not be the primary DSL but it has to be readable
    *
    * the first observation is that agent/tool invocations should not be stored in Tape or Tracer directly. Imagine a
    * GoTo invocation that can be fused in a planning phase:
    */
  def decideNext(d: (Discovery, Int)): String = ???

  trait Discovery

  trait Expr[T] {

    import Expr.*

    type Pending

    def foreach(f: T => Unit): Pending ~> T = map(v => { f(v); v })

    // basic
    def map[R](f: T => R): Pending ~> R = ???
    def flatMap[R](f: T => Expr[R]): Pending ~> R = ???
    def zip[T2](t2: Pending ~> T2): Pending ~> (T, T2) = ???

    // maybe useful?
    def cutElimination[P2, R](f: ((T, P2)) => R)(
        implicit
        zip: Zippable[Pending, P2]
    ): zip.Out ~> R = ???
    def pointwise[I2, T2](t2: I2 ~> T2): (Pending, I2) ~> (T, T2) = ???

    def asFunction: (Pending => T) = ???
  }

  object Expr {
    infix type ~>[I, O] = Expr[O] { type Pending = I }
  }

  type Task[T] = Expr.~>[Unit, T]
  trait Const[T] extends Expr[T] { type Pending = Unit }

  class Row {

    val a: Const[String] = ???
    val b: Const[Int] = ???
  }

  object Proposal_DAGTracing {

    val r1 = new Row

    trait TracingPrimitive

    case class GoTo(target: Expr[String]) extends Expr[Discovery] {}
    object GoTo extends TracingPrimitive

    val url1 = r1.a
    val d1 = GoTo(url1)

    val url2 = d1.pointwise(r1.b).map(decideNext)
    val d2 = GoTo(url2)

    val result: Expr[(Discovery, Discovery)] = d1.pointwise(d2)

    result.foreach {
      case (x: Discovery, y: Discovery) =>
        println((x, y))
    }
  }

  /**
    * a crossover between DAGTracing & SequentCombinator
    *
    * like SequentCombinator it uses no tracer
    *
    * unlike SequentCombinator, it produces a non-linear computation graph
    */
  object Proposal_DAGBestEffort {

    val r1 = new Row

    case class GoTo(target: String) extends Expr[Discovery] {
      type Pending = Unit
    }

    implicit class TaskView[T](task: Task[T]) {

      def simplify: Task[T] = ???

      def mapAndSimplify[O](fn: T => O): Task[O] = task.map(fn).simplify

      def flatMapAndSimplify[O](fn: T => Expr[O]): Task[O] = task.map(fn).simplify.flatMap(identity)
    }

    val url1: Const[String] = r1.a
    val d1: Unit ~> Discovery = url1.flatMapAndSimplify(GoTo.apply)

    {
      // equivalent to:
      url1
        .map(GoTo.apply)
        .simplify // <-- best effort simplify will succeed, yielding an Const[GoTo] that can be fused
        .flatMap(identity)
    }

    val url2: Task[String] = d1
      .zip(r1.b: Const[Int])
      .mapAndSimplify(
        decideNext
      ) // <-- best effor simplify will fail (d1 is just a placeholder in dry-run), fallback to itself
    // yielding a computation graph with blackbox primitives, but depends on GoTo(r1.a) and r1.b
    // this is already the best effort in fusing, no need to look into the blackbox primitives

    /**
      * CAUTION: the above `zip` operator is formalised as "Applicative" in most libraries
      *
      * plain for-comprehension won't use it, but some advanced compiler extensions (e.g. Haskell ApplicativeDo, Scala3
      * AvocADO) can optimise into this as an IR
      */

    val d2: Task[Discovery] = url2.flatMapAndSimplify(GoTo.apply) // <-- ditto

    val result: Unit ~> (Discovery, Discovery) = d1.zip(d2)

    result.foreach {
      case (x: Discovery, y: Discovery) =>
        println((x, y))
    }

    /**
      * lesson learned here:
      *   - do not optimise IR for for-comprehension! which is hardly optimised, ambiguous and constantly changing.
      *     Optimise for monad/applicative API instead.
      *   - IR should be explicit, human-readable and rely on minimal syntax sugars, it can be verbose to write, but AI
      *     won't care if it type safe.
      *     - henceforth, tracers like dual numbers should not be part of IR.
      *   - operator fusion can work on blackbox primitives, unlike autodiff
      */

    object ShorterVersion {
      case class AlwaysSimplify[T](
          self: Task[T]
      ) extends Expr[T] {

        override type Pending = self.Pending

        override def map[R](f: T => R): Pending ~> R = {
          copy(self.mapAndSimplify(f))
        }

        override def flatMap[R](f: T => Expr[R]): Pending ~> R = {
          copy(self.flatMapAndSimplify(f))
        }
      }

      val d1: Unit ~> Discovery = AlwaysSimplify(r1.a)
        .flatMap(GoTo.apply)

      val d2: Unit ~> Discovery = d1
        .zip(r1.b)
        .map {
          decideNext
        }
        .flatMap(GoTo.apply)

      val result: Unit ~> (Discovery, Discovery) = {
        d1.zip(d2) // <-- d1 is used twice, this makes it fundamentally incompatible with sequent for-comprehension
        // TODO: this actually causes execution of `result` to be underdefined, as the result d1 can be cached or not
      }

      result.foreach {
        case (x: Discovery, y: Discovery) =>
          println((x, y))
      }
    }
  }

  object Proposal_SequentCombinators {

    val r1 = new Row

    case class GoTo(target: String) extends Expr[Discovery] {
      override type Pending = Unit
    }
    object GoTo extends (String => Expr[Discovery]) with Expr[String => Discovery] {}

    implicit class fromFunction[I, O](fn: I => O) extends Expr[O] {

      type Pending = I
    }

    def id[I]: fromFunction[I, I] = fromFunction(identity[I])

    case class keepInput[I]() {

      def apply[O](fn: I => O): I => (I, O) = ???
    }

    val result: Expr[(Discovery, Discovery)] = {

      val raw: Unit ~> (Discovery, Discovery) =
        for (
          a <- r1.a;
          d1 <- GoTo(a);
          b <- r1.b;
          next = decideNext(d1, b);
          d2 <- GoTo(next)
        ) yield {
          d1 -> d2
        }

      val desugared_scala3_7: Unit ~> (Discovery, Discovery) = r1.a.flatMap { a => // this is irrelevant at the moment
        GoTo(a).flatMap { d1 =>
          r1.b
            .flatMap { b =>
              {
                val next = decideNext(d1, b) // less granular

                GoTo(next).map { d2 =>
                  d1 -> d2
                }
              }
            }
        }
      }

      val desugared: Unit ~> (Discovery, Discovery) = r1.a.flatMap { a: String =>
        GoTo(a).flatMap { d1 =>
          r1.b
            .map { b =>
              b -> decideNext(d1, b)
            }
            .flatMap {
              case (_, next) =>
                GoTo(next).map { d2 =>
                  d1 -> d2
                }
            }
        }
      }

      val primitiveCaptured: Unit ~> (Discovery, Discovery) = r1.a.flatMap {
        val header =
          for (
            x <- id[String];
            d1 <- GoTo(x)
          ) yield d1

        val continuation: Discovery => Task[(Discovery, Discovery)] = {
          val header: Unit ~> Int = for (b <- r1.b) yield b

          val combined1: Discovery ~> ((Int, Discovery), String) = header.cutElimination(
            keepInput[(Int, Discovery)]().apply {
              case (b, d1) =>
                decideNext(d1, b)
            }
          )

          val continuation2: (((Int, Discovery), String)) => Task[(Discovery, Discovery)] = {

            val header =
              for (
                x <- id[String];
                d2 <- GoTo(x)
              ) yield d2

//            header.cutElimination(
//
//              for (x <- )
//            )

            {
              case ((_, d1), next) => // TODO: need to rewrite this part
                val result: Unit ~> (Discovery, Discovery) = GoTo(next).map { d2 =>
                  d1 -> d2
                }

                result
            }
          }

          val result: Discovery ~> Task[(Discovery, Discovery)] = combined1.map(continuation2)

          result.asFunction
        }

        val ff = header.map(continuation)

        ff.asFunction
      }

      primitiveCaptured
    }
  }
}
