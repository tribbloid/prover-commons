package ai.acyclic.prover.commons.jit.tracing

object __TracingRequirements {

  /**
    * # Fusing & AutoGrad
    *
    * can compile a plain function (direct style) into 2-stages: an optimisation stage and an execution stage.
    *
    * Compilation is always best-effort, in particular, DO NOT assume that:
    *   - meta-rewriter is always stable, only runtime-AOT tracing is usable
    *   - every elementary function has a diff/grad/tangent form, they can be:
    *     - not IID (independent & identically distributed)
    *     - IID but not pure/deterministic
    *     - pure but not continuous
    *     - continuous but not analytical, only numerical diff approximation is usable
    */

  type F32 = Float

  val f1: F32 => F32 = ???
  val f2: (F32, F32) => F32 = ???

  type UniVarFunction = F32 => F32

  type Vec2K[T] = (T, T)
  type Vec3K[T] = (T, T, T)
  type Vec3 = Vec3K[F32]

  type Vec3Function = Vec3 => F32

  val fn: Vec3Function = { (v: Vec3) =>
    val (x, y, z) = v
    f1(x) + f2(y, z) + 1
  }

  object FusingObjective { // <-- easy

    trait Plain {

      fn(1, 2, 3)
    }

    trait Compiled {

      val input = Some((1f, 2f, 3f))

      val stage1: Option[(F32, F32)] = input.map {
        case (x, y, z) =>
          val x1 = f1(x)
          val x2 = f2(y, z)

          (x1, x2) // <-- fuse this data if f1 & f2 are expensive and results can be reused
      }

      val stage2: Option[F32] = stage1.map { v: (F32, F32) => // <-- fuse this function if runnng on SIMD hardware
        val (x1, x2) = v
        x1 + x2 + 1
      }

      stage2.get
    }
  }

  object AutoGradObjective_GetTangentForm {

    /**
      * A linear function/line/plane/hyperplane and a point on it, approximating the original function.
      *
      * Alternatively, this function can be uniquely derived from input/output and:
      *   - a total diff (if output is a number) or a Jacobian (if output is a vector), to be accumulated in any order
      *     (usually backward)
      *
      * AKA exterior diff/1-form/1st-order Taylor series, presumably easier to reason with than pure Jacobian
      */
    infix trait TangentApproximator[R, T] {

      val linearFn: Function1[R, T]
      val in: R // <-- actually useless in autograd
      lazy val out: T = linearFn(in) // <-- useful in forward inference
    }

    /** given a function and a point, return an [[TangentApproximator]] near that point * */
    type GetTangentElementary = UniVarFunction => F32 => TangentApproximator[F32, F32]
    val getTangentElementary: GetTangentElementary = ???

    type GetTangent = Vec3Function => Vec3 => TangentApproximator[Vec3, F32]
    val getTangent: GetTangent = { throw new UnsupportedOperationException("WTF is this?") }

    trait Plain {

      val linear: ((F32, F32, F32)) => TangentApproximator[Vec3, F32] = getTangent(fn)
      linear(1, 2, 3)
    }

    trait Compiled extends FusingObjective.Compiled {

      val stage1Tangent: Option[Vec2K[TangentApproximator[Vec3, F32]]] = input.map {
        case (x, y, z) =>

          val f1Tangent = {
            val f1d = getTangentElementary(f1)(x)

            new TangentApproximator[Vec3, F32] {

              override val linearFn: Vec3 => F32 = { (v: Vec3) =>
                val (x, y, z) = v
                f1d.linearFn(x)
              }
              override val in: (F32, F32, F32) = (x, y, z)
            }
          }

          val f2Tangent = {
            val f2dy = getTangentElementary((t: F32) => f2(t, z))(y)
            val f2dz = getTangentElementary((t: F32) => f2(y, t))(z)

            TangentApproximator { (v: Vec3) =>
              {
                val (x, y, z) = v
                f2dy(y) + f2dz(z)
              }
            }
          }

          (f1Tangent, f2Tangent)
      }

      val stage2Tangent: Option[((F32, F32, F32)) => F32] = stage1Tangent.map {
        case (t1, t2) =>

          TangentApproximator { (v: Vec3) =>
            t1(v) + t2(v) + 1 // <-- already linear, no need to getTangentElementary again
            // otherwise, use the output of
          }
      }

      stage2Tangent.get
    }
  }
}
