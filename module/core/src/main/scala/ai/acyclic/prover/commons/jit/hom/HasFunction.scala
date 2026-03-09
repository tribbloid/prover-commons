package ai.acyclic.prover.commons.jit.hom

import ai.acyclic.prover.commons.collection.CacheMagnet
import ai.acyclic.prover.commons.jit.Hom
import ai.acyclic.prover.commons.jit.{CanSimplify, Domains, FnBuilder, Rule}
import ai.acyclic.prover.commons.multiverse.CanEqual
import ai.acyclic.prover.commons.debug.SrcDefinition
import ai.acyclic.prover.commons.jit.eval.{Args, PartialEvalEnv}
import Args.{><:, T0}
import ai.acyclic.prover.commons.TypeTag

import scala.language.implicitConversions

object HasFunction {

  implicit def asFn(v: HasFunction): v.Fn.type = v.Fn
}

trait HasFunction {
  self: Hom.type =>

  trait DepFn[-I <: Args] extends CanSimplify[DepFn[I]] {
    type In >: I <: Args

    override def partialEval(env: () => PartialEvalEnv[In]): DepFn[I] = this
  }
  case object DepFn {

    type constraint <: Any
  }

  trait Fn[-I <: Args, +O] extends CanSimplify[Fn[I, O]] with DepFn[I] with Domains {

    type Out <: O

    override def partialEval(env: () => PartialEvalEnv[In]): Fn[I, O] = this

    // TODO: this should be a special case of specialise/partial-eval
  }
  case object Fn extends FnBuilder.Root {
    type Fn1[-I, +O] = Fn[I ><: T0, O]
    type Fn2[-I, -J, +O] = Fn[I ><: J ><: T0, O]

    /**
      * function with computation graph, like a lifted JAXpr
      */
//    type K[-I <: Args, +O] = DepFn.K[I] { type OutK[T] <: O }

    // sanity check - disabled because scalafix/semanticdb cannot parse bare blocks
    // implicitly[Fn[Int, String] <:< K2[Int, String]]

    val Tracing: ai.acyclic.prover.commons.jit.cps.Continuation.type =
      ai.acyclic.prover.commons.jit.cps.Continuation

    abstract class Impl[I <: Args, O](
        implicit
        override val _definedAt: SrcDefinition
    ) extends Fn[I, O] { // most specific

      type In = I
      type Out = O
    }

    type Impl0[O] = Impl[T0, O]
    type Impl1[I, O] = Impl[I ><: T0, O]
    type Impl2[I1, I2, O] = Impl[I1 ><: I2 ><: T0, O]

//    sealed trait Compositor {} // TODO: this needs to be a supertype of all Impl composed from multiple functions

    def id[I]: Identity[I] = Identity[I]()

    case class Mapped[I <: Args, M, O](
        left: Fn[I, M],
        right: Fn[M ><: T0, O]
    ) extends Impl[I, O] {

      override lazy val noInput: I = left.noInput.asInstanceOf[I]

      override type Rules = left.Rules & right.Rules

      override def apply(arg: I): O =
        right.apply(Const.Provided(left(arg)) ><: Args.eye)

      override def partialEval(env: () => PartialEvalEnv[In]): Fn[I, O] = {
        val simplifiedLeft = left.partialEval(env)
        val simplifiedRight = right.simplify

        copy(
          left = simplifiedLeft,
          right = simplifiedRight
        )
      }
    }

    case class Flatten[I <: Args, T, O](
        base: Fn[I, T],
        coerce: T => Fn[I, O]
    ) extends Impl[I, O] {

      override lazy val noInput: I = base.noInput.asInstanceOf[I]

      override def apply(arg: I): O = {
        coerce(base(arg)).apply(arg)
      }

      override def partialEval(env: () => PartialEvalEnv[In]): Fn[I, O] = {
        copy(base = base.partialEval(env))
      }

    }

    case class Flipped[I1, I2, O](
        base: Fn2[I1, I2, O]
    ) extends Impl2[I2, I1, O] {

      override type In = I2 ><: I1 ><: T0
      override lazy val noInput: In = Args.NoInput.T2

      override type Rules = base.Rules

      override def apply(arg: I2 ><: I1 ><: T0): O = {

        val (i2: Args.Element[I2], t1) = Args.deCons(arg)
        val (i1: Args.Element[I1], _) = Args.deCons(t1)
        val t2: I2 ><: T0 = i2 ><: T0
        val swapped: I1 ><: I2 ><: T0 = i1 ><: t2
        base.apply(swapped)
      }
    }

    case class Fork[I <: Args, O1, O2](
        left: Fn[I, O1],
        right: Fn[I, O2]
    ) extends Impl[I, (O1, O2)] {

      override lazy val noInput: I = left.noInput.asInstanceOf[I]

      override type Rules = left.Rules & right.Rules

      override def apply(arg: I): (O1, O2) = {
        left(arg) -> right(arg)
      }

      override def partialEval(env: () => PartialEvalEnv[In]): Fn[I, (O1, O2)] = {
        copy(
          left = left.partialEval(env),
          right = right.partialEval(env)
        )
      }
    }

    object Duplicate {

      def apply[I]() = Fork(Identity[I](), Identity[I]())
    }

    case class Zipped[I <: Args, O, I2 <: Args, O2, Z <: Args](
        left: Fn[I, O],
        right: Fn[I2, O2]
    )(
        unzip: Args.Zippable.Aux[I, I2, Z]
    ) extends Impl[Z, (O, O2)] {

      override type In = Z
      override lazy val noInput = ???

      override type Rules = left.Rules & right.Rules

      override def apply(arg: Z): (O, O2) = {
        val (leftArg, rightArg) = unzip.unzip(arg)
        left(leftArg) -> right(rightArg)
      }
    }

    // typed helpers for CPS/tracing composition to keep structure stable in explain trees
    def provided0[O](value: O): Fn[T0, O] = {
      Const.Provided(value)
    }

    def zip[I <: Args, O, I2 <: Args, O2, Z <: Args](
        left: Fn[I, O],
        right: Fn[I2, O2]
    )(
        implicit
        unzip: Args.Zippable.Aux[I, I2, Z]
    ): Fn[Z, (O, O2)] = {

      Zipped(left, right)(unzip)
    }

    def fork[I <: Args, O, O2](
        left: Fn[I, O],
        right: Fn[I, O2]
    ): Fn[I, (O, O2)] = {

      Fork[I, O, O2](left, right)
    }

    // TODO: fix this, old type signature is wrong
    case class Identity[I]() extends Impl1[I, I] {

      override type In = I ><: T0
      override lazy val noInput: In = Args.NoInput.T1

      override type Rules <: Rule.Linear

      def apply(arg: I ><: T0): I = arg.head.compute
    }

    case class Conditional[I](
        filter: Fn.Impl1[I, Boolean]
    ) extends Impl1[I, I] {

      override lazy val noInput: In = filter.noInput

      override def apply(o: I ><: T0): I = {
        val v = o.head.compute
        val passes: Boolean = filter(o) // filter returns Boolean directly
        if (passes) v
        else throw new MatchError(s"condition ${_definedAt} is not applicable on $v")
      }
    }

    trait HasLambdaInfo[F] {
      def fn: F

      /**
        * returns:
        *   - Some(lambdaInfo) if [[fn]] is constructed from a lambda definition (like {x: Int => x + 1})
        *   - None otherwise
        */
      lazy val lambdaInfo: Option[LambdaInfo] = {

        val name = fn.getClass.getName
        val isLambda = name.contains("$$Lambda") || name.contains("$$anonfun")

        if (isLambda) {

          /**
            * Scala compiler automatically convert a lambda definition (like {x: Int => x + 1}) into a Java
            * function/closure, with each free variable assigned as an ad-hoc object member, named "arg$1", "arg$2" etc.
            *
            * This is only the rule in JVM, other runtimes may have different rules
            */
          val freeVariables: Seq[Any] = {
            val fields = fn.getClass.getDeclaredFields
            val result = fields.toSeq
              .filterNot(v => java.lang.reflect.Modifier.isStatic(v.getModifiers))
              .map { field =>
                //            val name = field.getName
                field.setAccessible(true)
                field.get(fn)
              }
            result
          }

          Some(LambdaInfo(freeVariables))
        } else None
      }
    }

    case class Blackbox[I, R](
        final override val _definedAt: SrcDefinition
    )(val fn: I => R)
        extends Impl1[I, R]
        with HasLambdaInfo[I => R] {

      override type In = I ><: T0
      override lazy val noInput: In = Args.NoInput.T1

      override def apply(arg: I ><: T0): R = {

        fn(arg.head.compute)
      }
    }

//    case class BlackboxArgs[I <: Args, R](
//        final override val _definedAt: SrcDefinition
//    )(val fn: I => R)
//        extends Impl[I, R]
//        with HasLambdaInfo[I => R] {
//
//      override def apply(arg: I): R = {
//        fn(arg)
//      }
//    }

    implicit def _as1View[I, O](v: CanSimplify[Fn[I ><: T0, O]])(
        implicit
        _definedAt: SrcDefinition
    ): Function1View[I, O] = {
      Function1View(v.simplify, _definedAt)
    }

    implicit def _as0View[O](v: CanSimplify[Thunk[O]])(
        implicit
        _definedAt: SrcDefinition
    ): Function0View[O] = {
      Function0View(v.simplify, _definedAt)
    }

    implicit def fromFunction1[I, R](fn: I => R)(
        implicit
        _definedAt: SrcDefinition
    ): Fn[I ><: T0, R] = {
      fn match {
        case vv: Function1View[I @unchecked, R @unchecked] =>
          vv.self match {
            case impl: Fn[I ><: T0, R @unchecked] =>
              impl
          }
        case _ =>
          Blackbox[I, R](_definedAt)(fn)
      }
    }

    implicit def fromFunction0[R](fn: () => R)(
        implicit
        _definedAt: SrcDefinition
    ): Const.Impl[R] = {

      case class ThunkImpl()(
          implicit
          override val _definedAt: SrcDefinition
      ) extends Impl0[R] {
        override type In = T0
        override lazy val noInput: In = Args.Eye

        override def apply(arg: T0): R = fn()
      }
      Const.Lazy(ThunkImpl())
    }

    trait Pure {}

    object Pure {

      case class Is[I <: Args, R](delegate: Fn[I, R])(
          implicit
          inputSchema0: I
      ) extends Impl[I, R]
          with Pure {

        override lazy val noInput: I = delegate.noInput.asInstanceOf[I]

        override def apply(v: I): R = delegate.apply(v)
      }
    }

    trait CachedPure extends Pure

    // TODO: make a dependent class, also in Thunk
    final case class CachedImpl[I <: Args, R](backbone: Fn[I, R])(
        getLookup: () => CacheMagnet[I, R] = () => CanEqual.Native.Lookup[I, R]()
    ) extends Impl[I, R]
        with CachedPure {

      override lazy val noInput: I = backbone.noInput.asInstanceOf[I]

      lazy val lookup: CacheMagnet[I, R] = getLookup()

      def apply(key: I): R = {
        lookup
          .getOrElseUpdateOnce(key) {

            val value = backbone(key)
            value
          }
      }

      def getExisting(arg: I): Option[R] = {
        lookup
          .get(arg)
      }
    }

    override protected type BuildTarget[I, O] = Fn[I ><: T0, O]

    protected def build[I, O](fn: I => O)(
        implicit
        _definedAt: SrcDefinition
    ): BuildTarget[I, O] = {

      Fn.fromFunction1(fn)(_definedAt)
    }

    case class DomainBuilder[I, O]() extends IDomainBuilder[I, O] {

      type _Lemma = Fn[I ><: T0, O]
      type _Impl = Fn.Impl[I ><: T0, O]
      type _Native = (I => O)

      def fn[o <: O](fn: I => o)(
          implicit
          _definedAt: SrcDefinition
      ): BuildTarget[I, o] = {
        apply(fn)(_definedAt)
      }

      def raw[o <: O](fn: I => o): I => o = fn
    }
    def domainBuilder[i, o]: DomainBuilder[i, o] = DomainBuilder()
  }

  case class Function1View[I, O] private[hom] (
      self: Fn[I ><: T0, O],
      otherFnDefinedAt: SrcDefinition
  ) extends Function[I, O] {

    def function1: Function1View[I, O] = this

    final override def apply(v: I): O = self(Const.Provided(v) ><: Args.eye)

    // TODO: both of these are not narrow enough
    final override def andThen[O2](next: O => O2): Function1View[I, O2] = {

      val _next: Fn[O ><: T0, O2] = Fn.at[O](next)(otherFnDefinedAt)

      val result =
        Fn.Mapped[I ><: T0, O, O2](self, _next)

      Function1View(result, otherFnDefinedAt)
    }

    final override def compose[I1](prev: I1 => I): Function1View[I1, O] = {

      val _prev = Fn.at[I1](prev)(otherFnDefinedAt)

      Function1View(_prev, otherFnDefinedAt).andThen(self)
    }
  }

  case class Function0View[O] private[hom] (
      self: Thunk[O],
      _definedAt: SrcDefinition
  ) extends Function0[O] {

    def function0: Function0View[O] = this

    final override def apply(): O = self(Args.eye)

    //      override def normalise: Circuit[I, O] = self.normalise

    def asLazy: Const.Lazy[O] = Const.Lazy(self)

    def asEager: Const.Provided[Thunk[O]] = Const.Provided(self)
  }

  sealed trait ConstantFn[+O] extends Fn[Args, O] with Fn.CachedPure {

    val compute: O // should mostly be a lazy val
  }

  type Thunk[+O] = Fn[T0, O]
  type Const[+O] = Thunk[O] & ConstantFn[O]

  object Const {

    // sanity (TODO: this may threaten behaviour of ZIO zippable or unzippable, need some test cases)
    implicitly[Impl[Int] <:< Fn[T0, Int]] // this principle should be held at all cost
    implicitly[Const.Impl[Int] <:< Const[Int]]
    implicitly[Const[Int] <:< ConstantFn[Int]]

    implicitly[Args.Eye =:= Args.T0]

    sealed trait Impl[O] extends Fn.Impl[Args, O] with ConstantFn[O] {
      override type In = Args
      override lazy val noInput = ???

      override def apply(arg: Args): O = compute
    }

    final case class Provided[O](compute: O) extends Impl[O] {
      canEqualProjections += CanEqual.Native.on(compute)
    }

    final case class Lazy[O](gen: Thunk[O]) extends Impl[O] {

      // equivalent to CachedLazy[Unit, O], but much faster
      @transient lazy val compute: O = gen(Args.eye)
    }

    case object NotProvided extends Impl[Nothing] {

      @transient lazy val compute: Nothing = throw new NoSuchElementException("missing, not provided")
    }
  }

  implicit class _fnExt[I <: Args, O](self: Fn[I, O]) extends Serializable {

    def trace(
        implicit
        iTag: TypeTag[I],
        oTag: TypeTag[O]
    ) = {
      ai.acyclic.prover.commons.jit.cps.Continuation(self)
    }

    def cached(byLookup: => CacheMagnet[I, O]): Fn.CachedImpl[I, O] = {
      Fn.CachedImpl[I, O](self)(() => byLookup)
    }

    def cached(): Fn.CachedImpl[I, O] = {
      Fn.CachedImpl[I, O](self)()
    }
  }

  implicit class _thunkExt[O](self: Thunk[O]) extends Serializable {

    def trace(
        implicit
        oTag: TypeTag[O]
    ) = {
      ai.acyclic.prover.commons.jit.cps.Continuation(self)
    }
  }
}
