package ai.acyclic.prover.commons.jit.hom

import ai.acyclic.prover.commons.collection.CacheMagnet
import ai.acyclic.prover.commons.jit.{CanSimplify, FnBuilder, IntermediateRepresentation, Rule}
import ai.acyclic.prover.commons.jit.Domains
import ai.acyclic.prover.commons.multiverse.CanEqual
import ai.acyclic.prover.commons.debug.SrcDefinition
import ai.acyclic.prover.commons.jit.eval.Args
import Args.{><:, T0}

import scala.language.implicitConversions

object HasFunction {

  implicit def asFn(v: HasFunction): v.Fn.type = v.Fn
}

trait HasFunction {

  trait DepFn[-I <: Args.Prod] extends IntermediateRepresentation with CanSimplify[DepFn[I]] {
    type In >: I <: Args.Prod
  }
  // TODO: should be K1[I] (as refined type), but scala 2 implicit search is too weak fo this
  case object DepFn {

    type K1[-I <: Args.Prod] = IntermediateRepresentation { type In >: I }

    type constraint <: Any

    { // sanity
      implicitly[DepFn[Int ><: T0] <:< K1[Int ><: T0]]
    }
  }

  trait Fn[-I <: Args.Prod, +O] extends CanSimplify[Fn[I, O]] with DepFn[I] with Domains {

    type Out <: O

    override def simplify: Fn[I, O] = this // bypassing EqSat, always leads to better representation
    // TODO: this should be a special case of specialise/partial-eval
  }
  // TODO: should be K2[I, R] (as refined type), but scala 2 implicit search is too weak fo this
  case object Fn extends FnBuilder.Root {

    implicit class _extFn[I <: Args.Prod, O](
        self: Fn[I, O]
    ) extends Serializable {

      def cached(byLookup: => CacheMagnet[I, O]): Fn.CachedImpl[I, O] = {
        Fn.CachedImpl[I, O](self)(() => byLookup)
      }

      def cached(): Fn.CachedImpl[I, O] = {
        Fn.CachedImpl[I, O](self)()
      }
    }

    /**
      * function with computation graph, like a lifted JAXpr
      */
    type K2[-I <: Args.Prod, +O] = DepFn.K1[I] { type OutK[T] <: O }

    // sanity check - disabled because scalafix/semanticdb cannot parse bare blocks
    // implicitly[Fn[Int, String] <:< K2[Int, String]]

    val Tracing: ai.acyclic.prover.commons.jit.cps.Continuation.type =
      ai.acyclic.prover.commons.jit.cps.Continuation

    abstract class Impl[I <: Args.Prod, O](
        implicit
        override val _definedAt: SrcDefinition
    ) extends Fn[I, O] { // most specific

      type In = I
      type Out = O
    }

//    sealed trait Compositor {} // TODO: this needs to be a supertype of all Impl composed from multiple functions

    def id[I <: Args.Prod]: Identity[I] = Identity[I]()

    case class Mapped[I <: Args.Prod, M, O](
        left: Fn[I, M],
        right: Fn[M ><: T0, O]
    ) extends Impl[I, O] {

      override type Rules = left.Rules & right.Rules

      override def apply(arg: I): O =
        right.apply(Args.cons(Const.Provided(left(arg)).asInstanceOf[Hom.ConstantFn[M]], Args.eye))

      override def simplify: Fn[I, O] = {
        (left: Any, right: Any) match {
          case (ll: Fn[_, _], rr: Fn[_, _]) => Mapped(ll.asInstanceOf[Fn[I, M]], rr.asInstanceOf[Fn[M ><: T0, O]])
        }
      }
    }

    case class Flatten[I <: Args.Prod, T, O](
        base: Fn[I, T],
        coerce: T => Fn[I, O]
    ) extends Impl[I, O] {

      override def apply(arg: I): O = {

        coerce(base(arg).asInstanceOf[T]).apply(arg)
      }

      override def simplify: Fn[I, O] = {
        copy(base.simplify)
      }

    }

    case class Flipped[I1, I2, O](
        base: Fn[I1 ><: I2 ><: T0, O]
    ) extends Impl[I2 ><: I1 ><: T0, O] {

      override type Rules = base.Rules

      override def apply(arg: I2 ><: I1 ><: T0): O = {

        val (i2, t1) = Args.deCons(arg)
        val (i1, _) = Args.deCons(t1)
        base.apply(Args.cons(i1, Args.cons(i2, Args.eye))).asInstanceOf[O]
      }
    }

    case class Pointwise[I1 <: Args.Prod, O1, I2 <: Args.Prod, O2](
        left: Fn[I1, O1],
        right: Fn[I2, O2]
    ) extends Impl[(I1, I2) ><: T0, (O1, O2)] {

      override type Rules = left.Rules & right.Rules

      override def apply(arg: (I1, I2) ><: T0): (O1, O2) = {

        val (i1, i2) = arg.head.compute

        val lo = left(i1).asInstanceOf[O1]
        val ro = right(i2).asInstanceOf[O2]

        (lo -> ro).asInstanceOf[(O1, O2)]
      }
    }

    case class Duplicate[I]() extends Impl[I ><: T0, (I, I)] {

      override def apply(arg: I ><: T0): (I, I) = {
        val v = arg.head.compute
        v -> v
      }
    }

    case class DuplicateArgs[I <: Args.Prod]() extends Impl[I, (I, I)] {
      override def apply(arg: I): (I, I) = arg -> arg
    }

    case class Identity[I <: Args.Prod]() extends Impl[I, I] { // TOOD: this should be contravariant under DepFn

      override type Rules <: Rule.Linear

      override def apply(arg: I): I & OutK[arg.type] = arg.asInstanceOf[I & OutK[arg.type]]
    }

    case class Conditional[I](
        filter: Fn[I ><: T0, Boolean]
    ) extends Impl[I ><: T0, I] {

      override def apply(o: I ><: T0): I = {
        val v = o.head.compute
        if (filter(o).asInstanceOf[Boolean]) v
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
        extends Impl[I ><: T0, R]
        with HasLambdaInfo[I => R] {

      override def apply(arg: I ><: T0): R = {

        fn(arg.head.compute)
      }
    }

    case class BlackboxArgs[I <: Args.Prod, R](
        final override val _definedAt: SrcDefinition
    )(val fn: I => R)
        extends Impl[I, R]
        with HasLambdaInfo[I => R] {

      override def apply(arg: I): R = {
        fn(arg)
      }
    }

    implicit def _as1View[I, O](v: CanSimplify[Fn[I ><: T0, O]])(
        implicit
        _definedAt: SrcDefinition
    ): Function1View[I, O] = {
      v match {

        case vv: Function1View[_, _] => vv.asInstanceOf[Function1View[I, O]]
        case _                       =>
          Function1View(v.simplify, _definedAt)
      }
    }

    implicit def _as0View[O](v: CanSimplify[Thunk[O]])(
        implicit
        _definedAt: SrcDefinition
    ): Function0View[O] = {
      v match {

        case vv: Function0View[_] => vv.asInstanceOf[Function0View[O]]
        case _                    =>
          Function0View(v.simplify, _definedAt)
      }
    }

    implicit def fromFunction1[I, R](fn: I => R)(
        implicit
        _definedAt: SrcDefinition
    ): Fn.Impl[I ><: T0, R] = {
      fn match {
        case Function1View(c, _) => c.asInstanceOf[Fn.Impl[I ><: T0, R]]
        case _                   =>
          Blackbox[I, R](_definedAt)(fn)
      }
    }

    implicit def fromFunction0[R](fn: () => R)(
        implicit
        _definedAt: SrcDefinition
    ): Const.Impl[R] = {

      fn match {
        case Function0View(c, _) => c.asInstanceOf[Const.Impl[R]]
        case _                   =>
          case class ThunkImpl()(
              implicit
              override val _definedAt: SrcDefinition
          ) extends Impl[T0, R] {
            override def apply(arg: T0): R = fn()
          }
          val thunk = ThunkImpl()
          Const.Lazy(thunk.asInstanceOf[Thunk[R]])
      }
    }

    trait Pure {}

    object Pure {

      case class Is[I <: Args.Prod, R](delegate: Fn[I, R]) extends Impl[I, R] with Pure {

        override def apply(v: I): R & delegate.OutK[v.type] = delegate.apply(v).asInstanceOf[R & delegate.OutK[v.type]]
      }
    }

    trait CachedPure extends Pure

    // TODO: make a dependent class, also in Thunk
    final case class CachedImpl[I <: Args.Prod, R](backbone: Fn[I, R])(
        getLookup: () => CacheMagnet[I, R] = () => CanEqual.Native.Lookup[I, R]()
    ) extends Impl[I, R]
        with CachedPure {

      lazy val lookup: CacheMagnet[I, R] = getLookup()

      def apply(key: I): R = {
        lookup
          .getOrElseUpdateOnce(key) {

            val value = backbone(key)
            value
          }
          .asInstanceOf[R]
      }

      def getExisting(arg: I): Option[R] = {
        lookup
          .get(arg)
      }
    }

    override protected type BuildTarget[I, O] = Fn.Impl[I ><: T0, O]

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

    final override def apply(v: I): O = self(Args.cons(Const.Provided(v).asInstanceOf[Hom.ConstantFn[I]], Args.eye))

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

  sealed trait ConstantFn[+O] extends Fn[Args.Prod, O] with Fn.CachedPure {

    val compute: O // should mostly be a lazy val
  }

  type Thunk[+O] = Fn[T0, O]
  type Const[+O] = Thunk[O] & ConstantFn[O]

  object Const {

    // sanity (TODO: this may threaten behaviour of ZIO zippable or unzippable, need some test cases)
    implicitly[Const.Impl[Int] <:< Const[Int]]
    implicitly[Const[Int] <:< ConstantFn[Int]]

    sealed trait Impl[O] extends Fn.Impl[Args.Prod, O] with ConstantFn[O] { // <- CAUTION: this
      override def apply(arg: Args.Prod): O = compute
    }

    final case class Provided[O](compute: O) extends Impl[O] {}

    final case class Lazy[O](gen: Thunk[O]) extends Impl[O] {

      // equivalent to CachedLazy[Unit, O], but much faster
      @transient lazy val compute: O = gen(Args.eye)
    }

    case object NotProvided extends Impl[Nothing] {

      @transient lazy val compute: Nothing = throw new NoSuchElementException("missing, not provided")
    }
  }
}
