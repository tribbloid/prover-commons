package ai.acyclic.prover.commons.multiverse.rewrite

object CanNormalise {

  type From[-T] = CanNormalise[T, Any]

  case class FromFn[T, R](
      fn: T => R
  ) extends CanNormalise[T, R] {

    override def normalise(v: T): R = fn(v)

  }
}

trait CanNormalise[-T, +R] extends CanRewrite[T] {

  final override def rewrite(v: T): Seq[Any] = Seq(normalise(v))
  def normalise(v: T): R
  // normalised form is assumed to be always equal `v`, validation may be required to be impl later
}
