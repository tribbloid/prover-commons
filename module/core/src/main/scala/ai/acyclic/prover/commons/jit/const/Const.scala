//package ai.acyclic.prover.commons.jit.const
//
//sealed trait Const[O] extends Any { // <- CAUTION: this
//  def value: O
//}
//
//final case class Provided[O](value: O) extends AnyVal with Const[O] {}
//
//final case class Lazy[O](gen: () => O) extends AnyVal with Const[O] {
//
//  // equivalent to CachedLazy[Unit, O], but much faster
//  @transient lazy val value: O = gen()
//}
//
//final case class NotProvided[O](u: Unit) extends AnyVal with Const[O] {
//
//  @transient def value: O = throw new NoSuchElementException("missing, not provided")
//}
