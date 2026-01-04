package ai.acyclic.prover.commons.multiverse.rewrite

import scala.language.implicitConversions

trait Delegating_Imp0 {
  self: Delegating.type =>

  implicit def unbox1[T]: Conversion[Delegating[T], T] = (v: Delegating[T]) => Delegating._unbox(v)

  implicit def unbox11[T](v: Delegating[T]): T = Delegating._unbox(v)
}
