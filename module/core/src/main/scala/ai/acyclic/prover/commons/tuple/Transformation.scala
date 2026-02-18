package ai.acyclic.prover.commons.tuple

import ai.acyclic.prover.commons.jit.hom.Hom

// TODO: implement this, don't change definition, report all the required abstract functions at the end of the trait
/**
  * Polymorphic function, each instance can convert from [[from.system.Prod]] to [[to.system.Prod]]
  *
  * e.g.
  *   - from.system.Empty -> to.system.Empty
  *   - from.G[A] ><: from.system.Empty -> to.G[A] ><: to.system.Empty
  *   - from.G[A] ><: from.G[B] ><: from.system.Empty -> to.G[A] ><: to.G[B]><: to.system.Empty
  *   - ...
  */
trait Transformation extends Hom.Poly {
  import Transformation.*

  val from: Schema
  val to: Schema

}

object Transformation {

  trait Schema {

    val system: Schemata.Monoidal
    type G[T] <: system.VBound
  }

}
