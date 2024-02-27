package ai.acyclic.prover.commons.util

/**
  * capability tracking enabler without linear/affine type system, all subclasses of HasCap[_] can be freely cast into
  * HasCap[C] for any C in runtime.
  *
  * One of the rare cases where JVM type erasure could be useful.
  */
trait Capabilities {

  type @@[T, CC <: Cap] = T with Can[CC]

  trait Can[+C <: Cap] {

    def enable[CC <: Cap]: Can.this.type @@ CC = this.asInstanceOf[this.type with Can[CC]]
  }
  type NoCap = Can[Cap]

  trait Cap
}
