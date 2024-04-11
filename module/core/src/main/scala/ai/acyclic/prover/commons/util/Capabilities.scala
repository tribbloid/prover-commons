package ai.acyclic.prover.commons.util

/**
  * capability tracking enabler without linear/affine type system, all subclasses of HasCap[_] can be freely cast into
  * HasCap[C] for any C in runtime.
  *
  * One of the rare cases where JVM type erasure could be useful.
  */
trait Capabilities {

  type ^^[+T, +CC <: Cap] = T with _Can[CC] // following the convention of Scala 3.4.0 with capture checking

  trait MixinFn[CC <: Cap] {
    def apply[T](v: T): T ^^ CC = v.asInstanceOf[T ^^ CC]
  }

  def ^^[CC <: Cap] = new MixinFn[CC] {}

  trait NoCap {

    def enable[CC <: Cap]: NoCap.this.type ^^ CC = ^^[CC](this)
  }
  sealed trait _Can[+C <: Cap] {
    // DO NOT use outside this class, will trigger ClassCastException unless mixin

//    def enable[CC <: Cap]: _Can.this.type ^^ CC = this.asInstanceOf[this.type ^^ CC]
  }

//  type NoCap = Can[Cap]

  trait Cap {}
}
