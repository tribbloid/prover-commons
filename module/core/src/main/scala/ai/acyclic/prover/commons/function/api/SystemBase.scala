package ai.acyclic.prover.commons.function.api

import ai.acyclic.prover.commons.debug.Debug

import scala.language.implicitConversions

trait SystemBase extends HasMono with Serializable {
  self: Singleton =>

  trait API extends Fn.CanBuild {

    implicit def asFnSystem(v: this.type): SystemBase.this.type = SystemBase.this

    override type =>>[i <: IUB, o] = FnImpl[i, o]

    override protected def _defining[I <: IUB, R](fn: I => R)(
        implicit
        _definedAt: Debug.CallStackRef
    ): I =>> R = Fn(fn)

  }
}
