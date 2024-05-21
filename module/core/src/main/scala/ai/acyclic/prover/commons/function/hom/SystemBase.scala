package ai.acyclic.prover.commons.function.hom

import scala.language.implicitConversions

trait SystemBase extends HasMono with Serializable {
  self: Singleton =>

  trait Entry {

    implicit def asFnSystem(v: this.type): SystemBase.this.type = SystemBase.this
  }
}
