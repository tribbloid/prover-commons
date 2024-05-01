package ai.acyclic.prover.commons.function

import ai.acyclic.prover.commons.function.api.{HasMono, HasPoly}

import scala.language.implicitConversions

trait HomSystem_Imp0 extends HasMono with HasPoly with Serializable {

  final type IUB = Any

  class FnAsFunction1[I, R](val self: FnCompat[I, R]) extends (I => R) with Serializable {

    final override def apply(v1: I): R = self.apply(v1)

    final override def toString: String = self.toString // preserve reference transparency

  }

  implicit def _fnAsFunction1[I, R](fn: FnCompat[I, R]): I => R = {
    new FnAsFunction1[I, R](fn)
  }
}
