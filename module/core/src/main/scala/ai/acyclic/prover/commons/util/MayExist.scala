package ai.acyclic.prover.commons.util

import scala.language.experimental.macros

trait MayExist[+A]

object MayExist {
  implicit def materialize[A]: MayExist[A] = macro MayExistMacro.impl[A]
}
