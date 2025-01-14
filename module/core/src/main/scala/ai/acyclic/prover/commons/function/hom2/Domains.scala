package ai.acyclic.prover.commons.function.hom2

import ai.acyclic.prover.commons.util.Erased

trait Domains extends Erased {

  type _I // Domain, Min
  type _O[T] // Codomain, Max
}
