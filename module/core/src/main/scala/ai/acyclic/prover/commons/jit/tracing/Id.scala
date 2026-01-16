package ai.acyclic.prover.commons.jit.tracing

import ai.acyclic.prover.commons.jit.hom.Hom.:=>

case class Id[T]() extends StaticTracingFn[T, T] {

  // TODO: should:
  //  - (override getValue) return a concrete Fn T :=> T
  //  - return a tracing proto Fn Var[T] :=> Expr[T] (see CanChain.parse output)
  //  - show whitebox best-effort tracing & JIT compilation process
  //  - make sure that the IR is associated with hom, not tracing/JIT
  //  this applies to every Constructor

  override val concrete: T :=> T = { v: T => v }
}
