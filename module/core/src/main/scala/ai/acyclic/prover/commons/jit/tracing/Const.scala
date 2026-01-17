package ai.acyclic.prover.commons.jit.tracing

case class Const[+T](concrete: T) extends Expr.Static[T] {}
