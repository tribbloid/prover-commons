package ai.acyclic.prover.commons.jit.tracing

import ai.acyclic.prover.commons.debug.SrcDefinition

import java.util.UUID

/**
  * AOT/empirical tracer always use tapes that are stateful
  *
  * henceforth tracer should always be created on the fly, with a new tape
  *
  * for multiple variable interacting with each other in a function, their tracers have to be defined in the same Env
  */
case class AOTEnv() {

  class Tracer[T](
      val defAt: SrcDefinition,
      val inhabited: Option[Inhabited[T]] = None
  ) extends Expr[T] {

    final type Pending = T

    final def env: AOTEnv = AOTEnv.this

    val uuid: UUID = UUID.randomUUID()

    override def getConcrete(
        implicit
        defAt: SrcDefinition
    ): T = inhabited
      .map(_.getExample)
      .getOrElse(
        super.getConcrete
      )
  }

  object Tracer {
    def apply[T](
        implicit
        defAt: SrcDefinition
    ): Tracer[T] = new Tracer[T](defAt)
  }
}

object AOTEnv {}
