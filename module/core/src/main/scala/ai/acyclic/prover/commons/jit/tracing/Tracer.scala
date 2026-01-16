package ai.acyclic.prover.commons.jit.tracing

/**
  * a placeholder of (value: O), if the value is concrete, [[TracingFn]]'s primary form can be invoked on it to yield an
  * executable task. But before this happen, it is used to build the computation graph of a [[TracingFn]] before its
  * invocation
  *
  * Ideally this graph can be derived at compile-time (with meta-rewriter), unfortunately our compiler is janky at the
  * moment and we have to do it later
  */
trait Tracer[+O] {}

object Tracer {

  /**
    * AKA tracing-by-run: [[TracingFn]]'s AOT form can be invoked to yield the computation graph or at least part of it
    */
  trait RuntimeAOT[+O] extends Tracer[O] {}

  /**
    * in addition to tracing-by-run, each execution of [[TracingFn]] also reveal some internal data of its computation
    * graph, which are then collected for further JIT optimisation
    */
  trait RuntimeEmpirical[O] extends RuntimeAOT[O] {

    // TODO: This capability will be enabled for dynamic recursion, non-termination, graph breaking or empirical JIT optimisation
  }
}
