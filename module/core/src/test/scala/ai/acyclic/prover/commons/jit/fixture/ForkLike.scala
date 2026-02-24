package ai.acyclic.prover.commons.jit.fixture

object ForkLike {

  import Circuits.*

  private val forked = fn1.trace <%> fn2.trace

  val s1 = forked.map {
    case (o1, o2) =>
      o1.zip(o2).map(v => v._1 + v._2)
  }

  lazy val pairs = {

    Seq(
      forked ->
        s"""
           |+ Mapped
           |!-+ Pointwise
           |: !-- ${fn1.explain.nodeText}
           |: !-- ${fn2.explain.nodeText}
           |!-- Blackbox(s1 <at PointwiseAndChain.scala:12>)
           |""".stripMargin,
      s1 ->
        s"""
            |+ Mapped
            |!-+ Pointwise
            |: !-- ${fn1.explain.nodeText}
            |: !-- ${fn2.explain.nodeText}
            |!-- Blackbox(s1 <at PointwiseAndChain.scala:12>)
            |""".stripMargin
    )
  }
}
