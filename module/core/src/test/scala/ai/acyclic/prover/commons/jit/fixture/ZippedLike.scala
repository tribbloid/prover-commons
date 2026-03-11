package ai.acyclic.prover.commons.jit.fixture

object ZippedLike {

  import Circuits.*

  val zipped = fn1.trace <*> fn2.trace

  val s1 = zipped.map {
    case (o1, o2) =>
      o1.zip(o2).map(v => v._1 + v._2)
  }

  val s2 =
    for (case (o1, o2) <- zipped)
      yield {
        o1.zip(o2).map(v => v._1 + v._2)
      }

  val s3 =
    for (
      tt <- zipped;
      o1 = tt._1;
      o2 = tt._2
    ) yield {

      o1.zip(o2).map(v => v._1 + v._2)
    }

  lazy val pairs = {

    Seq(
      zipped ->
        s"""
           |+ Zipped
           |!-- ${fn1.explain.nodeText}
           |!-- ${fn2.explain.nodeText}
           |""".stripMargin,
      s1 ->
        s"""
           |+ Mapped
           |!-+ Zipped
           |: !-- ${fn1.explain.nodeText}
           |: !-- ${fn2.explain.nodeText}
           |!-- Blackbox(s1 <at ZippedLike.scala:9>)
           |""".stripMargin,
      s1 ->
        s"""
            |+ Mapped
            |!-+ Zipped
            |: !-- ${fn1.explain.nodeText}
            |: !-- ${fn2.explain.nodeText}
            |!-- Blackbox(s1 <at ZippedLike.scala:9>)
            |""".stripMargin,
      s2 ->
        s"""
             |+ Mapped
             |!-+ Mapped
             |: !-+ Zipped
             |: : !-- ${fn1.explain.nodeText}
             |: : !-- ${fn2.explain.nodeText}
             |: !-- Blackbox(s2 <at ZippedLike.scala:15>)
             |!-- Blackbox(s2 <at ZippedLike.scala:15>)
             |""".stripMargin,
      s3 ->
        s"""
             |+ Mapped
             |!-+ Mapped
             |: !-+ Zipped
             |: : !-- Blackbox(fn1 <at Circuits.scala:12>)
             |: : !-- Blackbox(fn2 <at Circuits.scala:16>)
             |: !-- Blackbox(s3 <at ZippedLike.scala:22>)
             |!-- Blackbox(s3 <at ZippedLike.scala:22>)
             |""".stripMargin
    )
  }
}
