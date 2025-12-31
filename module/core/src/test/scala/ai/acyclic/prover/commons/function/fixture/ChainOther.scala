package ai.acyclic.prover.commons.function.fixture

import ai.acyclic.prover.commons.function.hom.Hom.:=>

object ChainOther {

  import Circuits.*

  val s1: Int :=> String = fn0.andThen[String] { v =>
    s"${v}b"
  }

  val s2: Int :=> String = for (v <- fn0.trace) yield {
    s"${v}b"
  }

  val s3: Int :=> String = {

    val tracing: :=>.Tracing[Int, String] =
      for (
        v: Int <- :=>.id[Int].trace;
        x: Int = v + 1;
        v2: Int <- :=>.id[Int].trace;
        y: Int = x + v + v2 + 1
      ) yield {
        y.toString
      }

    tracing.unbox
  }

  val s4: Int :=> String = {

//    implicit val _definedAt: ai.acyclic.prover.commons.debug.SrcDefinition =
//      ai.acyclic.prover.commons.debug.SrcDefinition.InlinedWithText[String](
//        sourcecode.File("ChainOther.scala"),
//        sourcecode.Line(21),
//        sourcecode.Name("tracing"),
//        Some("")
//      )

    val tracing: :=>.Tracing[Int, String] =
      :=>.id[Int].trace
        .withFilter { _: Int =>
          true
        }
        .map { v: Int =>
          v
        }
        .flatMap { v: Int =>
          val x: Int = v + 1
          :=>.id[Int].trace.map { v2: Int =>
            val y: Int = x + v + v2 + 1
            y.toString
          }
        }

    tracing.unbox
  }

  def main(args: Array[String]): Unit = {
    val strs = Seq(s3, s4).map { s =>
      s.explain.text_hierarchy()
    }

    println(strs(0))
    println("===")
    println(strs(1))
  }

  lazy val pairs: Seq[(Int :=> String, String)] = {

    val pairs: Seq[(Int :=> String, String)] = {
      Seq(
        (
          s1,
          s"""
             |+ Mapped
             |!-- ${fn0.explain.nodeText}
             |!-- Blackbox(s1 <at ChainOther.scala:9>)
             |""".stripMargin
        ),
        (
          s2,
          s""" 
             |+ Mapped
             |!-- ${fn0.explain.nodeText}
             |!-- Blackbox(s2 <at ChainOther.scala:13>)
             |""".stripMargin
        )
//        (s3, ""),
//        (s4, "")
      )
    }

    pairs
  }
}
