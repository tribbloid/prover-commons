package ai.acyclic.prover.commons.testlib

import ai.acyclic.prover.commons.diff.StringDiff
import ai.acyclic.prover.commons.typesetting.TextBlock
import ai.acyclic.prover.commons.util.Summoner
import org.scalatest.funspec.AnyFunSpec
import splain.test.TryCompile

import java.util.regex.Pattern
import scala.reflect.ClassTag

trait BaseSpec extends AnyFunSpec with TryCompile.Static.default.FromCodeMixin {
  
  import scala.reflect.runtime.universe.TypeTag

  @transient implicit class _StringOps(str: String) {

    // TODO: use reflection to figure out test name and annotate
    def shouldBe(
        groundTruth: String = null,
        sort: Boolean = false,
        ignoreCase: Boolean = false,
        trim: TextBlock => TextBlock = StringDiff.defaultTrim,
        mode: StringDiff.ComparisonMode = StringDiff.Equal
    ): Unit = {

      StringDiff(
        left = Option(str),
        right = Option(groundTruth),
        classes = Seq(this.getClass),
        sort = sort,
        ignoreCase = ignoreCase,
        trim = trim
      ).assert(mode)
    }

    def rowsShouldBe(
        gd: String = null
    ): Unit = shouldBe(gd, sort = true)

    def shouldBeLike(
        groundTruth: String = null,
        sort: Boolean = false,
        ignoreCase: Boolean = false,
        mode: StringDiff.ComparisonMode = StringDiff.Equal
    ): Unit = {

      StringDiff(
        Option(str),
        Option(groundTruth),
        Seq(this.getClass),
        sort,
        ignoreCase
      ).assert(mode, fuzzyRight = true)
    }

    def rowsShouldBeLike(gd: String = null): Unit = shouldBeLike(gd, sort = true)
  }

  @transient implicit class StringSeqOps[F[_]](self: F[String])(
      implicit
      ev: F[String] => Seq[String]
  ) {

    def shouldBeIdentical(): Unit = {

      self.reduce { (v1, v2) =>
        v1 shouldBe v2
        v2
      }
    }
  }

  /**
    * ScalaTest assertDoesNotCompile sometimes malfunction (due to unwashed execution order?) & doesn't perform literal
    * check if the code compiles successfully, the project compilation will fail
    */
//  val shouldNotCompile: illTyped.type = illTyped

  // renamed to "shouldNotType"
  def shouldNotCompile(
      tryCompile: TryCompile,
      pattern: String = null
  ): Unit = {

    tryCompile match {

      case v: TryCompile.TypingError =>
        Option(pattern).foreach { pp =>
          val errorMsg = v.Error.filteredIssues.map(ii => ii.msg).mkString("\n").trim

          val _pp = Pattern.compile(pp, Pattern.DOTALL)
          val fit = _pp.matcher(errorMsg).matches()

          errorMsg.matches(pp)
          assert(
            fit,
            s"""
                 |expecting type error with pattern:
                 |$pp
                 |
                 |but get:
                 |$errorMsg
                 |""".stripMargin
          )
        }

      case v @ _ =>
        throw new AssertionError(
          s"""
             |expecting type error, but get:
             |$v
             |""".stripMargin.trim
        )
    }
  }

  def typeOfIt[T: TypeTag](
      subject: T
  )(fn: T => Unit): Unit = {

    val ttg: TypeTag[T] = Summoner.summon[TypeTag[T]]
    it(ttg.tpe.toString) {
      fn(subject)
    }
  }

  def classOfIt[T: ClassTag](
      subject: T
  )(fn: T => Unit): Unit = {
    val ctg: ClassTag[T] = Summoner.summon[ClassTag[T]]
    it(ctg.runtimeClass.toString) {
      fn(subject)
    }
  }
}

object BaseSpec {}
