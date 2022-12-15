package ai.acyclic.prover.commons.testlib

import ai.acyclic.prover.commons.debug.print_@
import ai.acyclic.prover.commons.diff.StringDiff
import org.scalatest.funspec.AnyFunSpec
import splain.test.TryCompile

import java.util.regex.Pattern

trait BaseSpec extends AnyFunSpec with TryCompile.Static.default.FromCodeMixin {

  @transient implicit class testStringView(str: String) {

    val _print = new print_@(
      Seq(
        this.getClass
      )
    )

    // TODO: use reflection to figure out test name and annotate
    def shouldBe(
        groundTruth: String = null,
        sort: Boolean = false,
        ignoreCase: Boolean = false,
        trim: Boolean = true,
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

      StringDiff(Option(str), Option(groundTruth), Seq(this.getClass), sort, ignoreCase).assert(mode, fuzzyRight = true)
    }

    def rowsShouldBeLike(gd: String = null): Unit = shouldBeLike(gd, sort = true)
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
}

object BaseSpec {}
