package org.shapesafe.graph.commons.testlib

import org.shapesafe.graph.commons.util.debug.print_@
import org.shapesafe.graph.commons.util.diff.StringDiff
import org.scalatest.funspec.AnyFunSpec
import shapeless.test.illTyped

trait BaseSpec extends AnyFunSpec {

  @transient implicit class TestStringView(str: String) {

    val _print = new print_@(
      Seq(
        this.getClass
      )
    )

    //TODO: use reflection to figure out test name and annotate
    def shouldBe(
        groundTruth: String = null,
        sort: Boolean = false,
        ignoreCase: Boolean = false,
        mode: StringDiff.ComparisonMode = StringDiff.Equal
    ): Unit = {

      StringDiff(Option(str), Option(groundTruth), Seq(this.getClass), sort, ignoreCase).assert(mode)
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
    * ScalaTest assertDoesNotCompile sometimes malfunction (due to unwashed execution order?)
    * & doesn't perform literal check
    * if the code compiles successfully, the project compilation will fail
    */
  val shouldNotCompile: illTyped.type = illTyped
}

object BaseSpec {}
