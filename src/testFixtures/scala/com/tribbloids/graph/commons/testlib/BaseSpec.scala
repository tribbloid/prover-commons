package com.tribbloids.graph.commons.testlib

import com.tribbloids.graph.commons.util.debug.print_@
import com.tribbloids.graph.commons.util.diff.StringDiff
import org.scalatest.funspec.AnyFunSpec

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
}

object BaseSpec {}
