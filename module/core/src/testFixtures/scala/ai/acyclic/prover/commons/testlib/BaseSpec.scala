package ai.acyclic.prover.commons.testlib

import ai.acyclic.prover.commons.diff.StringDiff
import ai.acyclic.prover.commons.typesetting.TextBlock

trait BaseSpec extends ai.acyclic.prover.infra.testlib.BaseSpec {

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

}

object BaseSpec {}
