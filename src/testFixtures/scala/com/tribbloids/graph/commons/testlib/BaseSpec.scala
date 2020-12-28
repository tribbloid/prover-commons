package com.tribbloids.graph.commons.testlib

import org.scalatest.funspec.AnyFunSpec

trait BaseSpec extends AnyFunSpec {

  @transient implicit class TestStringView(str: String) {

    //TODO: use reflection to figure out test name and annotate
    def shouldBe(
        gd: String = null,
        sort: Boolean = false,
        ignoreCase: Boolean = false,
        superSet: Boolean = false
    ): Unit = {

      val rows = str
        .split("\n")
        .toList

      var a: List[String] = rows
        .filterNot(_.replaceAllLiterally(" ", "").isEmpty)
        .map(v => ("|" + v).trim.stripPrefix("|"))
      if (sort) a = a.sorted
      if (ignoreCase) a = a.map(_.toLowerCase)

      Option(gd) match {
        case None =>
          println(AssertionErrorObject(rows, null).actualInfo)
        case Some(_gd) =>
          var b = _gd
            .split("\n")
            .toList
            .filterNot(_.replaceAllLiterally(" ", "").isEmpty)
            .map(v => ("|" + v).trim.stripPrefix("|"))
          if (sort) b = b.sorted
          if (ignoreCase) b = b.map(_.toLowerCase)
          if (superSet) {
            Predef.assert(
              a.intersect(b).nonEmpty,
              AssertionErrorObject(a, b)
            )
          } else {
            Predef.assert(
              a == b,
              AssertionErrorObject(a, b)
            )
          }
      }
    }

    def rowsShouldBe(
        gd: String = null
    ): Unit = shouldBe(gd, sort = true)

    def shouldBeLike(
        gd: String = null,
        sort: Boolean = false,
        ignoreCase: Boolean = false
    ): Unit = {
      val aRaw: List[String] = str
        .split("\n")
        .toList
        .filterNot(_.replaceAllLiterally(" ", "").isEmpty)
        .map(v => ("|" + v).trim.stripPrefix("|"))
      val a =
        if (sort) aRaw.sorted
        else aRaw

      Option(gd) match {
        case None =>
          println(AssertionErrorObject(a, null).actualInfo)
        case Some(_gd) =>
          var b = _gd
            .split("\n")
            .toList
            .filterNot(_.replaceAllLiterally(" ", "").isEmpty)
            .map(v => ("|" + v).trim.stripPrefix("|"))
          if (sort) b = b.sorted
          if (ignoreCase) b = b.map(_.toLowerCase)
          try {
            a.zipAll(b, null, null).foreach { tuple =>
              val fixes = tuple._2.split("[.]{6,}", 2)
              Predef.assert(
                tuple._1.startsWith(fixes.head)
              )
              Predef.assert(
                tuple._1.endsWith(fixes.last)
              )
            }
          } catch {
            case e: Exception =>
              throw new AssertionError("" + AssertionErrorObject(a, b), e)
          }
      }
    }

    def rowsShouldBeLike(gd: String = null): Unit = shouldBeLike(gd, sort = true)

    //    def uriContains(contains: String): Boolean = {
    //      str.contains(contains) &&
    //        str.contains(URLEncoder.encode(contains,"UTF-8"))
    //    }
    //
    //    def assertUriContains(contains: String): Unit = {
    //      assert(
    //        str.contains(contains) &&
    //        str.contains(URLEncoder.encode(contains,"UTF-8")),
    //        s"$str doesn't contain either:\n" +
    //          s"$contains OR\n" +
    //          s"${URLEncoder.encode(contains,"UTF-8")}"
    //      )
    //    }

  }
}

object BaseSpec {}
