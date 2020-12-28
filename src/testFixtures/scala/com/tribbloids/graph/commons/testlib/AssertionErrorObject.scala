package com.tribbloids.graph.commons.testlib

case class AssertionErrorObject(actual: List[String], expected: List[String]) {

  val ACTUAL =
    "[ACTUAL   /  LEFT]"
  val EXPECTED =
    "[EXPECTED / RIGHT]"

  lazy val actualStr: String = actual.mkString("\n")
  lazy val actualInfo: String = (
    s"\n=============================== $ACTUAL ================================\n\n" +
      actualStr
  ).trim

  lazy val expectedStr: String = expected.mkString("\n")
  lazy val expectedInfo: String = (
    s"\n=============================== $EXPECTED ================================\n\n" +
      expectedStr
  ).trim

  override lazy val toString: String = {

    val result = "\n" + s"""
         |"
         |$actualInfo
         |" did not equal "
         |$expectedInfo
         |"
      """.stripMargin.trim

    result
  }
}
