package com.tribbloids.graph.commons.issues

// https://stackoverflow.com/questions/67133297/in-scala-how-to-eliminate-unchecked-since-it-is-eliminated-by-erasure-error-f
import scala.reflect.api.Universe

trait OuterSpike2 {

  val universe: Universe

  type Type = universe.Type

  case class TypeView(
      self: Type,
      comment: Option[String] = None
  ) {

    lazy val baseTypes: List[TypeView] = {

      self match {
        case v: Type with scala.reflect.internal.Types#Type =>
        case _ =>

      }

      ???
    }
  }
}
