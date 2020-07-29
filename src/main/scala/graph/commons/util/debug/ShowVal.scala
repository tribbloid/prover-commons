package graph.commons.util.debug

import graph.commons.util.TypeTag

case class ShowVal[T](
    value: T,
    inferredTTag: TypeTag[_],
    runtimeClass: Class[_]
) {

  override def toString: String = {
    s"""$value
         | : ${inferredTTag.tpe}
         | @ ${runtimeClass.getCanonicalName}
         """.trim.stripMargin
  }

  lazy val inferType: ShowType = {

    ShowType.apply(inferredTTag)
  }
}
