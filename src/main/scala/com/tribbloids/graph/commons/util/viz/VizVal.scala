//package com.tribbloids.graph.commons.util.viz
//
//case class VizVal[T](
//    value: T,
//    inferredTTag: TypeTag[_],
//    runtimeClass: Class[_]
//) {
//
//  override def toString: String = {
//    s"""$value
//         | : ${inferredTTag.tpe}
//         | @ ${runtimeClass.getCanonicalName}
//         """.trim.stripMargin
//  }
//
//  lazy val inferType: VizType = {
//
//    VizType.apply(inferredTTag)
//  }
//}
