package com.tribbloids.graph.commons.util.reflect.format

import com.tribbloids.graph.commons.util.reflect.format.TypeFormat.Output

trait HasTypeInfo {

  type _TypeInfo
}

object HasTypeInfo {

  trait ConstV[T]
  case object ConstV extends TypeFormat {
    override def resolve(ff: Formatting): Output = {
      val u = ff.refl.universe
      val tt = ff.typeView.self.asInstanceOf[u.Type]

      tt match {
        case v: u.ConstantType =>
          v.value.value.toString
        case v: u.SingletonType =>
          v.toString
        case _ =>
          ff.resolved
      }
    }
  }

  trait ~~[A, B]
  case object ~~ extends TypeFormat {

    override def resolve(ff: Formatting): Output = {

      ff.resolved
    }
  }
}
