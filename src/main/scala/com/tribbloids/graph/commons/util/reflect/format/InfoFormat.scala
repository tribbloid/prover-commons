package com.tribbloids.graph.commons.util.reflect.format

import com.tribbloids.graph.commons.util.reflect.format.TypeFormat.Output

object InfoFormat {

  trait ConstV[T]
  case object ConstV extends TypeFormat {
    override def resolve(ff: Formatting): Output = {
      val u = ff.refl.getUniverse
      val tt = ff.typeView.self.asInstanceOf[u.Type].dealias

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
