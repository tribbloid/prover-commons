package com.tribbloids.graph.commons.util

trait HasID {
  import HasID._

  protected def _id: Any
  @transient final lazy val id: Any = {
    _idCompose(_id)
  }
}

object HasID {
  def _idCompose(id: Any): Any = {
    val result = id match {
      case aa: Array[_] => aa.toList
      case _            => id
    }
    result
  }
}
