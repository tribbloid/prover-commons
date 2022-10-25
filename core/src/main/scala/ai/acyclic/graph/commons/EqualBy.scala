package ai.acyclic.graph.commons

trait EqualBy {

  import EqualBy._

  protected def _equalBy: Any
  @transient final private lazy val equalBy: Any = {
    rectify(_equalBy)
  }

  override def hashCode: Int = equalBy.##
  override def equals(v: Any): Boolean = {
    if (v == null) false
    else if (v.isInstanceOf[AnyRef] && this.eq(v.asInstanceOf[AnyRef])) true
    else if (v.isInstanceOf[EqualBy]) { // TODO: should subclass be allowed to == this?
      (v.getClass == this.getClass) &&
      (v.asInstanceOf[EqualBy].equalBy == this.equalBy)
    } else false
  }
}

object EqualBy {

  trait FieldsWithTolerance extends EqualBy with Product {

    @transient override protected lazy val _equalBy: (String, List[Any]) =
      productPrefix -> productIterator.map(truncateToTolerance).toList

    def truncateToTolerance(v: Any): Any
  }

  trait Fields extends FieldsWithTolerance {

    def truncateToTolerance(v: Any): Any = identity _
  }

  def rectify(id: Any): Any = {
    val result = id match {
      case aa: Array[_] => aa.toList
      case _            => id
    }
    result
  }
}
