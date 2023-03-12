package ai.acyclic.prover.commons

trait EqualBy extends EqualBy.EqualByMixin {

  protected def _equalBy: Any
//  @transient final private lazy val equalBy: Any = {
//    rectify(_equalBy)
//  }
//
//  override def hashCode: Int = equalBy.##
//  override def equals(v: Any): Boolean = {
//    if (v == null) false
//    else if (v.isInstanceOf[AnyRef] && this.eq(v.asInstanceOf[AnyRef])) true
//    else if (v.isInstanceOf[EqualBy]) { // TODO: should subclass be allowed to == this?
//      (v.getClass == this.getClass) &&
//      (v.asInstanceOf[EqualBy].equalBy == this.equalBy)
//    } else false
//  }
}

object EqualBy extends Sameness.Evidence {

//  trait FieldsWithTolerance extends EqualBy with Product {
//
//    @transient override protected lazy val _equalBy: (String, List[Any]) =
//      productPrefix -> productIterator.map(truncateToTolerance).toList
//
//    def truncateToTolerance(v: Any): Any
//  }
//
//  trait Fields extends FieldsWithTolerance {
//
//    def truncateToTolerance(v: Any): Any = v
//  }
//
//
//  case class Construction[T](v: T) extends EqualBy {
//    override protected def _equalBy: (Int, T) = util.constructionID(v)
//  }

  override protected def getIDInternal(v1: Any): Any = {
    v1 match {
      case v: EqualBy => v._equalBy
      case _          => v1
    }
  }
}
