package ai.acyclic.prover.commons.multiverse

trait CanToString[T] {

  val canUnapply: CanUnapply[T]

  lazy val bracket: (String, String) = "(" -> ")"

  case class ToString(v: T) {

//    final override def line: String = {
//      val unappliedForm = canUnapply.unapply(v)
//
//      unappliedForm.map { form => }
//    }
//
//    final override def paragraph: String = {}
//
//    final override def markdown: String = {}
  }

}
