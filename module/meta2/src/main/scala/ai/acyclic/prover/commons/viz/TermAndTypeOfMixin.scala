package ai.acyclic.prover.commons.viz

import ai.acyclic.prover.commons.diff.StringDiff

trait TermAndTypeOfMixin extends TypeOfMixin {

  class TermAndTypeOf[T](
      val rtValue: T,
      override val tt: universe.Type
  ) extends TypeOf(tt) {

    //  override def toString: String = {
    //    s"""
    //       |$value
    //       |
    //       |${tree.treeString}
    //       |""".stripMargin.trim
    //  }

    def ===!(that: TermAndTypeOf[_] = null): Unit = {

      should_=:=(that)

      val Seq(s1, s2) = Seq(this, that).map { v =>
        Option(v).map(_.rtValue.toString)
      }

      val diff = StringDiff(s1, s2, Seq(this.getClass))

      (diff.Left.isDefined, diff.Right.isDefined) match {

        case (true, true) =>
          Predef.assert(
            this.rtValue == that.rtValue,
            diff.ErrorDiffClue
          )

        case _ =>
          diff.show()
      }
    }
  }

}
