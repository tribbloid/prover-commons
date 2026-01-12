package ai.acyclic.prover.commons.congr

object CongruentM {
  def deriveCongruent[A: Type, B: Type](
      using
      Quotes
  ): Expr[Congruent[A, B]] = {

    (TypeRepr.of[A], TypeRepr.of[B]) match {
      case (tA: NamedType, tB: NamedType) if tA.name == tB.name && tA.typeSymbol == tB.typeSymbol =>
        val p1 = tA.qualifier
        val p2 = tB.qualifier
        (p1.asType, p2.asType) match {
          case ('[p1], '[p2]) =>
            Expr.summon[Congruent[p1, p2]] match {
              case Some(c) =>
                '{ new Congruent[A, B]((${ c }).equality.asInstanceOf[A =:= B]) }
              case None =>
                report.errorAndAbort(
                  s"Cannot find implicit Congruent[${Type.show[p1]}, ${Type.show[p2]}]"
                )
            }
        }
      case _ =>
        report.errorAndAbort("Not valid for derived Congruence")
    }
  }
}
