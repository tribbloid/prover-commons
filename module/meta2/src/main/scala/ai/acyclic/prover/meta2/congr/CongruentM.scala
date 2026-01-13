package ai.acyclic.prover.meta2.congr

object CongruentM {

  import scala.reflect.macros.whitebox

  def deriveCongruent[A: c.WeakTypeTag, B: c.WeakTypeTag](
      c: whitebox.Context
  ): c.Expr[Congruent[A, B]] = {

    import c.universe.*

    val tA = weakTypeOf[A]
    val tB = weakTypeOf[B]

    val congruentSym = weakTypeOf[Congruent[Any, Any]].typeSymbol

    (tA, tB) match {
      case (tA: TypeRef, tB: TypeRef) if tA.sym == tB.sym => // && tA.pre =:= tB.pre =>
//        c.abort(c.enclosingPosition, "A and B are the same type")
        // TODO: this logic is flawed, but sufficient for now

        val p1 = tA.pre
        val p2 = tB.pre

        val congruentType = appliedType(
          congruentSym,
          p1,
          p2
        )

        val found = c.inferImplicitValue(congruentType)

        if (found.isEmpty) {
          c.abort(
            c.enclosingPosition,
            s"Cannot find implicit Congruent[${p1}, ${p2}]"
          )
        } else {
          val result =
            q"new $congruentSym[$tA, $tB]($found.equality.asInstanceOf[$tA =:= $tB])"
          c.Expr[Congruent[A, B]](result)
        }

      case (tA: SingleType, tB: SingleType) if tA.sym == tB.sym =>
        val p1 = tA.pre
        val p2 = tB.pre

        val congruentType = appliedType(
          congruentSym,
          p1,
          p2
        )

        val found = c.inferImplicitValue(congruentType)

        if (found.isEmpty) {
          c.abort(
            c.enclosingPosition,
            s"Cannot find implicit Congruent[${p1}, ${p2}]"
          )
        } else {
          val result =
            q"new $congruentSym[$tA, $tB]($found.equality.asInstanceOf[$tA =:= $tB])"
          c.Expr[Congruent[A, B]](result)
        }

      case _ =>
        c.abort(c.enclosingPosition, s"Not valid for derived Congruence: $tA, $tB")
    }
  }
}
