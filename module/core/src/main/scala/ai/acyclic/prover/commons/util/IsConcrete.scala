package ai.acyclic.prover.commons.util

import scala.language.experimental.macros
import scala.reflect.macros.blackbox

trait IsConcrete[T]

object IsConcrete {

  implicit def materialize[T]: IsConcrete[T] = macro IsConcreteMacro.impl[T]
}

class IsConcreteMacro(val c: blackbox.Context) {

  import c.universe.*

  def impl[T: c.WeakTypeTag]: c.Expr[IsConcrete[T]] = {

    val tpe = weakTypeOf[T].dealias

    if (tpe.typeSymbol == definitions.NothingClass) {
      c.abort(c.enclosingPosition, s"Type $tpe is Nothing, which is not concrete")
    }

    tpe match {
      case RefinedType(_, _) =>
        c.abort(c.enclosingPosition, s"Type $tpe is a refined type, which is not concrete")
      case _ =>
    }

    c.Expr[IsConcrete[T]](q"new ai.acyclic.prover.commons.util.IsConcrete[$tpe] {}")
  }
}
