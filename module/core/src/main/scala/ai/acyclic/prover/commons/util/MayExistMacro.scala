package ai.acyclic.prover.commons.util

import scala.reflect.macros.whitebox

class MayExistMacro(val c: whitebox.Context) {

  import c.universe.*

  def impl[A: c.WeakTypeTag]: c.Tree = {

    val requestedType = c.openImplicits.headOption
      .flatMap(_.pt.typeArgs.headOption)
      .getOrElse(weakTypeOf[A])
    val tpe = simplify(requestedType)
    val tpt = TypeTree(tpe)

    impossibleReason(tpe).foreach { reason =>
      c.abort(
        c.enclosingPosition,
        s"Type $tpe cannot have an instance because $reason"
      )
    }

    q"new _root_.ai.acyclic.prover.commons.util.MayExist[$tpt] {}"
  }

  private def impossibleReason(tpe0: Type): Option[String] = {

    val tpe = simplify(tpe0)

    if (tpe.typeSymbol == definitions.NothingClass) {
      Some("it is Nothing")
    } else {
      tpe match {
        case RefinedType(parents, _) =>
          refinedTypeReason(tpe, parents.map(simplify))
        case TypeRef(_, sym, _) if sym.isType && sym.asType.isAbstract =>
          abstractTypeReason(sym)
        case _ =>
          None
      }
    }
  }

  private def abstractTypeReason(sym: Symbol): Option[String] = {

    boundsOf(sym).flatMap { bounds =>
      val lower = simplify(bounds.lo)
      val upper = simplify(bounds.hi)

      if (!(lower <:< upper)) {
        Some(s"its lower bound $lower is not a subtype of its upper bound $upper")
      } else {
        impossibleReason(upper).map { reason =>
          s"its upper bound $upper cannot have an instance because $reason"
        }
      }
    }
  }

  private def refinedTypeReason(
      tpe: Type,
      parents: List[Type]
  ): Option[String] = {

    parents.view
      .flatMap { parent =>
        impossibleReason(parent).map { reason =>
          s"parent $parent cannot have an instance because $reason"
        }
      }
      .headOption
      .orElse {

        val classParents = minimalClassParents(parents)

        if (classParents.sizeCompare(1) > 0) {
          Some(s"it requires incompatible class parents ${classParents.mkString(" with ")}")
        } else {
          classParents.headOption.collect {
            case parent if isNonExtendable(parent) && !(parent <:< tpe) =>
              s"it is a proper subtype of non-extendable type $parent"
          }
        }
      }
  }

  private def boundsOf(sym: Symbol): Option[TypeBounds] = {

    sym.typeSignature match {
      case bounds: TypeBounds =>
        Some(bounds)
      case PolyType(_, bounds: TypeBounds) =>
        Some(bounds)
      case _ =>
        None
    }
  }

  private def minimalClassParents(parents: List[Type]): List[Type] = {

    val classParents = parents.filter(isClassParent)

    classParents.filterNot { parent =>
      classParents.exists { other =>
        !(other =:= parent) && (other <:< parent)
      }
    }
  }

  private def isClassParent(tpe: Type): Boolean = {

    val sym = tpe.typeSymbol

    sym != NoSymbol && sym.isClass && !sym.asClass.isTrait
  }

  private def isNonExtendable(tpe: Type): Boolean = {

    tpe match {
      case _: SingleType | _: ConstantType =>
        true
      case _ =>
        val sym = tpe.typeSymbol
        sym != NoSymbol && sym.isClass && (sym.isFinal || sym.asClass.isModuleClass)
    }
  }

  private def simplify(tpe: Type): Type = {

    tpe.dealias match {
      case AnnotatedType(_, underlying) =>
        simplify(underlying)
      case ExistentialType(_, underlying) =>
        simplify(underlying)
      case NullaryMethodType(result) =>
        simplify(result)
      case PolyType(_, result) =>
        simplify(result)
      case other =>
        other
    }
  }
}
