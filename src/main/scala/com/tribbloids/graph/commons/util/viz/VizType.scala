package com.tribbloids.graph.commons.util.viz

import com.tribbloids.graph.commons.util.ScalaReflection.universe._
import com.tribbloids.graph.commons.util.TreeLike

import scala.collection.mutable

case class VizType(tt: Type) {

  import VizType._

  lazy val tree: Tree = Tree(tt)

  override def toString: String = {

    tree.toString
  }
}

object VizType {

  case class Tree(
      _type: Type,
      visited: mutable.Set[Type] = mutable.Set.empty,
      expanded: mutable.ArrayBuffer[Type] = mutable.ArrayBuffer.empty
  ) extends TreeLike {

    {
      visited += _type
      expanded += _type
    }

    def expandedCount: Int = expanded.count(v => v =:= _type)

    override lazy val nodeStr: String = {

      children.foreach(v => v.nodeStr)

      val str = show1Line(_type)
      val ref = if (expandedCount >= 2) {
        val i = expanded.indexOf(_type)
        s" [$i]"
      } else {
        ""
      }
      str + ref
    }

    object ArgTree extends TreeLike {
      override val nodeStr: String = "  (Parameters) :"

      override lazy val children: List[Tree] = _type.typeArgs.map { tt =>
        copy(tt, mutable.Set.empty)
      }
    }

    lazy val argTreeOpt: Option[ArgTree.type] = {
      if (ArgTree.children.isEmpty) None
      else {
        Some(ArgTree)
      }
    }

    protected lazy val baseTypes: List[Type] = if (expandedCount >= 2) {

      Nil
    } else {

      val baseClzs = _type.baseClasses

      val baseTypes = baseClzs.map { clz =>
        _type.baseType(clz)
      }

      baseTypes
    }

    override lazy val children: List[TreeLike] = {
      val result = argTreeOpt.toList ++
        baseTypes.flatMap { tt =>
          if (visited.contains(tt)) None
          else {
            val result = copy(tt)
            result.nodeStr // depth first
            Some(result)
          }
        }

      result
    }
  }

  def show1Line(tpe: Type): String = {
//    tpe.typeArgs
//
//    tpe.typeParams

//    s"$tpe \t:= ${showRaw(tpe.typeSymbol.typeSignature)}"

    tpe.toString
  }

  def apply[T](implicit ttag: TypeTag[T]): VizType = VizType(ttag.tpe)

  def infer[T: TypeTag](v: T): VizType = apply[T]
}
