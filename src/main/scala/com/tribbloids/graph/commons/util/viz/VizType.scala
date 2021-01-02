package com.tribbloids.graph.commons.util.viz

import com.tribbloids.graph.commons.util.ScalaReflection.universe._
import com.tribbloids.graph.commons.util.TreeLike

import java.util.concurrent.atomic.AtomicInteger
import scala.collection.mutable

case class VizType(tt: Type) {

  import VizType._

  lazy val tree: Tree = Tree(tt)

  override def toString: String = {

    tree.toString
  }
}

object VizType {

  case class RefMap() {

    val ordinal = new AtomicInteger(0)

    case class Record(
        @volatile var count: Int = 0
    ) {

      lazy val sid: String = {

        val v = ordinal.getAndIncrement().toString
        v
      }
    }

    val records: mutable.HashMap[Type, Record] = mutable.HashMap.empty // Type -> Ordinal / Count

    def +=(_type: Type): Unit = {

      val count = records.getOrElseUpdate(_type, Record())

      count.count += 1
    }
  }

  case class Tree(
      _type: Type,
      visited: mutable.Set[Type] = mutable.Set.empty,
      expanded: RefMap = RefMap()
  ) extends TreeLike {

    {
      visited += _type
      expanded += _type
    }

    def refRecord: expanded.Record = expanded.records(_type)

    override lazy val nodeStr: String = {

      children.foreach(v => v.nodeStr)

      val str = show1Line(_type)
      val refFillLength = Math.max(5, 120 - str.length)
      val refFill = Array.fill(refFillLength)(".").mkString

      val rr = refRecord
      val ref = if (rr.count >= 2) {
        val i = rr.sid
        s" $refFill [$i]"
      } else {
        ""
      }
      str + ref
    }

    object ArgTree extends TreeLike {
      override val nodeStr: String = "  [ PARAMETER(S) ] :"

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

    protected lazy val baseTypes: List[Type] = if (refRecord.count >= 2) {

      Nil
    } else {

      val baseClzs = _type.baseClasses

      val baseTypes = baseClzs.map { clz =>
        _type.baseType(clz)
      }

      val notSelf = baseTypes.filterNot { tt =>
        tt =:= _type
      }

      notSelf
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
