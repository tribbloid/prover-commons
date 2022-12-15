package ai.acyclic.prover.commons.viz

import ai.acyclic.prover.commons.reflect.format.Formats0
import ai.acyclic.prover.commons.reflect.{Reflection, ScalaReflection}

class TypeViz[R <: Reflection](
    override val reflection: R,
    override val format: TypeVizFormat
) extends TypeVizLike {

  override type TTag[T] = universe.WeakTypeTag[T]

  object Strong extends TypeVizLike {

    override type TTag[T] = TypeTag[T]
    override val reflection: Reflection = ScalaReflection

    override val format: TypeVizFormat = TypeViz.this.format
  }

  def withFormat(format: TypeVizFormat = TypeVizFormat.Default) = new TypeViz[R](reflection, format)
}

object TypeViz extends TypeViz(ScalaReflection, TypeVizFormat.Default) {

  def apply[R <: Reflection](reflection: R) = new TypeViz[R](reflection, TypeVizFormat.Default)

  trait Fixtures {

    val TypeViz = ai.acyclic.prover.commons.viz.TypeViz

    val TypeVizShort = {

      val format = Formats0.TypeInfo.HidePackage.recursively.DeAlias
      TypeViz.withFormat(format)
    }

    val TypeVizDeAlias = {

      val format = Formats0.TypeInfo.DeAlias
      TypeViz.withFormat(format)
    }
  }
}
