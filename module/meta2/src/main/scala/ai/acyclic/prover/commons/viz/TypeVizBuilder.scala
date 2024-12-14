package ai.acyclic.prover.commons.viz

import ai.acyclic.prover.commons.HasInner
import ai.acyclic.prover.commons.meta.ScalaReflection
import ai.acyclic.prover.commons.refl.Reflection

class TypeVizBuilder[R <: Reflection](
    val reflection: R,
    val format: TypeHierarchy
) extends HasInner {

  sealed abstract class _TypeViz(
      val reflection: R = TypeVizBuilder.this.reflection
  ) extends TypeViz[R]
      with _Inner {

    override val format: TypeHierarchy = TypeVizBuilder.this.format
  }

  abstract class Weak extends _TypeViz() {

    override type TTag[T] = WeakTypeTag[T]

    def withFormat(format: TypeHierarchy = TypeHierarchy.Default) =
      new TypeVizBuilder(this.reflection, format).Weak
  }
  object Weak extends Weak

  abstract class Strong extends _TypeViz() {

    override type TTag[T] = TypeTag[T]

    def withFormat(format: TypeHierarchy = TypeHierarchy.Default) =
      new TypeVizBuilder(this.reflection, format).Strong
  }
  object Strong extends Strong

}

object TypeVizBuilder {

  object RuntimeDefault extends TypeVizBuilder(ScalaReflection, TypeHierarchy.Default) {}
}
