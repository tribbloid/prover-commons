package ai.acyclic.prover.commons.multiverse

import ai.acyclic.prover.commons.multiverse.UnappliedForm.Like

trait UnappliedForm extends Like {
  type T = Any
}

object UnappliedForm {

  trait Like {
    self: UnappliedForm =>
//    import UnappliedForm.*

    type T // defaults to Any

    def prefix: String // use "" if no prefix

    def schema: Seq[Option[String]]

    def values: Seq[T]

    def kvPairs: Seq[(Option[String], T)]

    def remove(
        keys: Set[String] = Set.empty,
        values: Set[Any] = Set.empty
    ): Pairs = {

      val filtered = kvPairs.filterNot {
        case (kOpt, v) =>
          val keyContains = kOpt.forall { k =>
            keys.contains(k)
          }

          val valueContains = values.contains(v)

          keyContains || valueContains
      }

      Pairs(filtered, prefix)
    }
  }

  case class Pairs(
      override val kvPairs: Seq[(Option[String], Any)],
      override val prefix: String = ""
  ) extends UnappliedForm {

    override lazy val schema: Seq[Option[String]] = kvPairs.map(_._1)

    @transient override lazy val values: Seq[Any] = kvPairs.map(_._2)
  }

  trait Schematic extends UnappliedForm {
    // defining schema & values separately
    // due to dynamic loading, schema cannot be determined at compile time
    // TODO: or can it be?

    @transient override lazy val kvPairs: Seq[(Option[String], Any)] = {
      schema.zip(values)
    }
  }

  object Singleton {

    val _schema = Vector(None)
  }

  case class Singleton(
      element: Any,
      override val prefix: String = ""
  ) extends Schematic {

    override def schema: Vector[Option[String]] = Singleton._schema

    @transient override lazy val values: Vector[Any] = Vector(element)
  }

  case class Tuple(
      override val values: Vector[Any],
      override val prefix: String = ""
  ) extends Schematic {

    override def schema: Vector[Option[String]] = values.map { _ =>
      None
    }
  }

  case class Named(
      names: Vector[String],
      override val values: Vector[Any],
      override val prefix: String = ""
  ) extends Schematic {

    require(names.size == values.size, "schema & elements must have the same size")

    @transient override lazy val schema: Vector[Option[String]] = names.map(s => Some(s))
  }
}
