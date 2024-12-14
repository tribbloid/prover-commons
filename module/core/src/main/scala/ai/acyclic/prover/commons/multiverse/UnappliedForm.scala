package ai.acyclic.prover.commons.multiverse

import ai.acyclic.prover.commons.multiverse.UnappliedForm.Like

trait UnappliedForm extends Like {
  type T = Any
}

object UnappliedForm {

  trait Like {
//    import UnappliedForm.*

    type T // defaults to Any

    def prefix: String // use "" if no prefix

    def schema: Vector[Option[String]]

    def elements: Vector[T]

    def kvPairs: Vector[(Option[String], T)]

    def removeKeys(toBeRemoved: Set[String]): Pairs = {

      val filtered = kvPairs.filter {
        case (Some(k), _) =>
          !toBeRemoved.contains(k)
      }

      Pairs(prefix, filtered)
    }
  }

  case class Pairs(
      override val prefix: String = "",
      override val kvPairs: Vector[(Option[String], Any)]
  ) extends UnappliedForm {

    override lazy val schema: Vector[Option[String]] = kvPairs.map(_._1)

    @transient override lazy val elements: Vector[Any] = kvPairs.map(_._2)
  }

  trait Schematic extends UnappliedForm {
    // defining schema & values separately
    // due to dynamic loading, schema cannot be determined at compile time
    // TODO: or can it be?

    @transient override lazy val kvPairs: Vector[(Option[String], Any)] = {
      schema.zip(elements)
    }
  }

  object Singleton {

    val _schema = Vector(None)
  }

  case class Singleton(element: Any) extends Schematic {

    override def prefix: String = ""

    override def schema: Vector[Option[String]] = Singleton._schema

    @transient override lazy val elements: Vector[Any] = Vector(element)
  }

  case class Tuple(override val elements: Vector[Any]) extends Schematic {
    override def prefix: String = ""

    override def schema: Vector[Option[String]] = elements.map { _ =>
      None
    }

  }

  case class Named(
      override val prefix: String = "",
      names: Vector[String],
      override val elements: Vector[Any]
  ) extends Schematic {

    require(names.size == elements.size, "schema & elements must have the same size")

    @transient override lazy val schema: Vector[Option[String]] = names.map(s => Some(s))
  }
}
