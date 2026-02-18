package ai.acyclic.prover.commons.compat

import ai.acyclic.prover.commons.jit.hom.Hom
import ai.acyclic.prover.commons.jit.hom.Hom.Poly
import ai.acyclic.prover.commons.tuple.{Products, Schemata}
import shapeless.labelled.FieldType
import shapeless.ops.hlist.Tupler
import shapeless.{Generic, HList}

trait HasTupleX {

  type TupleX = TupleX.Prod

  type *:[+X, +Y <: TupleX] = TupleX.><:[X, Y]

  object TupleX extends Products.Monoidal { // TODO: name is weird, also moved to tuple package

    override type VBound = Any
    override type Element[T] = T

    trait OpsMixin { // hollow inside, but mixin will bring _ops into the implicit scope
      self: TupleX =>
    }

    implicit class _ops[T <: TupleX](self: T) {

      def *:[H](h: H): *:[H, T] & OpsMixin = (h :: self).asInstanceOf[*:[H, T] & OpsMixin]
    }

    override type Prod = shapeless.HList

    override type Eye = shapeless.HNil
    val Eye: shapeless.HNil & OpsMixin = shapeless.HNil.asInstanceOf[shapeless.HNil & OpsMixin]

    type Unital = Eye // "Unit" has special meaning in Scala
    val Unital: Eye = Eye

    override infix type ><:[+H, +Tail <: Prod] = shapeless.::[H, Tail]

    override def cons[HEAD <: VBound, TAIL <: HList](head: HEAD, tail: TAIL): HEAD ><: TAIL = {

      head :: tail
    }

    override def deCons[HEAD <: VBound, TAIL <: HList](cons: HEAD *: TAIL): (HEAD, TAIL) = {
      cons.head -> cons.tail
    }

    type Builder = shapeless.ProductArgs
    object of extends Builder {

      def applyProduct[L <: TupleX](list: L): L = list
    }

    type Builder_narrow = shapeless.SingletonProductArgs

    object ofNarrow extends Builder_narrow {

      def applyProduct[L <: TupleX](list: L): L = list

    }

    type Mapper = shapeless.Poly1

    implicit class Ops[H <: Prod](hh: H) {

      // https://stackoverflow.com/questions/66036106/can-shapeless-record-type-be-used-as-a-poly1-part-2
      trait GetV extends Hom.Poly {

        implicit def getter[S](
            implicit
            _selector: shapeless.ops.hlist.Selector[H, S]
        ): Case[S, _selector.Out] = at[S] { _ =>
          _selector(hh)
        }
      }
      object GetV extends GetV

      trait GetField extends Hom.Poly {

        implicit def getter[S](
            implicit
            _selector: shapeless.ops.record.Selector[H, S]
        ): Case[S, FieldType[S, _selector.Out]] = at[S] { _ =>
          _selector(hh).asInstanceOf[FieldType[S, _selector.Out]]
        }
      }
      object GetField extends GetField
    }

    /**
      * can convert a [[Prod]] to a flat Scala tuple or Unit or value and back
      *
      * e.g.
      *   - (A, B) <-> A ><: B ><: Empty
      *   - (A) <-> A ><: Empty
      *   - A -> A ><: Empty
      *   - Unit -> Empty
      */
    object ToFlatRepr extends ToFlatRepr_Imp0 {

      implicit lazy val forUnit: Eye /=> Unit = at[Eye](_ => ())

      implicit def forValue[V <: VBound]: (V ><: Eye) /=> Element[V] = at[V ><: Eye] { v =>
        val (head, _) = deCons(v)
        head
      }
    }

    trait ToFlatRepr_Imp0 extends Poly {

      implicit def forTuples[I <: Prod, LN <: HList, O](
          implicit
          hlistToFlat: Tupler.Aux[I, O]
      ): I /=> O = at[I] { i =>
        hlistToFlat(i)
      }
    }

    /**
      * The inverse of [[ToFlatRepr]]
      */
    object FromFlatRepr extends FromFlatRepr_Imp0 {

      implicit val forUnit: Unit /=> Eye = at[Unit](_ => Eye)

      implicit def forProduct[P <: Product, O <: HList](
          implicit
          gen: Generic.Aux[P, O]
      ): P /=> O = at[P] { p =>
        gen.to(p)
      }

    }

    trait FromFlatRepr_Imp0 extends Poly {

      implicit def forValue[V <: VBound]: Element[V] /=> (V ><: Eye) = at[Element[V]] { v =>
        cons(v, Eye)
      }
    }
  }
}
