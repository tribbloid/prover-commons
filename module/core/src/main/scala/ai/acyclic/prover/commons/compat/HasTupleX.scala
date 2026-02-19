package ai.acyclic.prover.commons.compat

import ai.acyclic.prover.commons.jit.hom.Hom
import ai.acyclic.prover.commons.jit.hom.Hom.Poly
import ai.acyclic.prover.commons.tuple.Products
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

    trait VarArgsConstructor {

      def applyProduct[L <: TupleX](list: L): L = list
    }

    object of extends VarArgsConstructor with shapeless.ProductArgs {}

    object ofNarrow extends VarArgsConstructor with shapeless.SingletonProductArgs {}

    /**
      * The inverse of [[Ops.ToFlatTuple]]
      */
    object FromProductOrValue extends FromProductOrValue_Imp0 {

      implicit val _unit: Unit /=> Eye = at[Unit](_ => Eye)

      implicit def _product[P <: Product, O <: HList](
          implicit
          gen: Generic.Aux[P, O]
      ): P /=> O = at[P] { p =>
        gen.to(p)
      }

    }

    protected trait FromProductOrValue_Imp0 extends Poly {

      implicit def _value[V]: Element[V] /=> (V ><: Eye) = at[Element[V]] { v =>
        cons(v, Eye)
      }
    }
//    type Mapper = shapeless.Poly1

    implicit class Ops[H <: Prod](hh: H) {

      /**
        * can convert a [[Prod]] to a flat Scala tuple or Unit or value and back
        *
        * e.g.
        *   - (A, B) <-> A ><: B ><: Empty
        *   - (A) <-> A ><: Empty
        *   - A -> A ><: Empty
        *   - Unit -> Empty
        */
      def flatTuple[O](
          ev: Ops.ToFlatTuple.Lemma[H, O]
      ): O = ev(hh)

      // https://stackoverflow.com/questions/66036106/can-shapeless-record-type-be-used-as-a-poly1-part-2
      trait GetValue extends Hom.Poly {

        implicit def getter[S](
            implicit
            _selector: shapeless.ops.hlist.Selector[H, S]
        ): Case[S, _selector.Out] = at[S] { _ =>
          _selector(hh)
        }
      }
      object GetValue extends GetValue

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

    object Ops {

      object ToFlatTuple extends ToFlatTuple_Imp0 { // it should be removed in Scala 3

        implicit lazy val forUnit: Eye /=> Unit = at[Eye](_ => ())

        implicit def forValue[V <: VBound]: (V ><: Eye) /=> Element[V] = at[V ><: Eye] { v =>
          val (head, _) = deCons(v)
          head
        }
      }

      protected trait ToFlatTuple_Imp0 extends Poly {

        implicit def forTuples[I <: Prod, LN <: HList, O](
            implicit
            hlistToFlat: Tupler.Aux[I, O]
        ): I /=> O = at[I] { i =>
          hlistToFlat(i)
        }
      }
    }

  }
}
