package ai.acyclic.prover.commons.compat

import ai.acyclic.prover.commons.jit.Hom.Poly
import ai.acyclic.prover.commons.tuple.Products
import shapeless.labelled.FieldType
import shapeless.ops.hlist.Tupler
import shapeless.tag.@@
import shapeless.HList

trait HasTupleX {

  type TupleX = TupleX.Prod

  type TupleXEmpty = TupleX.Eye
  def TupleXEmpty = TupleX.Eye

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

    type Empty = Eye
    val Empty: Eye = Eye

    type Unital = Eye // "Unit" has special meaning in Scala
    val Unital: Eye = Eye

    override infix type ><:[+H, +Tail <: Prod] = shapeless.::[H, Tail]

    override def cons[HEAD <: VBound, TAIL <: HList](head: HEAD, tail: TAIL): HEAD ><: TAIL = {

      head :: tail
    }

    override def deCons[HEAD <: VBound, TAIL <: HList](cons: HEAD *: TAIL): (HEAD, TAIL) = {
      cons.head -> cons.tail
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
          implicit
          ev: Ops.ToFlatTuple.Lemma[H, O]
      ): O = ev(hh)

      // TODO: current Poly definitions have to be singleton which makes it unusable.
      //  test will be delayed after revision or Scala 3
      // https://stackoverflow.com/questions/66036106/can-shapeless-record-type-be-used-as-a-poly1-part-2
      trait GetValue extends Poly {

        implicit def getter[S](
            implicit
            _selector: shapeless.ops.hlist.Selector[H, S]
        ): Case[S, _selector.Out] = at[S] { _ =>
          _selector(hh)
        }
      }
      object GetValue extends GetValue

      trait GetField extends Poly {

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

  object NamedTupleX {

    type ->>[K, +V] = FieldType[K, V]

    type :=[K, +V] = (Symbol @@ K) ->> V

    type Builder = shapeless.RecordArgs

    object of extends Builder {

      def applyRecord[L <: TupleX](list: L): L = list
    }

    // TODO: impl Builder_narrow
  }
}
