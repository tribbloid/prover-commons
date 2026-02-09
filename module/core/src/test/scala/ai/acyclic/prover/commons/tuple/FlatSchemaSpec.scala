package ai.acyclic.prover.commons.tuple

import ai.acyclic.prover.commons.testlib.BaseSpec
import ai.acyclic.prover.commons.tuple.Tuples.{_0, cons}

class FlatSchemaSpec extends BaseSpec {

  describe("FlatSchema") {

    it("should handle Unit / Empty") {
      val schema = Tuples.FlatSchema.unitCase
      assert(schema.forward(Tuples.Empty) == ())
      assert(schema.reverse(()) == Tuples.Empty)
      assert(schema.toRuntimeList(Tuples.Empty) == Nil)
      assert(schema.fromRuntimeList(Nil) == Tuples.Empty)
    }

    it("should handle single value") {
      val v = 1
      val tuple = cons(v, _0)
      val schema = Tuples.FlatSchema.valueCase[Int]

      assert(schema.forward(tuple) == v)
      assert(schema.reverse(v) == tuple)
      assert(schema.toRuntimeList(tuple) == List(v))
      assert(schema.fromRuntimeList(List(v)) == tuple)
    }

    it("should handle Tuple2") {
      val v1 = 1
      val v2 = "a"
      val tuple = cons(v1, cons(v2, _0))
      val expected = (v1, v2)
      val schema = Tuples.FlatSchema.tuple2Case[Int, String]

      assert(schema.forward(tuple) == expected)
      assert(schema.reverse(expected) == tuple)
      assert(schema.toRuntimeList(tuple) == List(v1, v2))
      assert(schema.fromRuntimeList(List(v1, v2)) == tuple)
    }

    it("should handle Tuple3") {
      val v1 = 1
      val v2 = "a"
      val v3 = true
      val tuple = cons(v1, cons(v2, cons(v3, _0)))
      val expected = (v1, v2, v3)
      val schema = Tuples.FlatSchema.tuple3Case[Int, String, Boolean]

      assert(schema.forward(tuple) == expected)
      assert(schema.reverse(expected) == tuple)
      assert(schema.toRuntimeList(tuple) == List(v1, v2, v3))
      assert(schema.fromRuntimeList(List(v1, v2, v3)) == tuple)
    }

    it("should handle Tuple4") {
      val v1 = 1
      val v2 = "a"
      val v3 = true
      val v4 = 2.0
      val tuple = cons(v1, cons(v2, cons(v3, cons(v4, _0))))
      val expected = (v1, v2, v3, v4)
      val schema = Tuples.FlatSchema.tuple4Case[Int, String, Boolean, Double]

      assert(schema.forward(tuple) == expected)
      assert(schema.reverse(expected) == tuple)
      assert(schema.toRuntimeList(tuple) == List(v1, v2, v3, v4))
      assert(schema.fromRuntimeList(List(v1, v2, v3, v4)) == tuple)
    }

    it("should resolve implicits correctly") {
      def convert[I <: Tuples.Inductive, F](in: I)(
          implicit
          schema: Tuples.FlatSchema { type Repr = I; type FlatRepr = F }
      ): F = {
        schema.forward(in)
      }

      val t2 = cons(1, cons("a", _0))
      val res2 = convert(t2)
      assert(res2 == (1, "a"))

      val t3 = cons(1, cons("a", cons(true, _0)))
      val res3 = convert(t3)
      assert(res3 == (1, "a", true))
    }
  }
}
