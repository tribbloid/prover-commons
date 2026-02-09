package ai.acyclic.prover.commons.tuple

import ai.acyclic.prover.commons.testlib.BaseSpec
import ai.acyclic.prover.commons.tuple.Tuples
import ai.acyclic.prover.commons.tuple.Tuples.FlatSchema._
import ai.acyclic.prover.commons.tuple.Tuples.{><:, _0, cons}
import shapeless.{::, HNil}

class FlatSchemaSpec extends BaseSpec {

  describe("FlatSchema") {

    it("should handle Unit / Empty") {
      val schema = Tuples.FlatSchema.unitCase
      assert(schema.forward(Tuples.Empty) == ())
      assert(schema.reverse(()) == Tuples.Empty)
      assert(schema.toRuntimeList(Tuples.Empty) == Nil)
    }

    it("should handle single value") {
      val v = 1
      val tuple = cons(v, _0)
      val schema = Tuples.FlatSchema.valueCase[Int]

      assert(schema.forward(tuple) == v)
      assert(schema.reverse(v) == tuple)
      assert(schema.toRuntimeList(tuple) == List(v))
    }

    it("should handle Tuple2") {
      val v1 = 1
      val v2 = "a"
      val tuple = cons(v1, cons(v2, _0))
      val expected = (v1, v2)

      // Explicit check
      val schema = implicitly[Tuples.FlatSchema { type Repr = Int ><: String ><: HNil; type FlatRepr = (Int, String) }]

      assert(schema.forward(tuple) == expected)
      assert(schema.reverse(expected) == tuple)
      assert(schema.toRuntimeList(tuple) == List(v1, v2))
    }

    it("should handle Tuple3") {
      val v1 = 1
      val v2 = "a"
      val v3 = true
      val tuple = cons(v1, cons(v2, cons(v3, _0)))
      val expected = (v1, v2, v3)
      val schema = implicitly[
        Tuples.FlatSchema { type Repr = Int ><: String ><: Boolean ><: HNil; type FlatRepr = (Int, String, Boolean) }
      ]

      assert(schema.forward(tuple) == expected)
      assert(schema.reverse(expected) == tuple)
      assert(schema.toRuntimeList(tuple) == List(v1, v2, v3))
    }

    it("should handle Tuple4") {
      val v1 = 1
      val v2 = "a"
      val v3 = true
      val v4 = 2.0
      val tuple = cons(v1, cons(v2, cons(v3, cons(v4, _0))))
      val expected = (v1, v2, v3, v4)
      val schema = implicitly[
        Tuples.FlatSchema {
          type Repr = Int ><: String ><: Boolean ><: Double ><: HNil; type FlatRepr = (Int, String, Boolean, Double)
        }
      ]

      assert(schema.forward(tuple) == expected)
      assert(schema.reverse(expected) == tuple)
      assert(schema.toRuntimeList(tuple) == List(v1, v2, v3, v4))
    }

    it("should resolve implicits correctly") {
      val t2 = cons(1, cons("a", _0))
      // Verify that we can summon schema by input type alone (if return type is inferred or checked)
      // Note: Without knowing FlatRepr, implicit search might be harder if multiple exist (unlikely here)
      // But we can check if it exists for the specific FlatRepr

      val schema = implicitly[Tuples.FlatSchema { type Repr = Int ><: String ><: HNil }]
      val res2 = schema.forward(t2)
      assert(res2 == (1, "a"))

      val t3 = cons(1, cons("a", cons(true, _0)))
      val schema3 = implicitly[Tuples.FlatSchema { type Repr = Int ><: String ><: Boolean ><: HNil }]
      val res3 = schema3.forward(t3)
      assert(res3 == (1, "a", true))
    }
  }
}
