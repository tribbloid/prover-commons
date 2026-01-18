package ai.acyclic.prover.commons.util

import ai.acyclic.prover.commons.testlib.BaseSpec
import shapeless.{Generic, HNil}
import shapeless.ops.hlist.{IsHCons, Tupler}

class TupleUnpackSpec extends BaseSpec {

  def summon[T](v: T)(
      implicit
      unpack: TupleUnpack[T]
  ): unpack.type = unpack

  it("debug generics") {
    val t = (1, "a")
    val gen = Generic[(Int, String)]
    val hlist = gen.to(t)
    Predef.assert(hlist == 1 :: "a" :: HNil)

    val isHCons = IsHCons[gen.Repr]
    Predef.assert(isHCons.head(hlist) == 1)

    val tupler = Tupler[isHCons.T]
    Predef.assert(tupler(isHCons.tail(hlist)) == Tuple1("a"))
  }

  it("unpack (Int, String, Double)") {
    val v = (1, "a", 2.0)
    val unpacked = summon(v)

    implicitly[unpacked.Head =:= Int]
    implicitly[unpacked.Tail =:= (String, Double)]
  }

  it("unpack (Int)") {
    val v = Tuple1(1)
    val unpacked = summon(v)

    implicitly[unpacked.Head =:= Int]
    implicitly[unpacked.Tail =:= Unit]
  }

  it("unpack Int") {
    val v = 1
    val unpacked = summon(v)

    implicitly[unpacked.Head =:= Int]
    implicitly[unpacked.Tail =:= Unit]
  }

  it("unpack (Int, String)") {
    val v = (1, "a")
    val unpacked = summon(v)

    implicitly[unpacked.Head =:= Int]
    implicitly[unpacked.Tail =:= Tuple1[String]]
  }
}
