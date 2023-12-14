package ai.acyclic.prover.commons.util

object Summoner {

  // backported from Scala 3, tighter than scala 2's `implicitly[T]`
  // TODO: all invocation of `implicitly` should be replaced with this
  def summon[T](
      implicit
      x: T
  ): x.type = x

  { // demo
    trait Foo
    object Foo {
      trait Foo1 extends Foo

      implicit def summon: Foo1 = new Foo1 {}
    }

    {
      val foo = summon[Foo]
      foo
    }

    {
      implicitly[Foo]
//      val foo1: Foo.Foo1 = foo // fail to compile
    }
  }
}
