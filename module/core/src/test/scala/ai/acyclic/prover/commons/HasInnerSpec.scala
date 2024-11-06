package ai.acyclic.prover.commons

import ai.acyclic.prover.commons.testlib.BaseSpec

class HasInnerSpec extends BaseSpec {

  import HasInnerSpec._

  it("can summon from path-dependent companion object") {

    val ff = new Family
    val mm = ff.Magnet(1)
    implicitly[TypeCls[mm.type]]
  }

  it(" ... NOT if the path is lost") {

    val ff = new Family
    ff.Magnet(1): Family#Magnet

    shouldNotCompile(
      """implicitly[TypeCls[mm.type]]"""
    )
  }

  it("can summon again if outer path is available") {

    val ff = new Family
    val mm = ff.Magnet(1): Family#Magnet

//    TypeViz.infer(mm.inner).shouldBe()
    implicitly[TypeCls[mm.inner.type]]
  }
}

object HasInnerSpec {

  class TypeCls[T]

  class Family extends HasInner {

    case class Magnet(v: Int) extends Inner {

      type This = outer.Magnet
    }

    object Magnet {

      implicit def getTypeC[T <: Magnet]: TypeCls[T] = new TypeCls[T]
    }
  }

}
