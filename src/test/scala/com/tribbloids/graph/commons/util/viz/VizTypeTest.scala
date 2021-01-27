package com.tribbloids.graph.commons.util.viz

import com.tribbloids.graph.commons.testlib.BaseSpec
import com.tribbloids.graph.commons.util.ScalaReflection.WeakTypeTag
import shapeless.{syntax, HNil, Witness}

class VizTypeTest extends BaseSpec {

  import VizTypeTest._

  def infer[T: WeakTypeTag](v: T): String = {

    VizType
      .infer(v)
      .toString
//      .split('\n')
//      .filterNot { line =>
//        line.contains("+ Serializable")
//      }
//      .mkString("\n")
  }

  it("String") {

    VizType[String].toString.shouldBe(
      """
        |-+ String .................................................................................................................. [0]
        | !-+ CharSequence
        | : !-+ Object
        | :   !-- Any
        | !-- Comparable[String]
        | :         `-+ [ 1 ARG ] :
        | :           !-- String .................................................................................................................. [0]
        | !-- java.io.Serializable
        |""".stripMargin.trim
    )
  }

  it("HList") {

    val book =
      "author" ::
        "title" ::
        HNil

    infer(book)
      .shouldBe(
        """
        |-+ String :: String :: shapeless.HNil
        | :       `-+ [ 2 ARGS ] :
        | :         !-+ String .................................................................................................................. [0]
        | :         : !-+ CharSequence
        | :         : : !-- Object .................................................................................................................. [1]
        | :         : !-- Comparable[String]
        | :         : :         `-+ [ 1 ARG ] :
        | :         : :           !-- String .................................................................................................................. [0]
        | :         : !-- java.io.Serializable .................................................................................................... [2]
        | :         !-+ String :: shapeless.HNil
        | :           :       `-+ [ 2 ARGS ] :
        | :           :         !-- String .................................................................................................................. [0]
        | :           :         !-+ shapeless.HNil
        | :           :           !-- shapeless.HList ......................................................................................................... [3]
        | :           !-- shapeless.HList ......................................................................................................... [3]
        | !-+ shapeless.HList ......................................................................................................... [3]
        |   !-+ java.io.Serializable .................................................................................................... [2]
        |   : !-- Any
        |   !-+ Product
        |   : !-- Equals
        |   !-- Object .................................................................................................................. [1]
        |""".stripMargin.trim
      )
  }

  it("record") {
    import syntax.singleton._

    val book =
      ("author" ->> "Benjamin Pierce") ::
        ("price" ->> 44.11) ::
        HNil

    infer(book)
      .shouldBe(
        """
          |-+ String with shapeless.labelled.KeyTag[String("author"),String] :: Double with shapeless.labelled.KeyTag[String("price"),Double] :: shapeless.HNil
          | :       `-+ [ 2 ARGS ] :
          | :         !-+ String with shapeless.labelled.KeyTag[String("author"),String]
          | :         : !-+ shapeless.labelled.KeyTag[String("author"),String]
          | :         : : :       `-+ [ 2 ARGS ] :
          | :         : : :         !-+ String("author")
          | :         : : :         : !-- String .................................................................................................................. [0]
          | :         : : :         !-- String .................................................................................................................. [0]
          | :         : : !-- Object .................................................................................................................. [1]
          | :         : !-+ String .................................................................................................................. [0]
          | :         :   !-- CharSequence
          | :         :   !-- Comparable[String]
          | :         :   !-- java.io.Serializable .................................................................................................... [2]
          | :         !-+ Double with shapeless.labelled.KeyTag[String("price"),Double] :: shapeless.HNil
          | :           :       `-+ [ 2 ARGS ] :
          | :           :         !-+ Double with shapeless.labelled.KeyTag[String("price"),Double]
          | :           :         : !-+ shapeless.labelled.KeyTag[String("price"),Double]
          | :           :         : : :       `-+ [ 2 ARGS ] :
          | :           :         : : :         !-+ String("price")
          | :           :         : : :         : !-- String .................................................................................................................. [0]
          | :           :         : : :         !-- Double .................................................................................................................. [3]
          | :           :         : : !-- Object .................................................................................................................. [1]
          | :           :         : !-+ Double .................................................................................................................. [3]
          | :           :         :   !-- AnyVal
          | :           :         !-+ shapeless.HNil
          | :           :           !-- shapeless.HList ......................................................................................................... [4]
          | :           !-- shapeless.HList ......................................................................................................... [4]
          | !-+ shapeless.HList ......................................................................................................... [4]
          |   !-+ java.io.Serializable .................................................................................................... [2]
          |   : !-- Any
          |   !-+ Product
          |   : !-- Equals
          |   !-- Object .................................................................................................................. [1]
          |""".stripMargin.trim
      )
  }

  describe("Singleton") {

    it("type") {
      infer(adhocW.value)
        .shouldBe(
          """
            |-+ Int(3)
            | !-+ Int
            |   !-+ AnyVal
            |     !-- Any
            |""".stripMargin
        )

      infer(singletonW.value)
        .shouldBe(
          """
            |-+ com.tribbloids.graph.commons.util.viz.VizTypeTest.singletonW.T
            | !-+ Int
            |   !-+ AnyVal
            |     !-- Any
            |""".stripMargin
        )
    }

    it("witness type") {

      val adhocTree = infer(adhocW)
      adhocTree
        .shouldBe(
          """
            |-+ shapeless.Witness.Aux[Int(3)]
            | :       `-+ [ 1 ARG ] :
            | :         !-+ Int(3)
            | :           !-+ Int
            | :             !-+ AnyVal
            | :               !-- Any ..................................................................................................................... [0]
            | !-- <notype> *** <refinement of shapeless.Witness>
            | !-+ shapeless.Witness
            |   !-+ java.io.Serializable
            |   : !-- Any ..................................................................................................................... [0]
            |   !-- Object
            |""".stripMargin.trim
        )

      infer(singletonW).shouldBe(adhocTree)
    }

    it("local variable type") {

      {
        val vv = adhocW.value

        infer(vv)
          .shouldBe(
            infer(adhocW.value)
          )
      }

      {
        val vv = singletonW.value

        infer(vv)
          .shouldBe(
            infer(singletonW.value)
          )
      }

    }

    it("local witness type") {

      val ww: Witness.Lt[Int] = adhocW

      infer(ww)
        .shouldBe(
          """
            |-+ shapeless.Witness.Lt[Int]
            | :       `-+ [ 1 ARG ] :
            | :         !-+ Int
            | :           !-+ AnyVal
            | :             !-- Any ..................................................................................................................... [0]
            | !-- <notype> *** <refinement of shapeless.Witness>
            | !-+ shapeless.Witness
            |   !-+ java.io.Serializable
            |   : !-- Any ..................................................................................................................... [0]
            |   !-- Object
            |""".stripMargin
        )

      // TODO: compilation error! Why?
//      val vv = ww.value
//      infer(vv)
//        .shouldBe(
//          infer(adhocW.value)
//        )
    }
  }
}

object VizTypeTest {

  def adhocW = Witness(3)

  val singletonW = Witness(3)
}
