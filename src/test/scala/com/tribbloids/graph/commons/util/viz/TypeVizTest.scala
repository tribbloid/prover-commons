package com.tribbloids.graph.commons.util.viz

import com.tribbloids.graph.commons.testlib.BaseSpec
import com.tribbloids.graph.commons.util.reflect.ScalaReflection.WeakTypeTag
import shapeless.{syntax, HNil, Witness}

class TypeVizTest extends BaseSpec {

  import TypeVizTest._

  def infer[T: WeakTypeTag](v: T): String = {

    TypeViz
      .infer(v)
      .toString
//      .split('\n')
//      .filterNot { line =>
//        line.contains("+ Serializable")
//      }
//      .mkString("\n")
  }

  it("String") {

    TypeViz[String].toString.shouldBe(
      """
        |-+ String .......................................................................... [0]
        | !-+ CharSequence
        | : !-+ Object
        | :   !-- Any
        | !-- Comparable[String]
        | :         ┏ -+ Comparable [ 1 ARG ] :
        | :         ┃  !-- String .......................................................................... [0]
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
        | :       ┏ -+ shapeless.:: [ 2 ARGS ] :
        | :       ┃  !-+ String .......................................................................... [0]
        | :       ┃  : !-+ CharSequence
        | :       ┃  : : !-- Object .......................................................................... [1]
        | :       ┃  : !-- Comparable[String]
        | :       ┃  : :         ┏ -+ Comparable [ 1 ARG ] :
        | :       ┃  : :         ┃  !-- String .......................................................................... [0]
        | :       ┃  : !-- java.io.Serializable ............................................................ [2]
        | :       ┃  !-+ String :: shapeless.HNil
        | :       ┃    :       ┏ -+ shapeless.:: [ 2 ARGS ] :
        | :       ┃    :       ┃  !-- String .......................................................................... [0]
        | :       ┃    :       ┃  !-+ shapeless.HNil
        | :       ┃    :       ┃    !-- shapeless.HList ................................................................. [3]
        | :       ┃    !-- shapeless.HList ................................................................. [3]
        | !-+ shapeless.HList ................................................................. [3]
        |   !-+ java.io.Serializable ............................................................ [2]
        |   : !-- Any
        |   !-+ Product
        |   : !-- Equals
        |   !-- Object .......................................................................... [1]
        |""".stripMargin.trim
      )
  }

  it("record") {
    import syntax.singleton._

    val book =
      ("author" ->> "Benjamin Pierce") ::
        HNil

    infer(book)
      .shouldBe(
        """
          |-+ String with shapeless.labelled.KeyTag[String("author"),String] :: shapeless.HNil
          | :       ┏ -+ shapeless.:: [ 2 ARGS ] :
          | :       ┃  !-+ String with shapeless.labelled.KeyTag[String("author"),String]
          | :       ┃  : !-+ shapeless.labelled.KeyTag[String("author"),String]
          | :       ┃  : : :       ┏ -+ shapeless.labelled.KeyTag [ 2 ARGS ] :
          | :       ┃  : : :       ┃  !-+ String("author")
          | :       ┃  : : :       ┃  : !-- String .......................................................................... [0]
          | :       ┃  : : :       ┃  !-- String .......................................................................... [0]
          | :       ┃  : : !-- Object .......................................................................... [1]
          | :       ┃  : !-+ String .......................................................................... [0]
          | :       ┃  :   !-- CharSequence
          | :       ┃  :   !-- Comparable[String]
          | :       ┃  :   !-- java.io.Serializable ............................................................ [2]
          | :       ┃  !-+ shapeless.HNil
          | :       ┃    !-- shapeless.HList ................................................................. [3]
          | !-+ shapeless.HList ................................................................. [3]
          |   !-+ java.io.Serializable ............................................................ [2]
          |   : !-- Any
          |   !-+ Product
          |   : !-- Equals
          |   !-- Object .......................................................................... [1]
          |""".stripMargin.trim
      )
  }

  describe("Singleton") {

    it("type") {

      TypeViz[TypeVizTest.singleton.type].toString.shouldBe(
        """
          |-+ com.tribbloids.graph.commons.util.viz.VizTypeTest.singleton.type
          | !-+ Int
          |   !-+ AnyVal
          |     !-- Any
          |""".stripMargin
      )
    }

    it("local type") {

      val w = 3

      TypeViz[w.type].toString.shouldBe(
        """
          |-+ w.type
          | !-+ Int
          |   !-+ AnyVal
          |     !-- Any
          |""".stripMargin
      )
    }

    it("Witness.T") {

      infer(singletonW.value)
        .shouldBe(
          """
            |-+ com.tribbloids.graph.commons.util.viz.VizTypeTest.singletonW.T
            | !-+ Int
            |   !-+ AnyVal
            |     !-- Any
            |""".stripMargin
        )

      infer(adhocW.value)
        .shouldBe(
          """
            |-+ Int(3)
            | !-+ Int
            |   !-+ AnyVal
            |     !-- Any
            |""".stripMargin
        )

    }

    it("Witness type") {

      val adhocTree = infer(adhocW)
      adhocTree
        .shouldBe(
          """
            |-+ shapeless.Witness{type T = Int(3)} ≅ shapeless.Witness.Aux[Int(3)]
            | :       ┏ -+ shapeless.Witness.Aux [ 1 ARG ] :
            | :       ┃  !-+ Int(3)
            | :       ┃    !-+ Int
            | :       ┃      !-+ AnyVal
            | :       ┃        !-- Any ............................................................................. [0]
            | !-- <notype> ⁇ <refinement of shapeless.Witness>
            | !-+ shapeless.Witness
            |   !-+ java.io.Serializable
            |   : !-- Any ............................................................................. [0]
            |   !-- Object
            |""".stripMargin.trim
        )

      infer(singletonW).shouldBe(adhocTree)
    }

    it("local Witness.T") {

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

    it("local Witness type") {

      val ww = adhocW

      // CAUTION: copying witness into a new variable will lose its type information
      infer(ww.value)
        .shouldBe(
          """
            |-+ ww.T
            | !-+ Int
            |   !-+ AnyVal
            |     !-- Any
            |""".stripMargin
        )

      val wwErased: Witness.Lt[Int] = adhocW

      infer(wwErased)
        .shouldBe(
          """
            |-+ shapeless.Witness{type T <: Int} ≅ shapeless.Witness.Lt[Int]
            | :       ┏ -+ shapeless.Witness.Lt [ 1 ARG ] :
            | :       ┃  !-+ Int
            | :       ┃    !-+ AnyVal
            | :       ┃      !-- Any ............................................................................. [0]
            | !-- <notype> ⁇ <refinement of shapeless.Witness>
            | !-+ shapeless.Witness
            |   !-+ java.io.Serializable
            |   : !-- Any ............................................................................. [0]
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

  it("refined") {

    val v = TypeViz[Witness.Aux[Int]]
    v.toString.shouldBe(
      """
        |-+ shapeless.Witness{type T = Int} ≅ shapeless.Witness.Aux[Int]
        | :       ┏ -+ shapeless.Witness.Aux [ 1 ARG ] :
        | :       ┃  !-+ Int
        | :       ┃    !-+ AnyVal
        | :       ┃      !-- Any ............................................................................. [0]
        | !-- <notype> ⁇ <refinement of shapeless.Witness>
        | !-+ shapeless.Witness
        |   !-+ java.io.Serializable
        |   : !-- Any ............................................................................. [0]
        |   !-- Object
        |""".stripMargin
    )
  }

  describe("path-dependent") {

    it("with upper bound") {

      val e = new E { type D <: Int }
      TypeViz[e.D].toString.shouldBe(
        """
          |-+ e.D
          | !-+ Int
          |   !-+ AnyVal
          |     !-- Any
          |""".stripMargin
      )
    }

    it("with lower bound") {

      val e = new E { type D >: String <: CharSequence }
      TypeViz[e.D].toString.shouldBe(
        """
          |-+ e.D
          | !-+ CharSequence
          |   !-+ Object
          |     !-- Any
          |""".stripMargin
      )
    }

    it("with final type") {

      val e = new E { final type D = Int }

      TypeViz[e.D].toString.shouldBe(
        """
          |-+ e.D
          | !-+ Int
          |   !-+ AnyVal
          |     !-- Any
          |""".stripMargin
      )
    }

    it("with final type in path") {

      TypeViz[EE.D].toString.shouldBe(
        """
          |-+ Int ≅ com.tribbloids.graph.commons.util.viz.VizTypeTest.EE.D
          | !-+ AnyVal
          |   !-- Any
          |""".stripMargin
      )

      val e = new EE
      TypeViz[e.D].toString.shouldBe(
        """
          |-+ Int ≅ e.D
          | !-+ AnyVal
          |   !-- Any
          |""".stripMargin
      )

    }

  }
}

object TypeVizTest {

  val singleton = 3

  def adhocW = Witness(3)

  val singletonW = Witness(3)

  class E { type D }

  class EE extends E {

    final type D = Int
  }

  object EE extends EE
}
