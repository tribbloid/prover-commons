package ai.acyclic.prover.commons.viz

import ai.acyclic.prover.commons.diff.StringDiff.SuperSet
import ai.acyclic.prover.commons.reflect.Reflection.Runtime.WeakTypeTag
import ai.acyclic.prover.commons.testlib.BaseSpec
import shapeless.{syntax, HNil, Witness}

class TypeVizSpec extends BaseSpec with TypeViz.TestFixtures {

  import TypeVizSpec._

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
        | !-- Comparable[String]
        | :         ┏ -+ Comparable [ 1 ARG ] :
        | :         ┃  !-- String .......................................................................... [0]
        | !-- java.io.Serializable
        |""".stripMargin,
      mode = SuperSet
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
        | :       ┃  !-+ String .......................................................................... [3]
        | :       ┃  : !-- Comparable[String]
        | :       ┃  : :         ┏ -+ Comparable [ 1 ARG ] :
        | :       ┃  : :         ┃  !-- String .......................................................................... [3]
        | :       ┃  : !-- java.io.Serializable ............................................................ [0]
        | :       ┃  !-+ String :: shapeless.HNil
        | :       ┃    :       ┏ -+ shapeless.:: [ 2 ARGS ] :
        | :       ┃    :       ┃  !-- String .......................................................................... [3]
        | :       ┃    :       ┃  !-+ shapeless.HNil
        | :       ┃    :       ┃    !-- shapeless.HList ................................................................. [2]
        | :       ┃    !-- shapeless.HList ................................................................. [2]
        | !-+ shapeless.HList ................................................................. [2]
        |   !-+ java.io.Serializable ............................................................ [0]
        |   : !-- Any
        |   !-+ Product
        |   : !-- Equals
        |""".stripMargin,
        mode = SuperSet
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
          | :       ┃  : : :       ┃  : !-- String .......................................................................... [4]
          | :       ┃  : : :       ┃  !-- String .......................................................................... [4]
          |""".stripMargin,
        mode = SuperSet
      )
  }

  describe("Singleton") {

    it("global") {

      TypeViz[TypeVizSpec.singleton.type].toString.shouldBe(
        """
          |-+ ai.acyclic.prover.commons.viz.TypeVizSpec.singleton.type
          | !-+ Int
          |   !-+ AnyVal
          |     !-- Any
          |""".stripMargin
      )
    }

    it("... with Arg") {

      TypeViz[TypeVizSpec.singletonWArg.type].toString.shouldBe(
        """
          |-+ ai.acyclic.prover.commons.viz.TypeVizSpec.singletonWArg.type
          | !-+ ai.acyclic.prover.commons.viz.TypeVizSpec.WArg[Int]
          |   :       ┏ -+ ai.acyclic.prover.commons.viz.TypeVizSpec.WArg [ 1 ARG ] :
          |   :       ┃  !-+ Int
          |   :       ┃    !-+ AnyVal
          |   :       ┃      !-- Any ............................................................................. [0]
          |   !-+ java.io.Serializable
          |   : !-- Any ............................................................................. [0]
          |   !-+ Product
          |   : !-- Equals
          |   !-- Object
          |""".stripMargin
      )
    }

    it("local") {

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

    it("global Witness.T") {

      infer(singletonW.value)
        .shouldBe(
          """
            |-+ ai.acyclic.prover.commons.viz.TypeVizSpec.singletonW.T
            | !-+ Int
            |   !-+ AnyVal
            |     !-- Any
            |""".stripMargin
        )

      infer[3](adhocW.value)
        .shouldBe(
          """
            |-+ Int(3)
            | !-+ Int
            |   !-+ AnyVal
            |     !-- Any
            |""".stripMargin
        )

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

  }

  describe("Witness") {

    it("global") {

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
            | !-+ shapeless.Witness{type T = T0}
            |   !-+ shapeless.Witness
            |     !-+ java.io.Serializable
            |     : !-- Any ............................................................................. [0]
            |     !-- Object
            |""".stripMargin
        )

      infer(singletonW).shouldBe(adhocTree)
    }

    it("local") {

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
            | !-+ shapeless.Witness{type T <: Lub}
            |   !-+ shapeless.Witness
            |     !-+ java.io.Serializable
            |     : !-- Any ............................................................................. [0]
            |     !-- Object
            |""".stripMargin
        )
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
        | !-+ shapeless.Witness{type T = T0}
        |   !-+ shapeless.Witness
        |     !-+ java.io.Serializable
        |     : !-- Any ............................................................................. [0]
        |     !-- Object
        |""".stripMargin
    )
  }

  describe("type alias") {

    it("global") {

      TypeViz[Alias].toString.shouldBe(
        """
          |-+ Int(3) ≅ ai.acyclic.prover.commons.viz.TypeVizSpec.Alias
          | !-+ Int
          |   !-+ AnyVal
          |     !-- Any
          |""".stripMargin
      )
    }

    describe("generic") {

      it("Default") {
        TypeViz[AliasWArg].toString.shouldBe(
          """
            |-+ ai.acyclic.prover.commons.viz.TypeVizSpec.WArg[Double] ≅ ai.acyclic.prover.commons.viz.TypeVizSpec.AliasWArg
            | :       ┏ -+ ai.acyclic.prover.commons.viz.TypeVizSpec.WArg [ 1 ARG ] :
            | :       ┃  !-+ Double
            | :       ┃    !-+ AnyVal
            | :       ┃      !-- Any ............................................................................. [0]
            | !-+ java.io.Serializable
            | : !-- Any ............................................................................. [0]
            | !-+ Product
            | : !-- Equals
            | !-- Object
            |""".stripMargin
        )
      }

      it("DeAlias") {

        TypeVizDeAlias[AliasWArg].toString.shouldBe(
          """
            |-+ ai.acyclic.prover.commons.viz.TypeVizSpec.WArg[Double]
            | :       ┏ -+ ai.acyclic.prover.commons.viz.TypeVizSpec.WArg [ 1 ARG ] :
            | :       ┃  !-+ Double
            | :       ┃    !-+ AnyVal
            | :       ┃      !-- Any ............................................................................. [0]
            | !-+ java.io.Serializable
            | : !-- Any ............................................................................. [0]
            | !-+ Product
            | : !-- Equals
            | !-- Object
            |""".stripMargin
        )
      }

      it("Short") {

        TypeVizShort[AliasWArg].toString.shouldBe(
          """
            |-+ TypeVizSpec.WArg[Double]
            | :       ┏ -+ ai.acyclic.prover.commons.viz.TypeVizSpec.WArg [ 1 ARG ] :
            | :       ┃  !-+ Double
            | :       ┃    !-+ AnyVal
            | :       ┃      !-- Any ............................................................................. [0]
            | !-+ Serializable
            | : !-- Any ............................................................................. [0]
            | !-+ Product
            | : !-- Equals
            | !-- Object
            |""".stripMargin
        )
      }
    }

    describe("generic of generic") {

      it("Default") {
        TypeViz[AliasWArg2].toString.shouldBe(
          """
            |-+ ai.acyclic.prover.commons.viz.TypeVizSpec.WArg[ai.acyclic.prover.commons.viz.TypeVizSpec.WArg[Double]] ≅ ai.acyclic.prover.commons.viz.TypeVizSpec.AliasWArg2
            | :       ┏ -+ ai.acyclic.prover.commons.viz.TypeVizSpec.WArg [ 1 ARG ] :
            | :       ┃  !-+ ai.acyclic.prover.commons.viz.TypeVizSpec.WArg[Double]
            | :       ┃    :       ┏ -+ ai.acyclic.prover.commons.viz.TypeVizSpec.WArg [ 1 ARG ] :
            | :       ┃    :       ┃  !-+ Double
            | :       ┃    :       ┃    !-+ AnyVal
            | :       ┃    :       ┃      !-- Any ............................................................................. [3]
            | :       ┃    !-- java.io.Serializable ............................................................ [0]
            | :       ┃    !-- Product ......................................................................... [1]
            | :       ┃    !-- Object .......................................................................... [2]
            | !-+ java.io.Serializable ............................................................ [0]
            | : !-- Any ............................................................................. [3]
            | !-+ Product ......................................................................... [1]
            | : !-- Equals
            | !-- Object .......................................................................... [2]
            |""".stripMargin
        )
      }

      it("DeAlias") {

        TypeVizDeAlias[AliasWArg2].toString.shouldBe(
          """
            |-+ ai.acyclic.prover.commons.viz.TypeVizSpec.WArg[ai.acyclic.prover.commons.viz.TypeVizSpec.WArg[Double]]
            | :       ┏ -+ ai.acyclic.prover.commons.viz.TypeVizSpec.WArg [ 1 ARG ] :
            | :       ┃  !-+ ai.acyclic.prover.commons.viz.TypeVizSpec.WArg[Double]
            | :       ┃    :       ┏ -+ ai.acyclic.prover.commons.viz.TypeVizSpec.WArg [ 1 ARG ] :
            | :       ┃    :       ┃  !-+ Double
            | :       ┃    :       ┃    !-+ AnyVal
            | :       ┃    :       ┃      !-- Any ............................................................................. [3]
            | :       ┃    !-- java.io.Serializable ............................................................ [0]
            | :       ┃    !-- Product ......................................................................... [1]
            | :       ┃    !-- Object .......................................................................... [2]
            | !-+ java.io.Serializable ............................................................ [0]
            | : !-- Any ............................................................................. [3]
            | !-+ Product ......................................................................... [1]
            | : !-- Equals
            | !-- Object .......................................................................... [2]
            |""".stripMargin
        )
      }

      it("Short") {

        TypeVizShort[AliasWArg2].toString.shouldBe(
          """
            |-+ TypeVizSpec.WArg[TypeVizSpec.WArg[Double]]
            | :       ┏ -+ ai.acyclic.prover.commons.viz.TypeVizSpec.WArg [ 1 ARG ] :
            | :       ┃  !-+ TypeVizSpec.WArg[Double]
            | :       ┃    :       ┏ -+ ai.acyclic.prover.commons.viz.TypeVizSpec.WArg [ 1 ARG ] :
            | :       ┃    :       ┃  !-+ Double
            | :       ┃    :       ┃    !-+ AnyVal
            | :       ┃    :       ┃      !-- Any ............................................................................. [3]
            | :       ┃    !-- Serializable .................................................................... [0]
            | :       ┃    !-- Product ......................................................................... [1]
            | :       ┃    !-- Object .......................................................................... [2]
            | !-+ Serializable .................................................................... [0]
            | : !-- Any ............................................................................. [3]
            | !-+ Product ......................................................................... [1]
            | : !-- Equals
            | !-- Object .......................................................................... [2]
            |""".stripMargin
        )
      }
    }
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
          |-+ Int ≅ ai.acyclic.prover.commons.viz.TypeVizSpec.EE.D
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

  it("this") {

    val e = new EE
    TypeViz[e.THIS].toString.shouldBe(
      """
        |-+ e.type ≅ e.THIS
        | !-+ ai.acyclic.prover.commons.viz.TypeVizSpec.EE
        |   !-+ ai.acyclic.prover.commons.viz.TypeVizSpec.E
        |     !-+ Object
        |       !-- Any
        |""".stripMargin
    )
  }
}

object TypeVizSpec {

  case class WArg[T](v: T)

  val singleton = 3

  def adhocW = Witness(3)

  val singletonW = Witness(3)

  val singletonWArg = WArg(2)

  val singletonWArg2 = WArg(WArg(2))

  type Alias = 3

  type AliasWArg = WArg[Double]

  type AliasWArg2 = WArg[WArg[Double]]

  class E { type D }

  class EE extends E {

    final type D = Int

    final type THIS = this.type
  }

  object EE extends EE

}
