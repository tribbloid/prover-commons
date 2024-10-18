package ai.acyclic.prover.commons.viz

import ai.acyclic.prover.commons.diff.StringDiff.SuperSet
import ai.acyclic.prover.commons.meta.ScalaReflection.WeakTypeTag
import ai.acyclic.prover.commons.testlib.BaseSpec
import shapeless.{syntax, HNil, Witness}

class TypeVizSpec extends BaseSpec with TypeViz.TestFixtures {

  import TypeVizSpec._

  def infer[T: WeakTypeTag](v: T): String = {

    TypeViz
      .infer(v)
      .toString
  }

  it("trait with generic") {

    val viz = TypeViz[S2K[S1]]

    viz.text_hierarchy.toString.shouldBe(
      """
        |+ ai.acyclic.prover.commons.viz.TypeVizSpec.S2K[ai.acyclic.prover.commons.viz.TypeVizSpec.S1]
        |:       ┏ + ai.acyclic.prover.commons.viz.TypeVizSpec.S2K [ 1 ARG ] :
        |:       ┃ !-+ ai.acyclic.prover.commons.viz.TypeVizSpec.S1
        |:       ┃   !-- Object ... (see [0])
        |!-- Object .......................................................................... [0]
        |""".stripMargin
    )

    viz.text_flow.toString.shouldBe(
      """
        | ┌───────────────────────────────────────────────────────────────────────────────────────────┐
        | │ai.acyclic.prover.commons.viz.TypeVizSpec.S2K[ai.acyclic.prover.commons.viz.TypeVizSpec.S1]│
        | │      ┏ + ai.acyclic.prover.commons.viz.TypeVizSpec.S2K [ 1 ARG ] :                        │
        | │      ┃ !-+ ai.acyclic.prover.commons.viz.TypeVizSpec.S1                                   │
        | │      ┃   !-- Object ... (see [0])                                                         │
        | └─────────────────────────────────────────────┬─────────────────────────────────────────────┘
        |                                               │
        |                                               v
        |                                           ┌──────┐
        |                                           │Object│
        |                                           └──────┘
        |""".stripMargin
    )
  }

  it("trait with self-referencing arg") {

    val viz = TypeViz[S2]

    viz.text_hierarchy.toString.shouldBe(
      """
        |+ ai.acyclic.prover.commons.viz.TypeVizSpec.S2 .................................... [1]
        |!-+ ai.acyclic.prover.commons.viz.TypeVizSpec.S1
        |: !-- Object .......................................................................... [0]
        |!-+ ai.acyclic.prover.commons.viz.TypeVizSpec.S2K[ai.acyclic.prover.commons.viz.TypeVizSpec.S2]
        |  :       ┏ + ai.acyclic.prover.commons.viz.TypeVizSpec.S2K [ 1 ARG ] :
        |  :       ┃ !-- ai.acyclic.prover.commons.viz.TypeVizSpec.S2 ... (see [1])
        |  !-- Object ... (see [0])
        |""".stripMargin
    )

    viz.text_flow.toString.shouldBe(
      """
        |                                               ┌────────────────────────────────────────────┐
        |                                               │ai.acyclic.prover.commons.viz.TypeVizSpec.S2│
        |                                               └──────────────┬──────────────┬──────────────┘
        |                                                              │              │
        |                        ┌─────────────────────────────────────┘              │
        |                        │                                                    v
        |                        v                       ┌───────────────────────────────────────────────────────────────────────────────────────────┐
        | ┌────────────────────────────────────────────┐ │ai.acyclic.prover.commons.viz.TypeVizSpec.S2K[ai.acyclic.prover.commons.viz.TypeVizSpec.S2]│
        | │ai.acyclic.prover.commons.viz.TypeVizSpec.S1│ │      ┏ + ai.acyclic.prover.commons.viz.TypeVizSpec.S2K [ 1 ARG ] :                        │
        | └──────────────────────┬─────────────────────┘ │      ┃ !-- ai.acyclic.prover.commons.viz.TypeVizSpec.S2 ... (see [1])                     │
        |                        │                       └──────────────────────┬────────────────────────────────────────────────────────────────────┘
        |                        └────────────────────────────────────────────┐ │
        |                                                                     │ │
        |                                                                     v v
        |                                                                  ┌──────┐
        |                                                                  │Object│
        |                                                                  └──────┘
        |""".stripMargin
    )

  }

  ignore("String") { // TODO: heavy change between Java 11 to 17, don't know how to define ground truth

    val viz = TypeViz[String]

    viz.text_hierarchy.toString.shouldBe(
      """
        |+ String .......................................................................... [0]
        |!-- Comparable[String]
        |:         ┏ + Comparable [ 1 ARG ] :
        |:         ┃ !-- String ... (see [0])
        |!-- java.io.Serializable
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
        |+ String :: String :: shapeless.HNil
        |:       ┏ + shapeless.:: [ 2 ARGS ] :
        |:       ┃ !-+ String .......................................................................... [3]
        |:       ┃ : !-- Comparable[String]
        |:       ┃ : :         ┏ + Comparable [ 1 ARG ] :
        |:       ┃ : :         ┃ !-- String ... (see [3])
        |:       ┃ : !-- java.io.Serializable ... (see [0])
        |:       ┃ !-+ String :: shapeless.HNil
        |:       ┃   :       ┏ + shapeless.:: [ 2 ARGS ] :
        |:       ┃   :       ┃ !-- String ... (see [3])
        |:       ┃   :       ┃ !-+ shapeless.HNil
        |:       ┃   :       ┃   !-- shapeless.HList ... (see [2])
        |:       ┃   !-- shapeless.HList ... (see [2])
        |!-+ shapeless.HList ................................................................. [2]
        |  !-- java.io.Serializable ............................................................ [0]
        |  !-+ Product
        |  : !-- Equals
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
          |+ String with shapeless.labelled.KeyTag[String("author"),String] :: shapeless.HNil
          |:       ┏ + shapeless.:: [ 2 ARGS ] :
          |:       ┃ !-+ String with shapeless.labelled.KeyTag[String("author"),String]
          |:       ┃ : !-- shapeless.labelled.KeyTag[String("author"),String]
          |:       ┃ : :         ┏ + shapeless.labelled.KeyTag [ 2 ARGS ] :
          |:       ┃ : :         ┃ !-+ String("author")
          |:       ┃ : :         ┃ : !-- String ... (see [3])
          |:       ┃ : :         ┃ !-- String ... (see [3])
          |:       ┃ : !-+ String .......................................................................... [3]
          |""".stripMargin,
        mode = SuperSet
      )
  }

  describe("Singleton") {

    it("global") {

      TypeViz[TypeVizSpec.singleton.type].toString.shouldBe(
        """
          |+ ai.acyclic.prover.commons.viz.TypeVizSpec.singleton.type
          |!-+ Int
          |  !-- AnyVal
          |""".stripMargin
      )
    }

    it("... with Arg") {

      TypeViz[TypeVizSpec.singletonWArg.type].toString.shouldBe(
        """
          |+ ai.acyclic.prover.commons.viz.TypeVizSpec.singletonWArg.type
          |!-+ ai.acyclic.prover.commons.viz.TypeVizSpec.WArg[Int]
          |  :       ┏ + ai.acyclic.prover.commons.viz.TypeVizSpec.WArg [ 1 ARG ] :
          |  :       ┃ !-+ Int
          |  :       ┃   !-- AnyVal
          |  !-- java.io.Serializable
          |  !-+ Product
          |  : !-- Equals
          |  !-- Object
          |""".stripMargin
      )
    }

    it("local") {

      val w = 3

      TypeViz[w.type].toString.shouldBe(
        """
          |+ w.type
          |!-+ Int
          |  !-- AnyVal
          |""".stripMargin
      )
    }

    it("global Witness.T") {

      infer(singletonW.value)
        .shouldBe(
          """
            |+ ai.acyclic.prover.commons.viz.TypeVizSpec.singletonW.T
            |!-+ Int
            |  !-- AnyVal
            |""".stripMargin
        )

      infer[3](adhocW.value)
        .shouldBe(
          """
            |+ Int(3)
            |!-+ Int
            |  !-- AnyVal
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
            |+ shapeless.Witness{type T = Int(3)} ≅ shapeless.Witness.Aux[Int(3)]
            |:       ┏ + shapeless.Witness.Aux [ 1 ARG ] :
            |:       ┃ !-+ Int(3)
            |:       ┃   !-+ Int
            |:       ┃     !-- AnyVal
            |!-+ shapeless.Witness{type T = T0}
            |  !-+ shapeless.Witness
            |    !-- java.io.Serializable
            |    !-- Object
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
            |+ ww.T
            |!-+ Int
            |  !-- AnyVal
            |""".stripMargin
        )

      val wwErased: Witness.Lt[Int] = adhocW

      infer(wwErased)
        .shouldBe(
          """
            |+ shapeless.Witness{type T <: Int} ≅ shapeless.Witness.Lt[Int]
            |:       ┏ + shapeless.Witness.Lt [ 1 ARG ] :
            |:       ┃ !-+ Int
            |:       ┃   !-- AnyVal
            |!-+ shapeless.Witness{type T <: Lub}
            |  !-+ shapeless.Witness
            |    !-- java.io.Serializable
            |    !-- Object
            |""".stripMargin
        )
    }
  }

  it("refined") {

    val v = TypeViz[Witness.Aux[Int]]
    v.toString.shouldBe(
      """
        |+ shapeless.Witness{type T = Int} ≅ shapeless.Witness.Aux[Int]
        |:       ┏ + shapeless.Witness.Aux [ 1 ARG ] :
        |:       ┃ !-+ Int
        |:       ┃   !-- AnyVal
        |!-+ shapeless.Witness{type T = T0}
        |  !-+ shapeless.Witness
        |    !-- java.io.Serializable
        |    !-- Object
        |""".stripMargin
    )
  }

  describe("type alias") {

    it("global") {

      TypeViz[Alias].toString.shouldBe(
        """
          |+ Int(3) ≅ ai.acyclic.prover.commons.viz.TypeVizSpec.Alias
          |!-+ Int
          |  !-- AnyVal
          |""".stripMargin
      )
    }

    describe("generic") {

      it("Default") {
        TypeViz[AliasWArg].toString.shouldBe(
          """
            |+ ai.acyclic.prover.commons.viz.TypeVizSpec.WArg[Double] ≅ ai.acyclic.prover.commons.viz.TypeVizSpec.AliasWArg
            |:       ┏ + ai.acyclic.prover.commons.viz.TypeVizSpec.WArg [ 1 ARG ] :
            |:       ┃ !-+ Double
            |:       ┃   !-- AnyVal
            |!-- java.io.Serializable
            |!-+ Product
            |: !-- Equals
            |!-- Object
            |""".stripMargin
        )
      }

      it("DeAlias") {

        TypeVizDeAlias[AliasWArg].toString.shouldBe(
          """
            |+ ai.acyclic.prover.commons.viz.TypeVizSpec.WArg[Double]
            |:       ┏ + ai.acyclic.prover.commons.viz.TypeVizSpec.WArg [ 1 ARG ] :
            |:       ┃ !-+ Double
            |:       ┃   !-- AnyVal
            |!-- java.io.Serializable
            |!-+ Product
            |: !-- Equals
            |!-- Object
            |""".stripMargin
        )
      }

      it("Short") {

        TypeVizShort[AliasWArg].toString.shouldBe(
          """
            |+ TypeVizSpec.WArg[Double]
            |:       ┏ + ai.acyclic.prover.commons.viz.TypeVizSpec.WArg [ 1 ARG ] :
            |:       ┃ !-+ Double
            |:       ┃   !-- AnyVal
            |!-- Serializable
            |!-+ Product
            |: !-- Equals
            |!-- Object
            |""".stripMargin
        )
      }
    }

    describe("generic of generic") {

      it("Default") {
        TypeViz[AliasWArg2].toString.shouldBe(
          """
            |+ ai.acyclic.prover.commons.viz.TypeVizSpec.WArg[ai.acyclic.prover.commons.viz.TypeVizSpec.WArg[Double]] ≅ ai.acyclic.prover.commons.viz.TypeVizSpec.AliasWArg2
            |:       ┏ + ai.acyclic.prover.commons.viz.TypeVizSpec.WArg [ 1 ARG ] :
            |:       ┃ !-+ ai.acyclic.prover.commons.viz.TypeVizSpec.WArg[Double]
            |:       ┃   :       ┏ + ai.acyclic.prover.commons.viz.TypeVizSpec.WArg [ 1 ARG ] :
            |:       ┃   :       ┃ !-+ Double
            |:       ┃   :       ┃   !-- AnyVal
            |:       ┃   !-- java.io.Serializable ... (see [0])
            |:       ┃   !-- Product ... (see [1])
            |:       ┃   !-- Object ... (see [2])
            |!-- java.io.Serializable ............................................................ [0]
            |!-+ Product ......................................................................... [1]
            |: !-- Equals
            |!-- Object .......................................................................... [2]
            |""".stripMargin
        )
      }

      it("DeAlias") {

        TypeVizDeAlias[AliasWArg2].toString.shouldBe(
          """
            |+ ai.acyclic.prover.commons.viz.TypeVizSpec.WArg[ai.acyclic.prover.commons.viz.TypeVizSpec.WArg[Double]]
            |:       ┏ + ai.acyclic.prover.commons.viz.TypeVizSpec.WArg [ 1 ARG ] :
            |:       ┃ !-+ ai.acyclic.prover.commons.viz.TypeVizSpec.WArg[Double]
            |:       ┃   :       ┏ + ai.acyclic.prover.commons.viz.TypeVizSpec.WArg [ 1 ARG ] :
            |:       ┃   :       ┃ !-+ Double
            |:       ┃   :       ┃   !-- AnyVal
            |:       ┃   !-- java.io.Serializable ... (see [0])
            |:       ┃   !-- Product ... (see [1])
            |:       ┃   !-- Object ... (see [2])
            |!-- java.io.Serializable ............................................................ [0]
            |!-+ Product ......................................................................... [1]
            |: !-- Equals
            |!-- Object .......................................................................... [2]
            |""".stripMargin
        )
      }

      it("Short") {

        TypeVizShort[AliasWArg2].toString.shouldBe(
          """
            |+ TypeVizSpec.WArg[TypeVizSpec.WArg[Double]]
            |:       ┏ + ai.acyclic.prover.commons.viz.TypeVizSpec.WArg [ 1 ARG ] :
            |:       ┃ !-+ TypeVizSpec.WArg[Double]
            |:       ┃   :       ┏ + ai.acyclic.prover.commons.viz.TypeVizSpec.WArg [ 1 ARG ] :
            |:       ┃   :       ┃ !-+ Double
            |:       ┃   :       ┃   !-- AnyVal
            |:       ┃   !-- Serializable ... (see [0])
            |:       ┃   !-- Product ... (see [1])
            |:       ┃   !-- Object ... (see [2])
            |!-- Serializable .................................................................... [0]
            |!-+ Product ......................................................................... [1]
            |: !-- Equals
            |!-- Object .......................................................................... [2]
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
          |+ e.D
          |!-+ Int
          |  !-- AnyVal
          |""".stripMargin
      )
    }

    it("with lower bound") {

      val e = new E { type D >: String <: CharSequence }
      TypeViz[e.D].toString.shouldBe(
        """
          |+ e.D
          |!-+ CharSequence
          |  !-- Object
          |""".stripMargin
      )
    }

    it("with final type") {

      val e = new E { final type D = Int }

      TypeViz[e.D].toString.shouldBe(
        """
          |+ e.D
          |!-+ Int
          |  !-- AnyVal
          |""".stripMargin
      )
    }

    it("with final type in path") {

      TypeViz[EE.D].toString.shouldBe(
        """
          |+ Int ≅ ai.acyclic.prover.commons.viz.TypeVizSpec.EE.D
          |!-- AnyVal
          |""".stripMargin
      )

      val e = new EE
      TypeViz[e.D].toString.shouldBe(
        """
          |+ Int ≅ e.D
          |!-- AnyVal
          |""".stripMargin
      )

    }

  }

  it("this") {

    val e = new EE
    TypeViz[e.THIS].toString.shouldBe(
      """
        |+ e.type ≅ e.THIS
        |!-+ ai.acyclic.prover.commons.viz.TypeVizSpec.EE
        |  !-+ ai.acyclic.prover.commons.viz.TypeVizSpec.E
        |    !-- Object
        |""".stripMargin
    )
  }
}

object TypeVizSpec {

  trait S1

  trait S2K[V]

  trait S2 extends S2K[S2] with S1

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
