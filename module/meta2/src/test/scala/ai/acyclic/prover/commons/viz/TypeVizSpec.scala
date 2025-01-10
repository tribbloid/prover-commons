package ai.acyclic.prover.commons.viz

import ai.acyclic.prover.commons.meta.ScalaReflection.WeakTypeTag
import ai.acyclic.prover.commons.testlib.BaseSpec
import shapeless.{syntax, HNil, Witness}

class TypeVizSpec extends BaseSpec with TypeViz.TestFixtures {

  import TypeVizSpec.*

  def infer[T: WeakTypeTag](v: T): String = {

    TypeViz
      .infer(v)
      .toString
  }

  it("trait with generic") {

    val viz = TypeViz[S2K[S1]]

    viz
      .text_linkedHierarchy()
      .text
      .shouldBe(
        """
        |+ ai.acyclic.prover.commons.viz.TypeVizSpec.S2K[ai.acyclic.prover.commons.viz.TypeVizSpec.S1]
        |:       ┏ + ai.acyclic.prover.commons.viz.TypeVizSpec.S2K [ 1 ARG ] :
        |:       ┃ !-+ ai.acyclic.prover.commons.viz.TypeVizSpec.S1
        |:       ┃   !-- Object ... (see [0])
        |!-- Object .......................................................................... [0]
        |""".stripMargin
      )

    viz
      .text_flow()
      .toString
      .shouldBe(
        """
        | ┌───────────────────────────────────────────────────────────────────────────────────────────┐
        | │ai.acyclic.prover.commons.viz.TypeVizSpec.S2K[ai.acyclic.prover.commons.viz.TypeVizSpec.S1]│
        | │      ┏ + ai.acyclic.prover.commons.viz.TypeVizSpec.S2K [ 1 ARG ] :                        │
        | │      ┃ !-+ ai.acyclic.prover.commons.viz.TypeVizSpec.S1                                   │
        | │      ┃   !-- Object                                                                       │
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

    viz
      .text_linkedHierarchy()
      .toString
      .shouldBe(
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

    viz
      .text_flow()
      .toString
      .shouldBe(
        """
        |                                                      ┌────────────────────────────────────────────┐
        |                                                      │ai.acyclic.prover.commons.viz.TypeVizSpec.S2│
        |                                                      └──────────────┬──────────────┬──────────────┘
        |                                                                     │              │
        |                        ┌────────────────────────────────────────────┘              │
        |                        │                                                           v
        |                        │                       ┌─────────────────────────────────────────────────────────────────────────────────────────────────────────┐
        |                        │                       │ai.acyclic.prover.commons.viz.TypeVizSpec.S2K[ai.acyclic.prover.commons.viz.TypeVizSpec.S2]              │
        |                        │                       │      ┏ + ai.acyclic.prover.commons.viz.TypeVizSpec.S2K [ 1 ARG ] :                                      │
        |                        v                       │      ┃ !-+ ai.acyclic.prover.commons.viz.TypeVizSpec.S2 .................................... [1]        │
        | ┌────────────────────────────────────────────┐ │      ┃   !-+ ai.acyclic.prover.commons.viz.TypeVizSpec.S1                                               │
        | │ai.acyclic.prover.commons.viz.TypeVizSpec.S1│ │      ┃   : !-- Object .......................................................................... [0]    │
        | └──────────────────────┬─────────────────────┘ │      ┃   !-+ ai.acyclic.prover.commons.viz.TypeVizSpec.S2K[ai.acyclic.prover.commons.viz.TypeVizSpec.S2]│
        |                        │                       │      ┃     :       ┏ + ai.acyclic.prover.commons.viz.TypeVizSpec.S2K [ 1 ARG ] :                        │
        |                        │                       │      ┃     :       ┃ !-- ai.acyclic.prover.commons.viz.TypeVizSpec.S2 ... (see [1])                     │
        |                        │                       │      ┃     !-- Object ... (see [0])                                                                     │
        |                        │                       └─────────────────────────────┬───────────────────────────────────────────────────────────────────────────┘
        |                        └───────────────────────────────────────────────────┐ │
        |                                                                            │ │
        |                                                                            v v
        |                                                                         ┌──────┐
        |                                                                         │Object│
        |                                                                         └──────┘
        |""".stripMargin
      )

  }

  it("String") {

    val viz = TypeViz[String]

    viz
      .text_linkedHierarchy()
      .toString
      .shouldBe(
        """
        |- String
        |""".stripMargin
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
        |:       ┃ !-- String .......................................................................... [1]
        |:       ┃ !-+ String :: shapeless.HNil
        |:       ┃   :       ┏ + shapeless.:: [ 2 ARGS ] :
        |:       ┃   :       ┃ !-- String ... (see [1])
        |:       ┃   :       ┃ !-+ shapeless.HNil
        |:       ┃   :       ┃   !-- shapeless.HList ... (see [0])
        |:       ┃   !-- shapeless.HList ... (see [0])
        |!-+ shapeless.HList ................................................................. [0]
        |  !-- java.io.Serializable
        |  !-- Product
        |  !-- Object
        |""".stripMargin
      )
  }

  it("record") {
    import syntax.singleton.*

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
          |:       ┃ : :         ┃ !-- String("author")
          |:       ┃ : :         ┃ !-- String ... (see [1])
          |:       ┃ : !-- String .......................................................................... [1]
          |:       ┃ !-+ shapeless.HNil
          |:       ┃   !-- shapeless.HList ... (see [0])
          |!-+ shapeless.HList ................................................................. [0]
          |  !-- java.io.Serializable
          |  !-- Product
          |  !-- Object
          |""".stripMargin
      )
  }

  describe("Singleton") {

    it("global") {

      TypeViz[TypeVizSpec.singleton.type].toString.shouldBe(
        """
          |+ ai.acyclic.prover.commons.viz.TypeVizSpec.singleton.type
          |!-- Int
          |""".stripMargin
      )
    }

    it("... with Arg") {

      TypeViz[TypeVizSpec.singletonWArg.type].toString.shouldBe(
        """
          |+ ai.acyclic.prover.commons.viz.TypeVizSpec.singletonWArg.type
          |!-+ ai.acyclic.prover.commons.viz.TypeVizSpec.WArg[Int]
          |  :       ┏ + ai.acyclic.prover.commons.viz.TypeVizSpec.WArg [ 1 ARG ] :
          |  :       ┃ !-- Int
          |  !-- java.io.Serializable
          |  !-- Product
          |  !-- Object
          |""".stripMargin
      )
    }

    it("local") {

      val w = 3

      TypeViz[w.type].toString.shouldBe(
        """
          |+ w.type
          |!-- Int
          |""".stripMargin
      )
    }

    describe("Witness.T") {

      it("global") {

        infer(singletonW.value)
          .shouldBe(
            """
              |+ ai.acyclic.prover.commons.viz.TypeVizSpec.singletonW.T
              |!-- Int
              |""".stripMargin
          )

        infer[3](adhocW.value)
          .shouldBe(
            """
              |- Int(3)
              |""".stripMargin
          )
      }

      it("local") {

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

    it("case class constructor") {

      infer(W)
        .shouldBe(
          """
            |+ ai.acyclic.prover.commons.viz.TypeVizSpec.W.type
            |!-+ ai.acyclic.prover.commons.viz.TypeVizSpec.W.type
            |  !-- java.io.Serializable
            |  !-- Object
            |""".stripMargin
        )
    }

    it("... with args") {

      infer(WArg)
        .shouldBe(
          """
            |+ ai.acyclic.prover.commons.viz.TypeVizSpec.WArg.type
            |!-+ ai.acyclic.prover.commons.viz.TypeVizSpec.WArg.type
            |  !-- java.io.Serializable
            |  !-- Object
            |""".stripMargin
        )
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
            |:       ┃ !-- Int(3)
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
            |!-- Int
            |""".stripMargin
        )

      val wwErased: Witness.Lt[Int] = adhocW

      infer(wwErased)
        .shouldBe(
          """
            |+ shapeless.Witness{type T <: Int} ≅ shapeless.Witness.Lt[Int]
            |:       ┏ + shapeless.Witness.Lt [ 1 ARG ] :
            |:       ┃ !-- Int
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
        |:       ┃ !-- Int
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
          |- Int(3) ≅ ai.acyclic.prover.commons.viz.TypeVizSpec.Alias
          |""".stripMargin
      )
    }

    describe("generic") {

      it("Default") {
        TypeViz[AliasWArg].toString.shouldBe(
          """
            |+ ai.acyclic.prover.commons.viz.TypeVizSpec.WArg[Double] ≅ ai.acyclic.prover.commons.viz.TypeVizSpec.AliasWArg
            |:       ┏ + ai.acyclic.prover.commons.viz.TypeVizSpec.WArg [ 1 ARG ] :
            |:       ┃ !-- Double
            |!-- java.io.Serializable
            |!-- Product
            |!-- Object
            |""".stripMargin
        )
      }

      it("DeAlias") {

        TypeVizDeAlias[AliasWArg].toString.shouldBe(
          """
            |+ ai.acyclic.prover.commons.viz.TypeVizSpec.WArg[Double]
            |:       ┏ + ai.acyclic.prover.commons.viz.TypeVizSpec.WArg [ 1 ARG ] :
            |:       ┃ !-- Double
            |!-- java.io.Serializable
            |!-- Product
            |!-- Object
            |""".stripMargin
        )
      }

      it("Short") {

        TypeVizShort[AliasWArg].toString.shouldBe(
          """
            |+ TypeVizSpec.WArg[Double]
            |:       ┏ + ai.acyclic.prover.commons.viz.TypeVizSpec.WArg [ 1 ARG ] :
            |:       ┃ !-- Double
            |!-- Serializable
            |!-- Product
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
            |:       ┃   :       ┃ !-- Double
            |:       ┃   !-- java.io.Serializable ... (see [0])
            |:       ┃   !-- Product ... (see [1])
            |:       ┃   !-- Object ... (see [2])
            |!-- java.io.Serializable ............................................................ [0]
            |!-- Product ......................................................................... [1]
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
            |:       ┃   :       ┃ !-- Double
            |:       ┃   !-- java.io.Serializable ... (see [0])
            |:       ┃   !-- Product ... (see [1])
            |:       ┃   !-- Object ... (see [2])
            |!-- java.io.Serializable ............................................................ [0]
            |!-- Product ......................................................................... [1]
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
            |:       ┃   :       ┃ !-- Double
            |:       ┃   !-- Serializable ... (see [0])
            |:       ┃   !-- Product ... (see [1])
            |:       ┃   !-- Object ... (see [2])
            |!-- Serializable .................................................................... [0]
            |!-- Product ......................................................................... [1]
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
          |!-- Int
          |""".stripMargin
      )
    }

    it("with lower bound") {

      val e = new E { type D >: String <: CharSequence }
      TypeViz[e.D].toString.shouldBe(
        """
          |+ e.D
          |!-- CharSequence
          |""".stripMargin
      )
    }

    it("with final type") {

      val e = new E { final type D = Int }

      TypeViz[e.D].toString.shouldBe(
        """
          |+ e.D
          |!-- Int
          |""".stripMargin
      )
    }

    it("with final type in path") {

      TypeViz[EE.D].toString.shouldBe(
        """
          |- Int ≅ ai.acyclic.prover.commons.viz.TypeVizSpec.EE.D
          |""".stripMargin
      )

      val e = new EE
      TypeViz[e.D].toString.shouldBe(
        """
          |- Int ≅ e.D
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

  case class W(v: Int)

  case class WArg[T](v: T)

  val singleton = 3

  def adhocW = Witness(3) // TODO: merge inot singletonW

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
