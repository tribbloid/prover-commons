package ai.acyclic.prover.commons.ag

import ai.acyclic.prover.commons.util.Erased

object Foundation {

  trait Arrow

  trait Induction {

    def arrow: Arrow
  }

  trait Template extends Erased {

    type _I <: Induction
    type _V

    type _Node = Foundation.Node[_I, _V]
    type _Setter = Foundation.Setter[_I, _V]

    trait Node_ extends Node.Prime[this.type] {
      override val T: Template.this.type = Template.this
    }

    trait Setter_ extends Node.Prime[this.type] {
      override val T: Template.this.type = Template.this
    }
  }

  type T[I, V] = Template { type _I = I; type _V = V }

  implicit def newTemplate[I <: Induction, V]: Template {
    type _I = I
    type _V = V
  } = Erased()

  trait X_T[X <: Template] { // short for axiomatic
    val T: X
  }

  trait Def {

    type Prime[X <: Template] <: X_T[X]

//    type Lt[+I <: Induction, +V] = Prime[? <: Template] { val x: Template { type _X <: I; type _V <: V } }
//    type Lt[+I <: Induction, +V] = Prime[? <: Template { type _X <: I; type _V <: V }]
    type Lt[+I <: Induction, +V] = Prime[Template { type _X <: I; type _V <: V }]

    type Aux[I <: Induction, V] = Prime[? <: Template] { val x: Template { type _X = I; type _V = V } }
  }

  object Node extends Def {

    trait Prime[X <: Template] extends X_T[X] {

      def value: T._V

      def induction: Seq[(T._I, T._V)]
    }
  }
  type Node[+I <: Induction, +V] = Node.Lt[I, V]

  object Setter extends Def {

    trait Prime[X <: Template] extends X_T[X] {

      def link(node: T._Node)(
          to: Seq[(T._I, T._V)]
      ): T._Node
    }
  }
  type Setter[I <: Induction, V] = Setter.Aux[I, V]

  object Graph extends Def {

    trait Prime[X <: Template] extends X_T[X] {

      val engine: Engine

      def entries: engine._Batch[Foundation.Node[T._I, T._V]]
    }
  }

  trait Engine {

    type _Batch[V] <: Batch[V]
  }

}
