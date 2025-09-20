package ai.acyclic.prover.infra

object JoinTypeSpike {

  trait A {

    def get: (Any, Any)

    protected def get2: (Any, Any) = get
  }

  trait A1 extends A {

    def get: (Product, Any)
    override protected def get2: (Product, Any) = get
  }

  trait A2 extends A {
    def get: (Any, Int)
    override protected def get2: (Any, Int) = get
  }

//  object B extends A1 with A2 {
//    override def get: (Product, Int) = ???
//  }
}
