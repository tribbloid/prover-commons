package ai.acyclic.prover.commons.function.tracing

object Repro {

  case class Var[T]()

  trait Tracer[I, O] {
    // Mimic the signature in the user code:
    // def map[O2](right: Var[O] => O2): Tracer[I, O2]
    def map[O2](f: Var[O] => O2): Tracer[I, O2]

    // def flatMap[I2, O2](right: Var[O] => Tracer[I2, O2]): Tracer[(I, I2), O2]
    def flatMap[I2, O2](f: Var[O] => Tracer[I2, O2]): Tracer[(I, I2), O2]
  }

  def trace: Tracer[String, String] = ???

  def success() = {
    for {
      _ <- trace
      y <- trace
    } yield y
  }

//  def failing() = {
//    for {
//      x <- trace
//      c = x // This assignment causes the for-comprehension to map to a tuple, but flatMap receives Var[Tuple]
//      y <- trace
//    } yield y
//  }
//
//  def failingDesugared() = {
//    trace
//      .map(x => (x, x))
//      .flatMap {
//        case (x, c) => // Error here: expected Var[(Var[String], Var[String])], actual (Var[String], Var[String])
//          trace.map(y => y)
//      }
//  }
}
