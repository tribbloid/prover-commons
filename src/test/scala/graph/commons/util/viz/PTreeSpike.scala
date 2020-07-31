package graph.commons.util.viz

import org.scalatest.FunSpec
import pprint.{Renderer, Result, Tree}

class PTreeSpike extends FunSpec {

  val nodeStr = s"""
                   |[A]
                   |[B]
                   |""".stripMargin.trim

  it("can handle multiple lines") {

    val tree = Tree.Apply(
      nodeStr,
      Iterator(
        Tree.Apply(
          nodeStr,
          Iterator(
            Tree.Literal(
              nodeStr
            ),
            Tree.Literal(
              nodeStr
            ),
          )
        ),
      )
    )

//    print_@(tree)

//    val ii = pprint.pprintln(tree)

    val renderer = new Renderer(
      1000,
      pprint.colorApplyPrefix,
      pprint.colorLiteral,
      4
    )

    val lines: Result = renderer.rec(tree, 0, 4)

    val result = lines.iter.mkString("\n")

    println(result)

  }

  case class Prod(prefix: String)(vs: Any*) extends Product {
    override def productPrefix: String = prefix

    override def productArity: Int = vs.size

    override def productElement(n: Int): Any = vs(n)
  }

  it("try it another time") {

    val v = Prod(nodeStr)(
      Prod(nodeStr)(
        Prod(nodeStr)(),
        Prod(nodeStr)()
      )
    )

    pprint.pprintln(v)
  }
}
