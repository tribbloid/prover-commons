package ai.acyclic.prover.commons.debug

import ai.acyclic.prover.commons.testlib.BaseSpec
import sourcecode.Text

class SourceCodeSpike extends BaseSpec {

  it("sourcecode.Text captures an expression and its source") {

    val a = 1
    val b = 2

    val captured: Text[Int] = Text(a + b)

    assert(captured.value == 3)
    assert(captured.source.replaceAll("\\s", "") == "a+b")

    def show[T](t: Text[T]): String = s"${t.source} = ${t.value}"

    assert(show(Text(a + b)).replaceAll("\\s", "") == "a+b=3")
  }

  it("sourcecode.Text can be constructed implicitly") {

    val a = 1
    val b = 2

    def takesText[T](v: Text[T]): Text[T] = v

    val captured: Text[Int] = takesText(a + b)

    assert(captured.value == 3)
    assert(captured.source.replaceAll("\\s", "") == "a+b")
  }
}
