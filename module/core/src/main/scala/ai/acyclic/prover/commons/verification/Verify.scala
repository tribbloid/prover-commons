package ai.acyclic.prover.commons.verification

import shapeless.test.illTyped

object Verify {

  def typeError = illTyped

  // from
  // https://git.rossabaker.com/http4s/http4s/commit/1b5598249a3fc63bbbddd074ef215161a2aaa38d
//  inline def mustHaveTypeErrors(code: String): Unit = {
//    if (testing.typeChecks(code)) {
//      throw new IllegalArgumentException(s"${code} has no type error")
//    }
//    ()
//  }

//  inline def ensureDoesNotCompile(inline code: String): Unit = {
//    requireConst(code)
//  }
}
