package com.tribbloids.graph.commons.util.reflect.format

import com.tribbloids.graph.commons.testlib.BaseSpec
import com.tribbloids.graph.commons.util.reflect.format.TypeFormat.{DeAlias, Type}

class TypeFormatSpec extends BaseSpec {

  it("+>") {

    val v1 = DeAlias(Type)

    val v2 = Type ~ DeAlias

    assert(v1 == v2)
  }
}
