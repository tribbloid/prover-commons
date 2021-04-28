package com.tribbloids.graph.commons.util.reflect.format

import com.tribbloids.graph.commons.testlib.BaseSpec
import com.tribbloids.graph.commons.util.reflect.format.Factories.DeAlias
import com.tribbloids.graph.commons.util.reflect.format.Formats.TypeInfo

class TypeFormatSpec extends BaseSpec {

  it("+>") {

    val v1 = TypeInfo.DeAlias

    val v2 = TypeInfo ~ DeAlias

    assert(v1 == v2)
  }
}
