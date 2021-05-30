package org.shapesafe.graph.commons.util.reflect.format

import org.shapesafe.graph.commons.testlib.BaseSpec
import org.shapesafe.graph.commons.util.reflect.format.FormatProtos.DeAlias
import org.shapesafe.graph.commons.util.reflect.format.Formats.TypeInfo
import org.shapesafe.graph.commons.util.reflect.format.FormatProtos.DeAlias
import org.shapesafe.graph.commons.util.reflect.format.Formats.TypeInfo

class TypeFormatSpec extends BaseSpec {

  it("+>") {

    val v1 = TypeInfo.DeAlias

    val v2 = TypeInfo ~ DeAlias

    assert(v1 == v2)
  }
}
