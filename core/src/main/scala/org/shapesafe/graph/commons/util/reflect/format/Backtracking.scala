package org.shapesafe.graph.commons.util.reflect.format

import scala.util.control.NoStackTrace

class Backtracking(message: String) extends Throwable(message) with NoStackTrace {}
