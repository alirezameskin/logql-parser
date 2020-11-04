package logql.parser.AST

import scala.concurrent.duration.TimeUnit

sealed trait Value
case class StringValue(string: String)                   extends Value
case class NumberValue(number: Double)                   extends Value
case class DurationValue(number: Double, unit: TimeUnit) extends Value
