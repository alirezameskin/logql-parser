package logql.parser.AST

sealed trait Condition

case class CompareCondition(typ: ConditionType, field: String, value: Value) extends Condition
case class AndCondition(v1: Condition, v2: Condition)                        extends Condition
case class OrCondition(v1: Condition, v2: Condition)                         extends Condition
