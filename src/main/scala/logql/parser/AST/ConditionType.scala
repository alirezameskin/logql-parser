package logql.parser.AST

sealed trait ConditionType
case object Equal        extends ConditionType
case object NotEqual     extends ConditionType
case object GreaterThan  extends ConditionType
case object GreaterEqual extends ConditionType
case object LessThan     extends ConditionType
case object LessEqual    extends ConditionType
