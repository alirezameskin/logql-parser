package logql.parser.AST

sealed trait PipelineExpr

case class LineFilterExpr(typeMatch: LineFilterMatchType, matchStr: String) extends PipelineExpr

sealed trait LineFilterMatchType
case object ContainsString    extends LineFilterMatchType
case object ContainsNotString extends LineFilterMatchType
case object ContainsRegex     extends LineFilterMatchType
case object ContainsNotRegex  extends LineFilterMatchType

sealed trait ConditionFilterExpr extends PipelineExpr

case class ConditionExpr(typ: ConditionType, field: String, value: Either[String, Double]) extends ConditionFilterExpr
case class AndCondition(v1: ConditionFilterExpr, v2: ConditionFilterExpr)                  extends ConditionFilterExpr
case class OrCondition(v1: ConditionFilterExpr, v2: ConditionFilterExpr)                   extends ConditionFilterExpr

sealed trait ConditionType
case object Equal        extends ConditionType
case object NotEqual     extends ConditionType
case object GreaterThan  extends ConditionType
case object GreaterEqual extends ConditionType
case object LessThan     extends ConditionType
case object LessEqual    extends ConditionType
