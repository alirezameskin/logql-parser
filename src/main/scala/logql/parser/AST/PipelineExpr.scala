package logql.parser.AST

sealed trait PipelineExpr

case object JsonParserExpr                                                  extends PipelineExpr
case object LogFmtExpr                                                      extends PipelineExpr
case class LineFormatExpr(format: String)                                   extends PipelineExpr
case class RegexpExpr(regex: String)                                        extends PipelineExpr
case class LineFilterExpr(typeMatch: LineFilterMatchType, matchStr: String) extends PipelineExpr

sealed trait ConditionFilterExpr extends PipelineExpr

case class ConditionExpr(typ: ConditionType, field: String, value: Value) extends ConditionFilterExpr
case class AndCondition(v1: ConditionFilterExpr, v2: ConditionFilterExpr) extends ConditionFilterExpr
case class OrCondition(v1: ConditionFilterExpr, v2: ConditionFilterExpr)  extends ConditionFilterExpr
