package logql.parser.AST

sealed trait PipelineExpr

case object JsonParserExpr                                                  extends PipelineExpr
case object LogFmtExpr                                                      extends PipelineExpr
case class LineFormatExpr(format: String)                                   extends PipelineExpr
case class RegexpExpr(regex: String)                                        extends PipelineExpr
case class LineFilterExpr(typeMatch: LineFilterMatchType, matchStr: String) extends PipelineExpr
case class LabelFormatExpr(changes: List[LabelFormat])                      extends PipelineExpr
case class ConditionExpr(condition: Condition)                              extends PipelineExpr
