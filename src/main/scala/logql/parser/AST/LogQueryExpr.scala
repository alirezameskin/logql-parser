package logql.parser.AST

case class LogQueryExpr(
  labels: List[LabelMatchExpr] = Nil,
  filters: List[PipelineExpr] = Nil
)
