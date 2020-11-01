package logql.parser.AST

case class Query(
  metric: Option[String],
  labels: List[LabelMatcher] = Nil,
  filters: List[LineFilter] = Nil
)
