package logql.parser.AST

sealed trait LabelMatchExpr
case class MatchEqual(name: String, pattern: String)     extends LabelMatchExpr
case class MatchNotEqual(name: String, pattern: String)  extends LabelMatchExpr
case class MatchRegexp(name: String, pattern: String)    extends LabelMatchExpr
case class MatchNotRegexp(name: String, pattern: String) extends LabelMatchExpr
