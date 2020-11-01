package logql.parser.AST

sealed trait LabelMatcher
case class MatchEqual(name: String, pattern: String)     extends LabelMatcher
case class MatchNotEqual(name: String, pattern: String)  extends LabelMatcher
case class MatchRegexp(name: String, pattern: String)    extends LabelMatcher
case class MatchNotRegexp(name: String, pattern: String) extends LabelMatcher
