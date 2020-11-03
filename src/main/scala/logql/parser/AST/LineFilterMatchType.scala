package logql.parser.AST

sealed trait LineFilterMatchType
case object ContainsString    extends LineFilterMatchType
case object ContainsNotString extends LineFilterMatchType
case object ContainsRegex     extends LineFilterMatchType
case object ContainsNotRegex  extends LineFilterMatchType
