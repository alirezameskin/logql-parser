package logql.parser.AST

sealed trait LineFilter
case class ContainsString(string: String)    extends LineFilter
case class ContainsNotString(string: String) extends LineFilter
case class ContainsRegex(regex: String)      extends LineFilter
case class ContainsNotRegex(regex: String)   extends LineFilter
