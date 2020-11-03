package logql.parser

import scala.util.parsing.combinator.RegexParsers

object LogQueryParser extends RegexParsers {
  import logql.parser.AST._

  val ident: Parser[String] =
    "[a-zA-Z0-9_]+".r

  val number: Parser[String] =
    """-?\d+""".r

  val string: Parser[String] =
    "\"" ~> """([^"\x00-\x1F\x7F\\]|\\[\\'"bfnrt]|\\u[a-fA-F0-9]{4})*""".r <~ "\""

  // Label matchers
  val equalLabel: Parser[MatchEqual] =
    ident ~ "=" ~ string ^^ { case name ~ _ ~ ptr => MatchEqual(name, ptr) }

  val notEqualLabel: Parser[MatchNotEqual] =
    ident ~ "!=" ~ string ^^ { case name ~ _ ~ ptr => MatchNotEqual(name, ptr) }

  val regexLabel: Parser[MatchRegexp] =
    ident ~ "=~" ~ string ^^ { case name ~ _ ~ ptr => MatchRegexp(name, ptr) }

  val regexNotLabel: Parser[MatchNotRegexp] =
    ident ~ "!~" ~ string ^^ { case name ~ _ ~ ptr => MatchNotRegexp(name, ptr) }

  val label: Parser[LabelMatchExpr] =
    equalLabel | notEqualLabel | regexLabel | regexNotLabel

  val labels: Parser[List[LabelMatchExpr]] =
    "{" ~> repsep(label, ",") <~ "}"

  // Line filters
  val containsString: Parser[LineFilterExpr] =
    "|=" ~> string ^^ (s => LineFilterExpr(ContainsString, s))

  val containsNotString: Parser[LineFilterExpr] =
    "!=" ~> string ^^ (s => LineFilterExpr(ContainsNotString, s))

  val containsRegex: Parser[LineFilterExpr] =
    "|~" ~> string ^^ (s => LineFilterExpr(ContainsRegex, s))

  val containsNotRegex: Parser[LineFilterExpr] =
    "!~" ~> string ^^ (s => LineFilterExpr(ContainsNotRegex, s))

  val lineFilter: Parser[LineFilterExpr] =
    containsString | containsNotString | containsRegex | containsNotRegex

  // Conditions
  val equalExpr: Parser[ConditionFilterExpr] =
    ident ~ "=" ~ (string | number) ^^ { case name ~ _ ~ value => ConditionExpr(AST.Equal, name, toDoubleOrString(value)) }

  val notEqualExpr: Parser[ConditionFilterExpr] =
    ident ~ "!=" ~ (string | number) ^^ { case name ~ _ ~ value => ConditionExpr(AST.NotEqual, name, toDoubleOrString(value)) }

  val greaterThanExpr: Parser[ConditionFilterExpr] =
    ident ~ ">" ~ number ^^ { case field ~ _ ~ num => ConditionExpr(AST.GreaterThan, field, toDoubleOrString(num)) }

  val greaterEqualExpr: Parser[ConditionFilterExpr] =
    ident ~ ">=" ~ number ^^ { case field ~ _ ~ num => ConditionExpr(AST.GreaterEqual, field, toDoubleOrString(num)) }

  val lessThanExpr: Parser[ConditionFilterExpr] =
    ident ~ "<" ~ number ^^ { case field ~ _ ~ num => ConditionExpr(AST.LessThan, field, toDoubleOrString(num)) }

  val lessEqualExpr: Parser[ConditionFilterExpr] =
    ident ~ "<=" ~ number ^^ { case field ~ _ ~ num => ConditionExpr(AST.LessEqual, field, toDoubleOrString(num)) }

  lazy val andOr: Parser[ConditionFilterExpr] =
    expression ~ rep(("and" | "or") ~ expression) ^^ {
      case x ~ ls =>
        ls.foldLeft[ConditionFilterExpr](x) {
          case (e1, "and" ~ e2) => AndCondition(e1, e2)
          case (e1, "or" ~ e2)  => OrCondition(e1, e2)
        }
    }

  val parenthesis: Parser[ConditionFilterExpr] =
    "(" ~> andOr <~ ")"

  val expression: Parser[ConditionFilterExpr] =
    parenthesis | lessEqualExpr | lessThanExpr | greaterEqualExpr | greaterThanExpr | equalExpr

  val condition: Parser[ConditionFilterExpr] =
    "|" ~> andOr | "|" ~> expression

  //parser pipelines
  val jsonParser: Parser[PipelineExpr] =
    "|" ~> "json" ^^ (_ => JsonParserExpr)

  val logFmtParser: Parser[PipelineExpr] =
    "|" ~> "logfmt" ^^ (_ => LogFmtExpr)

  val lineFormat: Parser[LineFormatExpr] =
    "|" ~> "line_format" ~ string ^^ { case _ ~ fmt => LineFormatExpr(fmt) }

  val regexp: Parser[RegexpExpr] =
    "|" ~> "regexp" ~ string ^^ { case _ ~ reg => RegexpExpr(reg) }

  val parser: Parser[PipelineExpr] =
    lineFormat | jsonParser | logFmtParser | regexp

  // Pipelines
  val pipelines: Parser[List[PipelineExpr]] =
    rep(condition | lineFilter | parser)

  // Query pattern
  val query: Parser[LogQueryExpr] = labels ~ pipelines ^^ {
    case labels ~ filters => LogQueryExpr(labels, filters)
  }

  private def toDoubleOrString(value: String): Either[String, Double] =
    value.toDoubleOption match {
      case Some(number) => Right(number)
      case None         => Left(value)
    }

  def parse(string: String): Either[String, LogQueryExpr] =
    parse(query, string) match {
      case Success(result, _) => Right(result)
      case NoSuccess(msg, _)  => Left(msg)
    }
}
