package logql.parser

import java.util.concurrent.TimeUnit

import scala.util.parsing.combinator.RegexParsers

object LogQueryParser extends RegexParsers {
  private val DURATION_PATTERN = """(\d+(\.\d*)?|\d*\.\d+)(ns|us|µs|ms|s|m|h)""".r

  import logql.parser.AST._

  val ident: Parser[String] =
    "[a-zA-Z0-9_]+".r

  val number: Parser[String] =
    """(\d+(\.\d*)?|\d*\.\d+)""".r

  val string: Parser[String] =
    "\"" ~> """([^"\x00-\x1F\x7F\\]|\\[\\'"bfnrt]|\\u[a-fA-F0-9]{4})*""".r <~ "\""

  //duration
  val duration: Parser[String] =
    DURATION_PATTERN

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
    ident ~ "=" ~ (duration | number | string) ^^ { case name ~ _ ~ value => ConditionExpr(AST.Equal, name, toValue(value)) }

  val notEqualExpr: Parser[ConditionFilterExpr] =
    ident ~ "!=" ~ (duration | string | number) ^^ { case name ~ _ ~ value => ConditionExpr(AST.NotEqual, name, toValue(value)) }

  val greaterThanExpr: Parser[ConditionFilterExpr] =
    ident ~ ">" ~ (duration | number) ^^ { case field ~ _ ~ num => ConditionExpr(AST.GreaterThan, field, toValue(num)) }

  val greaterEqualExpr: Parser[ConditionFilterExpr] =
    ident ~ ">=" ~ (duration | number) ^^ { case field ~ _ ~ num => ConditionExpr(AST.GreaterEqual, field, toValue(num)) }

  val lessThanExpr: Parser[ConditionFilterExpr] =
    ident ~ "<" ~ (duration | number) ^^ { case field ~ _ ~ num => ConditionExpr(AST.LessThan, field, toValue(num)) }

  val lessEqualExpr: Parser[ConditionFilterExpr] =
    ident ~ "<=" ~ (duration | number) ^^ { case field ~ _ ~ num => ConditionExpr(AST.LessEqual, field, toValue(num)) }

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

  private def toValue(value: String): Value =
    value match {
      case DURATION_PATTERN(number, _, unit) => DurationValue(number.toDouble, toTimeUnit(unit))
      case _ =>
        value.toDoubleOption match {
          case Some(number) => NumberValue(number)
          case None         => StringValue(value)
        }
    }

  private def toTimeUnit(unit: String): TimeUnit =
    unit match {
      case "ns" => TimeUnit.NANOSECONDS
      case "us" => TimeUnit.MICROSECONDS
      case "µs" => TimeUnit.MICROSECONDS
      case "ms" => TimeUnit.MILLISECONDS
      case "s"  => TimeUnit.SECONDS
      case "m"  => TimeUnit.MINUTES
      case "h"  => TimeUnit.HOURS
      case _    => TimeUnit.SECONDS
    }

  def parse(string: String): Either[String, LogQueryExpr] =
    parse(query, string) match {
      case Success(result, _) => Right(result)
      case NoSuccess(msg, _)  => Left(msg)
    }
}
