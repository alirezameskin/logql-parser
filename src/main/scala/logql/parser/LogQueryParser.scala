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
  val equalExpr: Parser[CompareCondition] =
    ident ~ "=" ~ (duration | number | string) ^^ { case name ~ _ ~ value => CompareCondition(AST.Equal, name, toValue(value)) }

  val notEqualExpr: Parser[CompareCondition] =
    ident ~ "!=" ~ (duration | string | number) ^^ {
      case name ~ _ ~ value => CompareCondition(AST.NotEqual, name, toValue(value))
    }

  val greaterThanExpr: Parser[CompareCondition] =
    ident ~ ">" ~ (duration | number) ^^ { case field ~ _ ~ num => CompareCondition(AST.GreaterThan, field, toValue(num)) }

  val greaterEqualExpr: Parser[CompareCondition] =
    ident ~ ">=" ~ (duration | number) ^^ { case field ~ _ ~ num => CompareCondition(AST.GreaterEqual, field, toValue(num)) }

  val lessThanExpr: Parser[CompareCondition] =
    ident ~ "<" ~ (duration | number) ^^ { case field ~ _ ~ num => CompareCondition(AST.LessThan, field, toValue(num)) }

  val lessEqualExpr: Parser[CompareCondition] =
    ident ~ "<=" ~ (duration | number) ^^ { case field ~ _ ~ num => CompareCondition(AST.LessEqual, field, toValue(num)) }

  lazy val andOr: Parser[Condition] =
    expression ~ rep(("and" | "or") ~ expression) ^^ {
      case x ~ ls =>
        ls.foldLeft[Condition](x) {
          case (e1, "and" ~ e2) => AndCondition(e1, e2)
          case (e1, "or" ~ e2)  => OrCondition(e1, e2)
        }
    }

  val parenthesis: Parser[Condition] =
    "(" ~> andOr <~ ")"

  val expression: Parser[Condition] =
    parenthesis | lessEqualExpr | lessThanExpr | greaterEqualExpr | greaterThanExpr | equalExpr

  val condition: Parser[ConditionExpr] =
    ("|" ~> andOr | "|" ~> expression) ^^ (cnd => ConditionExpr(cnd))

  //parser pipelines
  val jsonParser: Parser[PipelineExpr] =
    "|" ~> "json" ^^ (_ => JsonParserExpr)

  val logFmtParser: Parser[PipelineExpr] =
    "|" ~> "logfmt" ^^ (_ => LogFmtExpr)

  val lineFormat: Parser[LineFormatExpr] =
    "|" ~> "line_format" ~ string ^^ { case _ ~ fmt => LineFormatExpr(fmt) }

  val regexp: Parser[RegexpExpr] =
    "|" ~> "regexp" ~ string ^^ { case _ ~ reg => RegexpExpr(reg) }

  lazy val labelRename: Parser[RenameLabelFormat] =
    ident ~ "=" ~ ident ^^ { case oldN ~ _ ~ newN => RenameLabelFormat(oldN, newN) }

  lazy val labelTpl: Parser[TemplateLabelFormat] =
    ident ~ "=" ~ string ^^ { case name ~ _ ~ tpl => TemplateLabelFormat(name, tpl) }

  val labelFmt: Parser[LabelFormatExpr] =
    "|" ~> "label_format" ~ rep1sep(labelRename | labelTpl, ",") ^^ {
      case _ ~ changes => LabelFormatExpr(changes)
    }

  val parser: Parser[PipelineExpr] =
    lineFormat | jsonParser | logFmtParser | regexp | labelFmt

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
