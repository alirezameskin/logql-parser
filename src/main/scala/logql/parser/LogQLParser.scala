package logql.parser

import scala.util.parsing.combinator.RegexParsers

object LogQLParser extends RegexParsers {
  import logql.parser.AST._

  val ident: Parser[String] = "[a-zA-Z0-9_]+".r

  val stringLiteral: Parser[String] =
    "\"" ~> """([^"\x00-\x1F\x7F\\]|\\[\\'"bfnrt]|\\u[a-fA-F0-9]{4})*""".r <~ "\""

  val equalLabel: Parser[MatchEqual]        = ident ~ "=" ~ stringLiteral ^^ { case name ~ _ ~ ptr  => MatchEqual(name, ptr) }
  val notEqualLabel: Parser[MatchNotEqual]  = ident ~ "!=" ~ stringLiteral ^^ { case name ~ _ ~ ptr => MatchNotEqual(name, ptr) }
  val regexLabel: Parser[MatchRegexp]       = ident ~ "=~" ~ stringLiteral ^^ { case name ~ _ ~ ptr => MatchRegexp(name, ptr) }
  val regexNotLabel: Parser[MatchNotRegexp] = ident ~ "!~" ~ stringLiteral ^^ { case name ~ _ ~ ptr => MatchNotRegexp(name, ptr) }

  val label: Parser[LabelMatcher]        = equalLabel | notEqualLabel | regexLabel | regexNotLabel
  val labels: Parser[List[LabelMatcher]] = "{" ~> repsep(label, ",") <~ "}"

  val containsString: Parser[ContainsString]       = "|=" ~> stringLiteral ^^ ContainsString
  val containsNotString: Parser[ContainsNotString] = "!=" ~> stringLiteral ^^ ContainsNotString
  val containsRegex: Parser[ContainsRegex]         = "|~" ~> stringLiteral ^^ ContainsRegex
  val containsNotRegex: Parser[ContainsNotRegex]   = "!~" ~> stringLiteral ^^ ContainsNotRegex

  val filter: Parser[LineFilter]        = containsString | containsNotString | containsRegex | containsNotRegex
  val filters: Parser[List[LineFilter]] = rep(filter)

  val query: Parser[Query] = opt(ident) ~ labels ~ filters ^^ {
    case metric ~ labels ~ filters => Query(metric, labels, filters)
  }

  def parse(string: String): Either[String, Query] =
    parse(query, string) match {
      case Success(result, _) => Right(result)
      case NoSuccess(msg, _)  => Left(msg)
    }
}
