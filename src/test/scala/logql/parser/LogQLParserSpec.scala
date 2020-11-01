package logql.parser

import logql.parser.AST.{MatchEqual, Query}
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers._

class LogQLParserSpec extends AnyFunSuite {

  test("""name{label1="val"}""") {
    LogQLParser.parse("""name{label1="val"}""") shouldBe Right(Query(Some("name"), List(MatchEqual("label1", "val")), Nil))
  }

  test("""{label1="val"}""") {
    LogQLParser.parse("""{label1="val"}""") shouldBe Right(Query(None, List(MatchEqual("label1", "val")), Nil))
  }

}
