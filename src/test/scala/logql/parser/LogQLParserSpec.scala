package logql.parser

import java.util.concurrent.TimeUnit

import logql.parser.AST._
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers._

class LogQLParserSpec extends AnyFunSuite {

  test("""{label1="val"}""") {
    LogQueryParser.parse("""{label1="val"}""") shouldBe Right(
      LogQueryExpr(List(MatchEqual("label1", "val")), Nil)
    )
  }

  test("""{label1="val", label2 =~ "val2" }""") {
    LogQueryParser.parse("""{label1="val", label2 =~ "val2" }""") shouldBe Right(
      LogQueryExpr(
        List(
          MatchEqual("label1", "val"),
          MatchRegexp("label2", "val2")
        ),
        Nil
      )
    )
  }

  test("Test filters") {
    LogQueryParser.parse("""{label1="val"} |= "plaintext" """) shouldBe Right(
      LogQueryExpr(List(MatchEqual("label1", "val")), List(LineFilterExpr(ContainsString, "plaintext")))
    )
  }

  test("Test filters with duplicate filters ") {
    LogQueryParser.parse("""{label1="val"} |= "plaintext" |= "text2" """) shouldBe Right(
      LogQueryExpr(
        List(MatchEqual("label1", "val")),
        List(LineFilterExpr(ContainsString, "plaintext"), LineFilterExpr(ContainsString, "text2"))
      )
    )
  }

  test("Test multiple filters") {
    LogQueryParser.parse("""{label1="val"} |= "plaintext" |~ "ptrn" """) shouldBe Right(
      LogQueryExpr(
        List(MatchEqual("label1", "val")),
        List(
          LineFilterExpr(ContainsString, "plaintext"),
          LineFilterExpr(ContainsRegex, "ptrn")
        )
      )
    )

  }

  test("Test filters with simple condition") {
    LogQueryParser.parse("""{label1="val"} |= "plaintext" | age = 10m """) shouldBe Right(
      LogQueryExpr(
        List(MatchEqual("label1", "val")),
        List(
          LineFilterExpr(ContainsString, "plaintext"),
          ConditionExpr(
            CompareCondition(AST.Equal, "age", DurationValue(10.0, TimeUnit.MINUTES))
          )
        )
      )
    )
  }

  test("Test filters with multiple conditions") {
    LogQueryParser.parse("""{label1="val"} | num = 10 | age = 10""") shouldBe Right(
      LogQueryExpr(
        List(MatchEqual("label1", "val")),
        List(
          ConditionExpr(CompareCondition(AST.Equal, "num", NumberValue(10))),
          ConditionExpr(CompareCondition(AST.Equal, "age", NumberValue(10)))
        )
      )
    )
  }

  test("Test filters with and condition") {
    LogQueryParser.parse("""{label1="val"} |= "plaintext" | age = 10 and num > 10.5s """) shouldBe
      Right(
        LogQueryExpr(
          List(MatchEqual("label1", "val")),
          List(
            LineFilterExpr(ContainsString, "plaintext"),
            ConditionExpr(
              AndCondition(
                CompareCondition(AST.Equal, "age", NumberValue(10)),
                CompareCondition(AST.GreaterThan, "num", DurationValue(10.5, TimeUnit.SECONDS))
              )
            )
          )
        )
      )
  }

  test("Test filters with complex condition") {
    LogQueryParser.parse("""{label1="val", label2 =~ "v.*"} |= "plaintext" | (age = 10 and num > 10) or num2 <= 101 """) shouldBe
      Right(
        LogQueryExpr(
          List(
            MatchEqual("label1", "val"),
            MatchRegexp("label2", "v.*")
          ),
          List(
            LineFilterExpr(ContainsString, "plaintext"),
            ConditionExpr(
              OrCondition(
                AndCondition(
                  CompareCondition(AST.Equal, "age", NumberValue(10)),
                  CompareCondition(AST.GreaterThan, "num", NumberValue(10.0))
                ),
                CompareCondition(AST.LessEqual, "num2", NumberValue(101))
              )
            )
          )
        )
      )
  }

  test("Test filters with multiple condition") {
    LogQueryParser.parse("""{label1="val"} |= "plaintext" | age = 10 and num > 10 | num2 <= 10""") shouldBe
      Right(
        LogQueryExpr(
          List(MatchEqual("label1", "val")),
          List(
            LineFilterExpr(ContainsString, "plaintext"),
            ConditionExpr(
              AndCondition(
                CompareCondition(AST.Equal, "age", NumberValue(10)),
                CompareCondition(AST.GreaterThan, "num", NumberValue(10))
              )
            ),
            ConditionExpr(
              CompareCondition(AST.LessEqual, "num2", NumberValue(10))
            )
          )
        )
      )
  }

  test("Test json parser pipeline ") {
    LogQueryParser.parse("""{label1="val"} |= "test" | json | num = 10 | age = 10""") shouldBe Right(
      LogQueryExpr(
        List(MatchEqual("label1", "val")),
        List(
          LineFilterExpr(ContainsString, "test"),
          JsonParserExpr,
          ConditionExpr(CompareCondition(AST.Equal, "num", NumberValue(10))),
          ConditionExpr(CompareCondition(AST.Equal, "age", NumberValue(10)))
        )
      )
    )
  }

  test("Test with label_format") {
    LogQueryParser.parse(
      """{app="foo"} |= "bar" | json | latency >= 250 | line_format "blip{{ .foo }}blop {{.status_code}}" """
    ) shouldBe Right(
      LogQueryExpr(
        List(MatchEqual("app", "foo")),
        List(
          LineFilterExpr(ContainsString, "bar"),
          JsonParserExpr,
          ConditionExpr(CompareCondition(GreaterEqual, "latency", NumberValue(250.0))),
          LineFormatExpr("blip{{ .foo }}blop {{.status_code}}")
        )
      )
    )
  }

  test("Query with regexp pipeline") {
    LogQueryParser.parse(
      """{job="cortex-ops/query-frontend"} |= "logging.go" | logfmt | line_format "{{.msg}}" | regexp "(?P<method>\\w+) (?P<path>[\\w|/]+) \\((?P<status>\\d+?)\\) (?P<duration>.*)" | (duration > 1 or status=200) and method="POST" | line_format "{{.duration}}|{{.method}}|{{.status}}"""".stripMargin
    ) shouldBe Right(
      LogQueryExpr(
        List(MatchEqual("job", "cortex-ops/query-frontend")),
        List(
          LineFilterExpr(ContainsString, "logging.go"),
          LogFmtExpr,
          LineFormatExpr("{{.msg}}"),
          RegexpExpr("""(?P<method>\\w+) (?P<path>[\\w|/]+) \\((?P<status>\\d+?)\\) (?P<duration>.*)"""),
          ConditionExpr(
            AndCondition(
              OrCondition(
                CompareCondition(GreaterThan, "duration", NumberValue(1.0)),
                CompareCondition(Equal, "status", NumberValue(200.0))
              ),
              CompareCondition(Equal, "method", StringValue("POST"))
            )
          ),
          LineFormatExpr("{{.duration}}|{{.method}}|{{.status}}")
        )
      )
    )
  }

  test("Query with label_format pipeline") {
    LogQueryParser.parse(
      """{app="foo"} |= "bar" | json | latency >= 250ms or ( status_code < 500 and status_code > 200) | label_format foo=bar,status_code="buzz{{.bar}}" | logfmt """
    ) shouldBe Right(
      LogQueryExpr(
        List(MatchEqual("app", "foo")),
        List(
          LineFilterExpr(ContainsString, "bar"),
          JsonParserExpr,
          ConditionExpr(
            OrCondition(
              CompareCondition(GreaterEqual, "latency", DurationValue(250, TimeUnit.MILLISECONDS)),
              AndCondition(
                CompareCondition(LessThan, "status_code", NumberValue(500)),
                CompareCondition(GreaterThan, "status_code", NumberValue(200))
              )
            )
          ),
          LabelFormatExpr(
            List(
              RenameLabelFormat("foo", "bar"),
              TemplateLabelFormat("status_code", "buzz{{.bar}}")
            )
          ),
          LogFmtExpr
        )
      )
    )
  }
}
