package logql.parser

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
    LogQueryParser.parse("""{label1="val"} |= "plaintext" | age = 10""") shouldBe Right(
      LogQueryExpr(
        List(MatchEqual("label1", "val")),
        List(LineFilterExpr(ContainsString, "plaintext"), ConditionExpr(AST.Equal, "age", Right(10)))
      )
    )
  }

  test("Test filters with multiple conditions") {
    LogQueryParser.parse("""{label1="val"} | num = 10 | age = 10""") shouldBe Right(
      LogQueryExpr(
        List(MatchEqual("label1", "val")),
        List(ConditionExpr(AST.Equal, "num", Right(10)), ConditionExpr(AST.Equal, "age", Right(10)))
      )
    )
  }

  test("Test filters with and condition") {
    LogQueryParser.parse("""{label1="val"} |= "plaintext" | age = 10 and num > 10 """) shouldBe
      Right(
        LogQueryExpr(
          List(MatchEqual("label1", "val")),
          List(
            LineFilterExpr(ContainsString, "plaintext"),
            AndCondition(
              ConditionExpr(AST.Equal, "age", Right(10)),
              ConditionExpr(AST.GreaterThan, "num", Right(10.0))
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
            OrCondition(
              AndCondition(
                ConditionExpr(AST.Equal, "age", Right(10)),
                ConditionExpr(AST.GreaterThan, "num", Right(10.0))
              ),
              ConditionExpr(AST.LessEqual, "num2", Right(101))
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
            AndCondition(
              ConditionExpr(AST.Equal, "age", Right(10)),
              ConditionExpr(AST.GreaterThan, "num", Right(10))
            ),
            ConditionExpr(AST.LessEqual, "num2", Right(10))
          )
        )
      )
  }

}
