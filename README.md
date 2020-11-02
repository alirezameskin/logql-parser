## Simple LogQL parser

```scala

import logql.parser.LogQueryParser

val res1 = LogQueryParser.parse("""{label1="val"}""")
//res1: Right(LogQueryExpr(List(MatchEqual(label1,val)),List()))


val res2 = LogQueryParser.parse("""{label1="val", label2 =~ "v.*"} |= "plaintext" | (age = 10 and num > 10) or num2 <= 101 """)
/*
res2: Right(
        LogQueryExpr(
          List(
            MatchEqual("label1", "val"),
            MatchRegexp("label2", "v.*"),
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
*/
```