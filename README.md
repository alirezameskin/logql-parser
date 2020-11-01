## Simple LogQL parser

```scala

import logql.parser.LogQLParser

val ast = LogQLParser.parse("""name{label1="val"}""")
//ast : Right(Query(Some("name"), List(MatchEqual("label1", "val")), Nil))
```