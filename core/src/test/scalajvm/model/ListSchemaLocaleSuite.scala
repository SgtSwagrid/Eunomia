package com.alecdorrington.eunomia
package model

import munit.FunSuite

/**
  * The default locale is shared by every suite in the JVM, so it is not changed
  * here. Instead, the items hold both capital I's: folding case by any locale's
  * rules (`İ` to `i` and a combining dot, or Turkish `I` to `ı`) misses one.
  */
class ListSchemaLocaleSuite extends FunSuite:

  private val name   = Field.of[String]("name", identity[String])
  private val schema = ListSchema(name)

  test("case is folded alike whatever the default locale"):
    assertEquals(
      schema
        .run(
          ListQuery(name.contains("iliad")),
          List("ILIAD", "İLIAD"),
        )
        .map(_.items),
      Right(List("ILIAD", "İLIAD")),
    )
