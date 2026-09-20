package com.alecdorrington.eunomia
package server

import com.alecdorrington.eunomia.model.{Comparison, Value}
import munit.FunSuite
import slick.jdbc.H2Profile
import slick.jdbc.H2Profile.api.*

/**
  * Checks that the values a list is filtered by are sent as parameters rather
  * than written into the statement, so that a database sees one statement per
  * shape of filter instead of one per value typed.
  */
class SqlBindingSuite extends FunSuite:

  private val lists  = SqlLists(H2Profile)
  private val rows   = TableQuery[Rows]
  private val name   = lists.text[Rows]("name")(_.name.?)
  private val rating = lists.whole[Rows]("rating")(_.rating)

  /** The `WHERE` clause of a list narrowed by one condition. */
  private def where(condition: Rows => Rep[Boolean]): String =
    val sql = rows.filter(condition).result.statements.head
    sql.substring(sql.indexOf("where"))

  private def assertBound(clause: String, value: String): Unit =
    assert(
      clause.contains("?"),
      s"$clause holds no parameter.",
    )
    assert(
      !clause.contains(value),
      s"$clause holds `$value` itself.",
    )

  test("a compared value is a parameter"):
    assertBound(
      where(rating.compare(_, Comparison.Ge, Value.Whole(50L))),
      "50",
    )

  test("each of several values is a parameter"):
    assertBound(
      where(rating.oneOf(
        _,
        List(Value.Whole(45L), Value.Whole(90L)),
      )),
      "45",
    )

  test("sought text is a parameter"):
    assertBound(
      where(name.contains(_, "alpha")),
      "alpha",
    )
