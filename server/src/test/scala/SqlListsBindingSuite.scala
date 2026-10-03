package com.alecdorrington.eunomia
package server

import com.alecdorrington.eunomia.model.{Operator, Value}
import munit.FunSuite
import slick.jdbc.H2Profile
import slick.jdbc.H2Profile.api.*

class SqlListsBindingSuite extends FunSuite:

  private val lists  = SqlLists(H2Profile)
  private val rows   = TableQuery[Rows]
  private val name   = lists.text[Rows]("name")(_.name.?)
  private val rating = lists.integer[Rows]("rating")(_.rating)

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
      where(rating.compare(_, Operator.AtLeast, Value.Integer(50L))),
      "50",
    )

  test("each of several values is a parameter"):
    assertBound(
      where(rating.oneOf(
        _,
        List(Value.Integer(45L), Value.Integer(90L)),
      )),
      "45",
    )

  test("sought text is a parameter"):
    assertBound(
      where(name.contains(_, "alpha")),
      "alpha",
    )
