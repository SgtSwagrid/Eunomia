package com.alecdorrington.eunomia
package model

import io.circe.parser.decode
import io.circe.syntax.*
import munit.FunSuite

class ListReplySuite extends FunSuite:

  test("both shapes of reply survive the wire"):
    val window: ListReply[Int] =
      ListReply.Window(Window(List(3, 4), 10, Some(Page(2, 2))))
    val whole: ListReply[Int] = ListReply.Whole(List(1, 2, 3))
    assertEquals(
      decode[ListReply[Int]](window.asJson.noSpaces),
      Right(window),
    )
    assertEquals(
      decode[ListReply[Int]](whole.asJson.noSpaces),
      Right(whole),
    )

  test("a whole reply is queried where it is received"):
    val n      = Field.of[Int]("n", identity[Int])
    val schema = ListSchema(n)
    val query  = ListQuery(n > 1, page = Some(Page(0, 1)))
    assertEquals(
      ListReply.Whole(List(1, 2, 3)).answer(schema, query),
      Right(Window(List(2), 2, Some(Page(0, 1)))),
    )
