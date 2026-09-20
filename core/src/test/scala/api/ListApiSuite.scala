package com.alecdorrington.eunomia
package api

import com.alecdorrington.eunomia.model.*
import io.circe.parser.decode
import io.circe.syntax.*
import munit.FunSuite

class ListApiSuite extends FunSuite:

  private final case class Book(name: String, rating: Option[Long])

  private val name   = Field.of[Book]("name", _.name)
  private val rating = Field.of[Book]("rating", _.rating)

  test("a filter is sent as small JSON objects"):
    assertEquals(
      (rating >= 50L).asJson.noSpaces,
      """{"field":"rating","is":">=","value":50}""",
    )

  test("every shape of filter survives the wire"):
    val filter = (name.contains("x") && !(rating >= 50L)) ||
      rating.oneOf(1L, 2L) || rating.missing ||
      Filter.Compare("score", Comparison.Ne, Value.Real(2.5)) || Filter.Compare(
        "inPrint",
        Comparison.Eq,
        Value.Flag(true),
      )
    assertEquals(
      decode[Filter](filter.asJson.noSpaces),
      Right(filter),
    )

  test("an unrecognised filter is refused"):
    assert(decode[Filter]("""{"xor":[]}""").isLeft)
    assert(decode[Filter]("""{"field":"rating","is":"~","value":1}""").isLeft)

  test("both shapes of reply survive the wire"):
    val window: ListReply[Int] =
      ListReply.Window(Paged(List(3, 4), 10, Some(Page(2, 2))))
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
    val schema = Schema(Field.of[Int]("n", identity[Int]))
    val query  = ListQuery(
      Field.of[Int]("n", identity[Int]) > 1,
      page = Some(Page(0, 1)),
    )
    assertEquals(
      ListReply.Whole(List(1, 2, 3)).answer(schema, query),
      Right(Paged(List(2), 2, Some(Page(0, 1)))),
    )

  test("a query is written as parameters, omitting the defaults"):
    val query = ListQuery(
      rating >= 50L,
      List(rating.descending, name.ascending),
      Some(Page(20, 10)),
    )
    assertEquals(
      ListParams.of(query),
      List(
        "filter" -> """{"field":"rating","is":">=","value":50}""",
        "sort"   -> "-rating,name",
        "offset" -> "20",
        "limit"  -> "10",
      ),
    )
    assertEquals(ListParams.of(ListQuery()), List.empty)
