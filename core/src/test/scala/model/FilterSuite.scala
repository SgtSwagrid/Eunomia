package com.alecdorrington.eunomia
package model

import io.circe.parser.decode
import io.circe.syntax.*
import munit.FunSuite

class FilterSuite extends FunSuite:

  private final case class Book(name: String, rating: Option[Long])

  private val name   = Field.of[Book]("name", _.name)
  private val rating = Field.of[Book]("rating", _.rating)

  private val every = (name.contains("x") && !(rating >= 50L)) ||
    rating.oneOf(1L, 2L) || rating.missing || name.is("y")

  test("folding a filter with its own cases gives it back"):
    assertEquals(
      every.fold[Filter](
        not = Filter.Not(_),
        all = Filter.And(_),
        any = Filter.Or(_),
        compare = Filter.Compare(_, _, _),
        contains = Filter.Contains(_, _),
        oneOf = Filter.OneOf(_, _),
        missing = Filter.Missing(_),
      ),
      every,
    )

  private def leaves(filter: Filter): Int = filter.fold[Int](
    not = identity,
    all = _.sum,
    any = _.sum,
    compare = (_, _, _) => 1,
    contains = (_, _) => 1,
    oneOf = (_, _) => 1,
    missing = _ => 1,
  )

  test("a fold reaches every leaf once"):
    assertEquals(leaves(every), 5)

  test("an empty conjunction and disjunction each reach no leaf"):
    assertEquals(leaves(Filter.always), 0)
    assertEquals(leaves(Filter.never), 0)

  test("a filter is sent as small JSON objects"):
    assertEquals(
      (rating >= 50L).asJson.noSpaces,
      """{"field":"rating","is":">=","value":50}""",
    )

  test("every shape of filter survives the wire"):
    val filter = (name.contains("x") && !(rating >= 50L)) ||
      rating.oneOf(1L, 2L) || rating.missing || Filter.Compare(
        "score",
        Operator.Unequal,
        Value.Real(2.5),
      ) || Filter.Compare(
        "inPrint",
        Operator.Equal,
        Value.Flag(true),
      )
    assertEquals(
      decode[Filter](filter.asJson.noSpaces),
      Right(filter),
    )

  test("an unrecognised filter is refused"):
    assert(decode[Filter]("""{"xor":[]}""").isLeft)
    assert(decode[Filter]("""{"field":"rating","is":"~","value":1}""").isLeft)
