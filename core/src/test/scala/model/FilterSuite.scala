package com.alecdorrington.eunomia
package model

import munit.FunSuite

class FilterSuite extends FunSuite:

  private final case class Book(name: String, rating: Option[Long])

  private val name   = Field.of[Book]("name", _.name)
  private val rating = Field.of[Book]("rating", _.rating)

  /** One filter of every shape there is. */
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

  test("a fold reaches every leaf once"):
    assertEquals(
      every.fold[Int](
        not = identity,
        all = _.sum,
        any = _.sum,
        compare = (_, _, _) => 1,
        contains = (_, _) => 1,
        oneOf = (_, _) => 1,
        missing = _ => 1,
      ),
      5,
    )

  test("an empty conjunction and disjunction each reach no leaf"):
    val count = (filter: Filter) =>
      filter.fold[Int](
        not = identity,
        all = _.sum,
        any = _.sum,
        compare = (_, _, _) => 1,
        contains = (_, _) => 1,
        oneOf = (_, _) => 1,
        missing = _ => 1,
      )
    assertEquals(count(Filter.always), 0)
    assertEquals(count(Filter.never), 0)
