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

  /** The number of leaves a fold reaches in a filter. */
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
