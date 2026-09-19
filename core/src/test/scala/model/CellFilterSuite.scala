package com.alecdorrington.eunomia
package model

import com.alecdorrington.eunomia.model.Comparison.*
import com.alecdorrington.eunomia.model.Filter.*
import munit.FunSuite

class CellFilterSuite extends FunSuite:

  private def text(input: String) = CellFilter.parse("name", Kind.Text, input)

  private def whole(input: String) =
    CellFilter.parse("rating", Kind.Whole, input)
  private def real(input: String) = CellFilter.parse("score", Kind.Real, input)

  private def flag(input: String) =
    CellFilter.parse("inPrint", Kind.Flag, input)

  test("a blank cell matches everything"):
    assertEquals(text(""), Right(Filter.always))
    assertEquals(whole("  | "), Right(Filter.always))

  test("text is sought by substring, spaces included"):
    assertEquals(
      text("my book"),
      Right(Contains("name", "my book")),
    )

  test("text can be excluded, or matched exactly"):
    assertEquals(
      text("!draft"),
      Right(Not(Contains("name", "draft"))),
    )
    assertEquals(
      text("=Alpha"),
      Right(Compare("name", Eq, Value.Text("Alpha"))),
    )
    assertEquals(
      text("!=Alpha"),
      Right(Compare("name", Ne, Value.Text("Alpha"))),
    )

  test("alternatives match wherever any does"):
    assertEquals(
      text("alpha | beta"),
      Right(Or(List(
        Contains("name", "alpha"),
        Contains("name", "beta"),
      ))),
    )

  test("a bare number is matched exactly"):
    assertEquals(
      whole("50"),
      Right(Compare("rating", Eq, Value.Whole(50))),
    )
    assertEquals(
      whole("-5"),
      Right(Compare("rating", Eq, Value.Whole(-5))),
    )

  test("each comparison is read by its longest operator"):
    Comparison
      .values
      .foreach(comparison =>
        assertEquals(
          whole(s"${ comparison.symbol }50"),
          Right(Compare("rating", comparison, Value.Whole(50))),
        ),
      )

  test("conditions separated by spaces must all hold"):
    assertEquals(
      whole(">=50 <80"),
      Right(And(List(
        Compare("rating", Ge, Value.Whole(50)),
        Compare("rating", Lt, Value.Whole(80)),
      ))),
    )

  test("a range is inclusive at both ends"):
    assertEquals(
      whole("50..80"),
      Right(And(List(
        Compare("rating", Ge, Value.Whole(50)),
        Compare("rating", Le, Value.Whole(80)),
      ))),
    )

  test("a number must be of the field's kind"):
    assert(whole("50.5").isLeft)
    assert(whole("abc").isLeft)
    assertEquals(
      real("50.5"),
      Right(Compare("score", Eq, Value.Real(50.5))),
    )

  test("absence and presence are written the same way for any kind"):
    assertEquals(whole("?"), Right(Missing("rating")))
    assertEquals(
      text("!?"),
      Right(Not(Missing("name"))),
    )

  test("truth values are read in several spellings"):
    assertEquals(
      flag("Yes"),
      Right(Compare("inPrint", Eq, Value.Flag(true))),
    )
    assertEquals(
      flag("0"),
      Right(Compare("inPrint", Eq, Value.Flag(false))),
    )
    assert(flag("maybe").isLeft)
