package com.alecdorrington.eunomia
package model

import com.alecdorrington.eunomia.model.Filter.*
import com.alecdorrington.eunomia.model.Operator.*
import munit.FunSuite

class CellFilterSuite extends FunSuite:

  private def text(input: String) = CellFilter.parse("name", Kind.Text, input)

  private def integer(input: String) =
    CellFilter.parse("rating", Kind.Integer, input)
  private def real(input: String) = CellFilter.parse("score", Kind.Real, input)

  private def flag(input: String) =
    CellFilter.parse("inPrint", Kind.Flag, input)

  test("a blank cell matches everything"):
    assertEquals(text(""), Right(Filter.always))
    assertEquals(integer("  | "), Right(Filter.always))

  test("text is sought by substring, spaces included"):
    assertEquals(
      text("my book"),
      Right(Contains("name", "my book")),
    )

  test("text can be excluded, or matched exactly"):
    assertEquals(
      text("!edition"),
      Right(Not(Contains("name", "edition"))),
    )
    assertEquals(
      text("=Alpha"),
      Right(Compare("name", Equal, Value.Text("Alpha"))),
    )
    assertEquals(
      text("!=Alpha"),
      Right(Compare("name", Unequal, Value.Text("Alpha"))),
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
      integer("50"),
      Right(Compare("rating", Equal, Value.Integer(50))),
    )
    assertEquals(
      integer("-5"),
      Right(Compare("rating", Equal, Value.Integer(-5))),
    )

  test("each operator is read by its longest symbol"):
    Operator
      .values
      .foreach(operator =>
        assertEquals(
          integer(s"${ operator.symbol }50"),
          Right(Compare("rating", operator, Value.Integer(50))),
        ),
      )

  test("conditions separated by spaces must all hold"):
    assertEquals(
      integer(">=50 <80"),
      Right(And(List(
        Compare("rating", AtLeast, Value.Integer(50)),
        Compare("rating", Less, Value.Integer(80)),
      ))),
    )

  test("a range is inclusive at both ends"):
    assertEquals(
      integer("50..80"),
      Right(And(List(
        Compare("rating", AtLeast, Value.Integer(50)),
        Compare("rating", AtMost, Value.Integer(80)),
      ))),
    )

  test("a range whose bounds are the wrong way round is refused"):
    assert(integer("80..50").isLeft)
    assert(real("1...2").isLeft)

  test("a range missing a bound is refused"):
    assert(integer("..80").isLeft)
    assert(integer("50 .. 80").isLeft)

  test("a number must be of the field's kind"):
    assert(integer("50.5").isLeft)
    assert(integer("abc").isLeft)
    assertEquals(
      real("50.5"),
      Right(Compare("score", Equal, Value.Real(50.5))),
    )

  test("an integer too large for a double is read exactly"):
    assertEquals(
      integer("9007199254740993"),
      Right(Compare(
        "rating",
        Equal,
        Value.Integer(9007199254740993L),
      )),
    )
    assertEquals(
      integer(">=9007199254740993"),
      Right(Compare(
        "rating",
        AtLeast,
        Value.Integer(9007199254740993L),
      )),
    )

  test("absence and presence are written the same way for any kind"):
    assertEquals(integer("?"), Right(Missing("rating")))
    assertEquals(
      text("!?"),
      Right(Not(Missing("name"))),
    )

  test("truth values are read in several spellings"):
    assertEquals(
      flag("Yes"),
      Right(Compare("inPrint", Equal, Value.Flag(true))),
    )
    assertEquals(
      flag("0"),
      Right(Compare("inPrint", Equal, Value.Flag(false))),
    )
    assert(flag("maybe").isLeft)
