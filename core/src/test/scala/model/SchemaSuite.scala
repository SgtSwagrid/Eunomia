package com.alecdorrington.eunomia
package model

import java.util.Locale
import munit.FunSuite

class SchemaSuite extends FunSuite:

  private final case class Book
    (
      name: String,
      rating: Option[Long],
      pages: Int,
      inPrint: Boolean,
    )

  private val books = List(
    Book("Alpha", Some(72L), 500, inPrint = true),
    Book("beta", Some(45L), 300, inPrint = false),
    Book("Gamma", None, 800, inPrint = false),
    Book("Delta", Some(90L), 450, inPrint = true),
  )

  private val name    = Field.of[Book]("name", _.name)
  private val rating  = Field.of[Book]("rating", _.rating)
  private val pages   = Field.of[Book]("pages", _.pages)
  private val inPrint = Field.of[Book]("inPrint", _.inPrint)

  private val schema = Schema(name, rating, pages, inPrint)

  private def names(query: ListQuery): Either[String, List[String]] = schema
    .run(query, books)
    .map(_.items.map(_.name))

  test("fields take their kind from the type they read, absent or not"):
    assertEquals(
      schema.kinds,
      Map(
        "name"    -> Kind.Text,
        "rating"  -> Kind.Whole,
        "pages"   -> Kind.Whole,
        "inPrint" -> Kind.Flag,
      ),
    )

  test("a schema may not name two fields alike"):
    intercept[IllegalArgumentException](Schema(
      name,
      rating,
      Field.of[Book]("name", _.pages),
    ))

  test("a comparison never holds for an absent value"):
    assertEquals(
      names(ListQuery(rating < 100L)),
      Right(List("Alpha", "beta", "Delta")),
    )

  test("negating a comparison includes the absent values"):
    assertEquals(
      names(ListQuery(!(rating < 100L))),
      Right(List("Gamma")),
    )

  test("absence is asked for explicitly"):
    assertEquals(
      names(ListQuery(rating.missing)),
      Right(List("Gamma")),
    )
    assertEquals(
      names(ListQuery(rating.present)).map(_.size),
      Right(3),
    )

  test("text is sought ignoring case"):
    assertEquals(
      names(ListQuery(name.contains("ALPH"))),
      Right(List("Alpha")),
    )

  test("filters compose"):
    val query = ListQuery((inPrint.is(true) && rating > 80L) || pages >= 800)
    assertEquals(
      names(query),
      Right(List("Gamma", "Delta")),
    )

  test("one of several values"):
    assertEquals(
      names(ListQuery(rating.oneOf(45L, 90L))),
      Right(List("beta", "Delta")),
    )

  test("an empty conjunction holds everywhere, an empty disjunction nowhere"):
    assertEquals(
      names(ListQuery(Filter.always)).map(_.size),
      Right(4),
    )
    assertEquals(
      names(ListQuery(Filter.never)),
      Right(List.empty),
    )

  test("absent values come last in either direction"):
    assertEquals(
      names(ListQuery(order = List(rating.ascending))),
      Right(List("beta", "Alpha", "Delta", "Gamma")),
    )
    assertEquals(
      names(ListQuery(order = List(rating.descending))),
      Right(List("Delta", "Alpha", "beta", "Gamma")),
    )

  test("later keys break the ties of earlier ones"):
    assertEquals(
      names(ListQuery(order = List(inPrint.descending, name.ascending))),
      Right(List("Alpha", "Delta", "Gamma", "beta")),
    )

  test("a page is a window, and the total counts every match"):
    val query = ListQuery(
      order = List(name.ascending),
      page = Some(Page(1, 2)),
    )
    val paged = schema.run(query, books)
    assertEquals(
      paged.map(_.items.map(_.name)),
      Right(List("Delta", "Gamma")),
    )
    assertEquals(paged.map(_.total), Right(4))
    assertEquals(
      paged.map(_.page),
      Right(Some(Page(1, 2))),
    )

  test("an unknown field is refused"):
    assert(
      schema
        .run(
          ListQuery(Filter.Missing("author")),
          books,
        )
        .isLeft,
    )
    assert(
      schema
        .run(
          ListQuery(order = List(Order("author"))),
          books,
        )
        .isLeft,
    )

  test("a value is converted to the kind of its field, when it can be"):
    val whole = Filter.Compare(
      "rating",
      Comparison.Eq,
      Value.Real(45.0),
    )
    val half = Filter.Compare(
      "rating",
      Comparison.Eq,
      Value.Real(45.5),
    )
    assertEquals(
      names(ListQuery(whole)),
      Right(List("beta")),
    )
    assert(names(ListQuery(half)).isLeft)

  test("text is sought only in text fields"):
    assert(names(ListQuery(Filter.Contains("rating", "4"))).isLeft)

  test("a malformed page is refused"):
    assert(names(ListQuery(page = Some(Page(-1, 10)))).isLeft)
    assert(names(ListQuery(page = Some(Page(0, 0)))).isLeft)

  test("a limit caps the page, and an unpaged query gets the first"):
    assertEquals(
      ListQuery().limited(50).page,
      Some(Page(0, 50)),
    )
    assertEquals(
      ListQuery(page = Some(Page(10, 500))).limited(50).page,
      Some(Page(10, 50)),
    )

  test("composing flattens rather than nesting"):
    val a = rating > 1L
    val b = rating < 9L
    val c = rating.missing
    assertEquals(a && b && c, Filter.And(List(a, b, c)))
    assertEquals(a || (b || c), Filter.Or(List(a, b, c)))
    assertEquals(Filter.always && a, a)
    assertEquals(!(!a), a)

  test("a heading cycles its field through ascending, descending and off"):
    val ascending = Order.toggled(List(Order("name")), "rating")
    assertEquals(
      ascending,
      List(Order("rating"), Order("name")),
    )
    val descending = Order.toggled(ascending, "rating")
    assertEquals(
      descending,
      List(
        Order("rating", descending = true),
        Order("name"),
      ),
    )
    assertEquals(
      Order.toggled(descending, "rating"),
      List(Order("name")),
    )

  test("a secondary key clicked becomes the primary one"):
    assertEquals(
      Order.toggled(
        List(
          Order("name"),
          Order("rating", descending = true),
        ),
        "rating",
      ),
      List(Order("rating"), Order("name")),
    )

  test("keys are written and read back as a sort parameter"):
    val keys = List(
      Order("rating", descending = true),
      Order("name"),
    )
    assertEquals(Order.textOf(keys), "-rating,name")
    assertEquals(Order.parseAll("-rating, name,"), keys)

  test("case is folded alike whatever the default locale"):
    val original = Locale.getDefault
    try
      Locale.setDefault(Locale.forLanguageTag("tr"))
      assertEquals(
        schema
          .run(
            ListQuery(name.contains("iliad")),
            List(Book("ILIAD", None, 1, inPrint = true)),
          )
          .map(_.items.size),
        Right(1),
      )
    finally Locale.setDefault(original)
