package com.alecdorrington.eunomia
package client

import com.alecdorrington.eunomia.model.{
  Field, Filter, ListQuery, Order, Schema,
}
import com.raquo.laminar.api.L.*
import munit.FunSuite

/**
  * Checks the browser-side state of a list: what a person typing into header
  * cells and clicking headings narrows the list to, and what is then shown. Run
  * over a list the browser already holds, so that no request is involved.
  */
class ListViewSuite extends FunSuite:

  private final case class Book(name: String, rating: Option[Long])

  private val books = List(
    Book("Alpha", Some(72L)),
    Book("beta draft", Some(45L)),
    Book("Gamma", None),
  )

  private val name   = Field.of[Book]("name", _.name)
  private val rating = Field.of[Book]("rating", _.rating)
  private val schema = Schema(name, rating)

  private given owner: Owner = new Owner {}

  /** The value a signal holds now. */
  private def now[A](signal: Signal[A]): A = signal.observe.now()

  private def view
    (narrowing: Signal[Filter] = Val(Filter.always))
    : ListView[Book] = ListView(
    schema,
    ListSource.items(Val(books)),
    narrowing,
  )

  private def names(list: ListView[Book]): List[String] =
    now(list.items).map(_.name)

  test("a blank cell narrows nothing"):
    val list = view()
    assertEquals(now(list.filter), Filter.always)
    assertEquals(names(list), books.map(_.name))

  test("text typed into a cell filters by that field"):
    val list = view()
    list.typeInto("name", "a")
    assertEquals(now(list.filter), name.contains("a"))
    assertEquals(
      names(list),
      List("Alpha", "beta draft", "Gamma"),
    )
    list.typeInto("name", "draft")
    assertEquals(names(list), List("beta draft"))

  test("a cell which cannot be read narrows nothing, and says why"):
    val list = view()
    list.typeInto("rating", "abc")
    assertEquals(now(list.filter), Filter.always)
    assertEquals(names(list), books.map(_.name))
    assert(now(list.cellProblem("rating")).isDefined)
    assertEquals(now(list.cellProblem("name")), None)

  test("cells narrow the list together"):
    val list = view()
    list.typeInto("name", "a")
    list.typeInto("rating", ">50")
    assertEquals(names(list), List("Alpha"))

  test("the host's own filter is applied beneath what is typed"):
    val list = view(Val(rating.present))
    assertEquals(
      names(list),
      List("Alpha", "beta draft"),
    )
    list.typeInto("name", "gamma")
    assertEquals(names(list), List.empty)

  test("a heading cycles its field through ascending, descending and off"):
    val list = view()
    list.toggleOrder("rating")
    assertEquals(now(list.order), List(Order("rating")))
    assertEquals(
      names(list),
      List("beta draft", "Alpha", "Gamma"),
    )
    list.toggleOrder("rating")
    assertEquals(
      now(list.order),
      List(Order("rating", descending = true)),
    )
    assertEquals(
      names(list),
      List("Alpha", "beta draft", "Gamma"),
    )
    list.toggleOrder("rating")
    assertEquals(now(list.order), List.empty)
    assertEquals(names(list), books.map(_.name))

  test("the total counts every match, not only those shown"):
    val list = ListView(
      schema,
      ListSource.items(Val(books)),
      initial = ListQuery(page = Some(model.Page(0, 2))),
    )
    assertEquals(
      names(list),
      List("Alpha", "beta draft"),
    )
    assertEquals(now(list.total), 3)

  test("a field the list has no column for is refused"):
    val list = view()
    list.typeInto("author", "x")
    assert(now(list.cellProblem("author")).isDefined)
    assertEquals(now(list.filter), Filter.always)
